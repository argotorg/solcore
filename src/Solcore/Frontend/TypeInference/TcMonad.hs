module Solcore.Frontend.TypeInference.TcMonad where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Writer

import Data.List
import qualified Data.List.NonEmpty as N
import Data.Map (Map)
import Data.Maybe
import qualified Data.Map as Map

import Solcore.Frontend.Pretty.SolcorePretty hiding((<>))
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Pipeline.Options(Option(..))
import Solcore.Primitives.Primitives


-- definition of type inference monad infrastructure 

type TcM a = (StateT TcEnv (ExceptT String IO)) a 

runTcM :: TcM a -> TcEnv -> IO (Either String (a, TcEnv))
runTcM m env = runExceptT (runStateT m env)

freshVar :: TcM Tyvar 
freshVar 
  = TVar <$> freshName 

freshName :: TcM Name 
freshName 
  = do
      vs <- getEnvFreeVars
      ns <- gets nameSupply 
      let (n, ns') = newName ns 
      modify (\ ctx -> ctx {nameSupply = ns'})
      return n 

incCounter :: TcM Int 
incCounter = do 
  c <- gets counter 
  modify (\ ctx -> ctx{counter = c + 1})
  pure c 

askGeneratingDefs :: TcM Bool 
askGeneratingDefs = gets generateDefs 

setGeneratingDefs :: Bool -> TcM ()
setGeneratingDefs b 
  = modify (\env -> env {generateDefs = b})

withNoGeneratingDefs :: TcM a -> TcM a 
withNoGeneratingDefs m 
  = do 
      b <- askGeneratingDefs
      setGeneratingDefs False 
      r <- m 
      setGeneratingDefs b 
      pure r

addUniqueType :: Name -> DataTy -> TcM ()
addUniqueType n dt 
  = do
      modify (\ ctx -> ctx{ uniqueTypes = Map.insert n dt (uniqueTypes ctx)})
      modifyTypeInfo (dataName dt) (typeInfoFor dt)

lookupUniqueTy :: Name -> TcM (Maybe DataTy)
lookupUniqueTy n 
  = (Map.lookup n) <$> gets uniqueTypes 

isUniqueTyName :: Name -> TcM Bool 
isUniqueTyName n = do 
  uenv <- gets uniqueTypes
  pure $ any (\ d -> dataName d == n) (Map.elems uenv)

typeInfoFor :: DataTy -> TypeInfo 
typeInfoFor (DataTy n vs cons)
  = TypeInfo (length vs) (map constrName cons) []

freshTyVar :: TcM Ty 
freshTyVar = TyVar <$> freshVar

writeFunDef :: FunDef Id -> TcM ()
writeFunDef fd = writeTopDecl (TFunDef fd)

writeDataTy :: DataTy -> TcM ()
writeDataTy dt 
  = writeTopDecl (TDataDef dt)  

writeInstance :: Instance Id -> TcM () 
writeInstance instd 
  = writeTopDecl (TInstDef instd)

writeTopDecl :: TopDecl Id -> TcM ()
writeTopDecl d 
  = do 
      b <- gets generateDefs 
      when b $ do 
        ts <- gets generated 
        modify (\env -> env{ generated = d : ts})

getEnvFreeVars :: TcM [Tyvar]
getEnvFreeVars 
  = concat <$> gets (Map.map fv . ctx)

unify :: Ty -> Ty -> TcM Subst
unify t t' 
  = do
      s <- getSubst
      s' <- tcmMgu (apply s t) (apply s t')
      s1 <- extSubst s'
      pure s1

matchTy :: Ty -> Ty -> TcM Subst 
matchTy t t' 
  = do 
      s <- match t t' 
      extSubst s 

addFunctionName :: Name -> TcM () 
addFunctionName n 
  = modify (\ env -> env {directCalls = n : directCalls env })

isDirectCall :: Name -> TcM Bool
isDirectCall (QualName n _) = pure True
isDirectCall n 
  = (elem n) <$> gets directCalls 

-- including contructors on environment

checkDataType :: DataTy -> TcM ()
checkDataType (DataTy n vs constrs) 
  = do
      let vals' = map (\ (n, ty) -> (n, Forall (fv ty) ([] :=> ty))) vals
      mapM_ (uncurry extEnv) vals'
      modifyTypeInfo n ti
    where 
      ti = TypeInfo (length vs) (map fst vals) []
      tc = TyCon n (TyVar <$> vs) 
      vals = map constrBind constrs        
      constrBind c = (constrName c, (funtype (constrTy c) tc))


-- type instantiation 

freshInst :: Scheme -> TcM (Qual Ty)
freshInst (Forall vs qt)
  = renameVars vs qt

renameVars :: HasType a => [Tyvar] -> a -> TcM a 
renameVars vs t 
  = do
      s <- mapM (\ v -> (v,) <$> freshTyVar) vs
      pure $ apply (Subst s) t

-- substitution 

withCurrentSubst :: HasType a => a -> TcM a
withCurrentSubst t = do
  s <- gets subst
  pure (apply s t)

getSubst :: TcM Subst 
getSubst = gets subst

extSubst :: Subst -> TcM Subst
extSubst s = modify ext >> getSubst where
    ext st = st{ subst = s <> subst st }

withLocalSubst :: HasType a => TcM a -> TcM a 
withLocalSubst m 
  = do 
      s <- getSubst 
      r <- m 
      modify (\ st -> st {subst = s})
      pure (apply s r)

clearSubst :: TcM ()
clearSubst = modify (\ st -> st {subst = mempty})

-- current contract manipulation 

withContractName :: Name -> TcM a -> TcM a 
withContractName n m 
  = do 
      setCurrentContract n 
      a <- m 
      clearCurrentContract
      pure a

setCurrentContract :: Name -> TcM ()
setCurrentContract n 
  = modify (\ ctx -> ctx{ contract = Just n })

clearCurrentContract :: TcM ()
clearCurrentContract 
  = modify (\ ctx -> ctx {contract = Nothing})

askCurrentContract :: TcM Name 
askCurrentContract 
  = do 
      n <- gets contract
      maybe (throwError "Impossible! Lacking current contract name!")
            pure  
            n 

-- manipulating contract field information

askField :: Name -> Name -> TcM Scheme 
askField cn fn 
  = do 
      ti <- askTypeInfo cn 
      when (fn `notElem` fieldNames ti) 
           (undefinedField cn fn)
      askEnv fn

-- manipulating data constructor information 

checkConstr :: Name -> Name -> TcM ()
checkConstr tn cn 
  = do 
      ti <- askTypeInfo tn 
      unless (validConstr cn ti)
             (undefinedConstr tn cn)

validConstr :: Name -> TypeInfo -> Bool 
validConstr n ti = n `elem` constrNames ti || isPair n 
  where 
    isPair (Name n) = n == "pair"
    isPair _ = False

-- extending the environment with a new variable 

extEnv :: Name -> Scheme -> TcM ()
extEnv n t 
  = modify (\ sig -> sig {ctx = Map.insert n t (ctx sig)})

withExtEnv :: Name -> Scheme -> TcM a -> TcM a 
withExtEnv n s m 
  = withLocalEnv (extEnv n s >> m)

withLocalCtx :: [(Name, Scheme)] -> TcM a -> TcM a 
withLocalCtx ctx m 
  = withLocalEnv do 
        mapM_ (\ (n,s) -> extEnv n s) ctx 
        a <- m
        pure a

-- Updating the environment 

putEnv :: Env -> TcM ()
putEnv ctx 
  = modify (\ sig -> sig{ctx = ctx})

-- Extending the environment 

withLocalEnv :: TcM a -> TcM a 
withLocalEnv ta 
  = do 
      ctx <- gets ctx 
      a <- ta
      putEnv ctx 
      pure a 

envList :: TcM [(Name, Scheme)]
envList = gets (Map.toList . ctx)

-- asking class info

askClassInfo :: Name -> TcM ClassInfo 
askClassInfo n 
  = do 
      r <- Map.lookup n <$> gets classTable
      maybe (undefinedClass n) pure r 

-- environment operations: variables 

maybeAskEnv :: Name -> TcM (Maybe Scheme)
maybeAskEnv n = gets (Map.lookup n . ctx)

askEnv :: Name -> TcM Scheme 
askEnv n 
  = do
      s <- maybeAskEnv n
      maybe (undefinedName n) pure s

-- type information

maybeAskTypeInfo :: Name -> TcM (Maybe TypeInfo)
maybeAskTypeInfo n 
  = gets (Map.lookup n . typeTable)

askTypeInfo :: Name -> TcM TypeInfo 
askTypeInfo n 
  = do
      ti <- maybeAskTypeInfo n
      maybe (undefinedType n) pure ti

modifyTypeInfo :: Name -> TypeInfo -> TcM ()
modifyTypeInfo n ti 
  = do 
        tenv <- gets typeTable
        let tenv' = Map.insert n ti tenv
        modify (\env -> env{typeTable = tenv'})


-- manipulating the instance environment 

getClassEnv :: TcM ClassTable 
getClassEnv 
  = gets classTable

askInstEnv :: Name -> TcM [Inst]
askInstEnv n 
  = maybe [] id . Map.lookup n <$> gets instEnv

getInstEnv :: TcM InstTable 
getInstEnv 
  = gets instEnv >>= renameInstEnv

renameInstEnv :: InstTable -> TcM InstTable 
renameInstEnv 
  = Map.traverseWithKey go
  where 
    go k v = do 
      let vs = fv v 
      v' <- renameVars vs v 
      pure v' 

addInstance :: Name -> Inst -> TcM ()
addInstance n inst 
  = do 
      modify (\ ctx -> 
        ctx{instEnv = Map.insertWith (++) n [inst] (instEnv ctx)})  
      

maybeToTcM :: String -> Maybe a -> TcM a 
maybeToTcM s Nothing = throwError s 
maybeToTcM _ (Just x) = pure x

-- checking coverage pragma 

pragmaEnabled :: Name -> PragmaStatus -> Bool 
pragmaEnabled n Enabled = False 
pragmaEnabled _ DisableAll = True 
pragmaEnabled n (DisableFor ns) = n `elem` N.toList ns

askCoverage :: Name -> TcM Bool 
askCoverage n 
  = (pragmaEnabled n) <$> gets coverage 

setCoverage :: PragmaStatus -> TcM ()
setCoverage st
  = modify (\ env -> env{coverage = st })

-- checking Patterson condition pragma 

askPattersonCondition :: Name -> TcM Bool 
askPattersonCondition n
  = (pragmaEnabled n) <$> gets patterson

setPattersonCondition :: PragmaStatus -> TcM ()
setPattersonCondition st
  = modify (\env -> env {patterson = st})

-- checking bound variable condition 

askBoundVariableCondition :: Name -> TcM Bool 
askBoundVariableCondition n 
  = (pragmaEnabled n) <$> gets boundVariable 

setBoundVariableCondition :: PragmaStatus -> TcM ()
setBoundVariableCondition st 
  = modify (\ env -> env {boundVariable = st})

disableBoundVariableCondition :: TcM a -> TcM a 
disableBoundVariableCondition m 
  = do 
      old <- gets boundVariable
      setBoundVariableCondition DisableAll
      x <- m 
      setBoundVariableCondition old 
      pure x 

-- recursion depth 

askMaxRecursionDepth :: TcM Int 
askMaxRecursionDepth = gets maxRecursionDepth 

-- other options

getTcOpts :: TcM Option
getTcOpts = gets tcOptions

getNoDesugarCalls :: TcM Bool
getNoDesugarCalls = gets (optNoDesugarCalls. tcOptions)

-- logging utilities

setLogging :: Bool -> TcM ()
setLogging b = modify (\ r -> r{enableLog = b})

isLogging :: TcM Bool 
isLogging = gets enableLog

isVerbose :: TcM Bool
isVerbose = gets (optVerbose . tcOptions)

info :: [String] -> TcM ()
info ss = do
            let msg = concat ss
            logging <- isLogging
            verbose <- isVerbose
            when logging $ modify (\ r -> r{ logs = msg : logs r })

warning :: String -> TcM ()
warning s = do 
  modify (\ r -> r{ warnings = s : "Warning:" : warnings r })

dumpLogs :: TcM ()
dumpLogs = do
  records <- gets logs
  liftIO $ putStrLn "\nLogs:"
  liftIO $ putStrLn $ unlines $ reverse records
  liftIO $ putStrLn "------------------------------------------------------------------"

-- wrapping error messages 

wrapError :: Pretty b => TcM a -> b -> TcM a
wrapError m e 
  = catchError m handler 
    where 
      handler msg = throwError (decorate msg)
      decorate msg = msg ++ "\n - in:" ++ pretty e

tcmMgu :: Ty -> Ty -> TcM Subst
tcmMgu t u = mgu t u `catchError` tcmError

-- error messages 

tcmError :: String -> TcM a
tcmError s = do
  verbose <- isVerbose
  when verbose dumpLogs
  throwError s

undefinedName :: Name -> TcM a 
undefinedName n 
  = throwError $ unwords ["Undefined name:", pretty n]

undefinedType :: Name -> TcM a 
undefinedType n 
  = do
      s <- (unlines . reverse) <$> gets logs 
      throwError $ unwords ["Undefined type:", pretty n, "\n", s]

undefinedField :: Name -> Name -> TcM a 
undefinedField n n'
  = throwError $ unlines ["Undefined field:"
                         , pretty n
                         , "in type:"
                         , pretty n'
                         ]

undefinedConstr :: Name -> Name -> TcM a 
undefinedConstr tn cn 
  = throwError $ unlines [ "Undefined constructor:"
                         , pretty cn 
                         , "in type:"
                         , pretty tn]

undefinedFunction :: Name -> Name -> TcM a 
undefinedFunction t n 
  = throwError $ unlines [ "The type:"
                         , pretty t 
                         , "does not define function:"
                         , pretty n
                         ]

undefinedClass :: Name -> TcM a 
undefinedClass n 
  = throwError $ unlines ["Undefined class:", pretty n]
