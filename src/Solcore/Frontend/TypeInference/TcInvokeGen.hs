module Solcore.Frontend.TypeInference.TcInvokeGen where 

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.State

import Data.Generics hiding (Constr)
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax hiding (epair)
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives

-- generate invoke instances for functions

generateDecls :: (FunDef Id, Scheme) -> TcM () 
generateDecls (fd@(FunDef sig bd), sch) 
  = do
      createTypeFunDef fd 
      dt <- lookupUniqueType (sigName sig)
      createInstance dt sig sch 
      pure ()

lookupUniqueType :: Name -> TcM (Maybe (TopDecl Name))
lookupUniqueType n = (fmap TDataDef . Map.lookup n) <$> gets uniqueTypes 

createInstance :: Maybe (TopDecl Name) -> Signature Id -> Scheme -> TcM () 
createInstance Nothing  _ _ = pure ()
createInstance (Just (TDataDef dt@(DataTy n vs _))) sig sch 
  = do
      (ps :=> t) <- freshInst sch
      (argTys', retTy) <- createArgs t   
      let mainTy = TyCon n ([argTys', retTy])
      bd <- createInvokeDef dt sig t 
      let instd = Instance [] invokableName ([argTys', retTy]) mainTy [ bd ]
      addInstance invokableName (mkInstPred instd)
      writeInstance instd
      pure ()

notUniqueTyArg :: Ty -> TcM Bool 
notUniqueTyArg (TyVar _) = pure True 
notUniqueTyArg (TyCon n _)
  = do 
      uniques <- Map.elems <$> gets uniqueTypes
      pure (all (\ d -> dataName d /= n) uniques)

mkInstPred :: Instance Id -> Inst
mkInstPred (Instance ctx n ts t _) 
  = anfInstance $ ctx :=> InCls n t ts

anfInstance :: Inst -> Inst
anfInstance inst@(q :=> p@(InCls c t [])) = inst
anfInstance inst@(q :=> p@(InCls c t as)) = q ++ q' :=> InCls c t bs 
  where
    q' = zipWith (:~:) bs as
    bs = map TyVar $ take (length as) freshNames
    tvs = fv inst
    freshNames = filter (not . flip elem tvs) (flip TVar False <$> namePool)

createInvokeDef :: DataTy -> Signature Id-> Ty -> TcM (FunDef Id)
createInvokeDef dt sig t 
  = do
      (sig', mi) <- createInvokeSig dt sig t 
      bd <- createInvokeBody dt sig mi t 
      pure (FunDef sig' bd)

createInvokeSig :: DataTy -> Signature Id -> Ty -> TcM (Signature Id, Id)
createInvokeSig (DataTy n vs cons) sig t 
  = do
      (argTys', retTy) <- createArgs t
      (args, mp) <- mkParamForSig argTys' (TyCon n ([argTys', retTy]))
      let  
          vs = fv [retTy, argTys']
          sig' = Signature vs [] invokeName args (Just retTy)
      pure (sig', mp)

createArgs :: Ty -> TcM (Ty, Ty)
createArgs t  
  = do
      let (ts',t') = splitTy t 
      argTys' <- filterM notUniqueTyArg ts'
      pure $ (tupleTyFromList argTys', t')

tupleTyFromList :: [Ty] -> Ty
tupleTyFromList [] = unit 
tupleTyFromList [t] = t 
tupleTyFromList [t1,t2] = pair t1 t2 
tupleTyFromList (t1 : ts) = pair t1 (tupleTyFromList ts)

mkParamForSig :: Ty -> Ty -> TcM ([Param Id], Id)
mkParamForSig argTy selfTy 
  = do 
      let selfArg = Typed (Id selfName selfTy) selfTy 
      paramName <- freshName 
      let pid = Id paramName argTy
      pure ([selfArg, Typed pid argTy], pid)

-- creating invoke body 

-- no pattern matching needed: just make a call to the function 
-- pattern matching needed:
-- * No closure: function receives more than one argument, which are passed as a tuple 
-- * Closure: pattern match on closure to get the parameters and make the call with arguments.

createInvokeBody :: DataTy -> Signature Id -> Id -> Ty -> TcM (Body Id)
createInvokeBody (DataTy dt vs [Constr c1 targs]) sig x@(Id pid ty) t 
  = do
      let patTys = tyConArgs ty
          cname = Id (sigName sig) t 
          (argTys, retTy) = splitTy t 
      (pats, ns) <- unzip <$> mapM mkPat patTys
      if [ty] == argTys && null targs then 
        -- no need to match the closure type and arguments tuple 
        pure [Return $ Call Nothing cname [Var x]]
      else if null targs then do
        -- no pattern matching on closure needed
        let args = if null argTys then [] else [Var x]
        if null patTys then 
          -- no pattern matching on arguments tuple
          pure [Return $ Call Nothing cname args]
        else do 
          -- pattern matching on arguments tuple 
          let pats' = foldr1 ppair pats 
              ret = Return $ Call Nothing cname (concat ns)
          pure [Match [Var x] [([pats'], [ret])]]
      else do 
        cps <- mapM (\ t -> (PVar . flip Id t) <$> freshName) targs 
        -- matching on closure needed
        let n1 = Id (Name "self") tc 
            cp = Typed n1 tc
            cpat = PCon (Id c1 (funtype targs tc)) cps
            pats1 = if null pats then [PVar x] else pats 
            ns1 = if null ns then [Var n1, Var x] else Var n1 : (Var x) : (concat ns) 
            pats' = foldr1 ppair (cpat : pats1) 
            ret = Return $ Call Nothing cname ns1
            tc = TyCon dt (TyVar <$> vs)
        pure [Match [epair (Var n1) (Var x)] [([pats'], [ret])]]


isQual :: Name -> Bool
isQual (QualName _ _) = True 
isQual _ = False

tyConArgs :: Ty -> [Ty]
tyConArgs (TyVar _) = []
tyConArgs (TyCon _ tys) = tys

ppair :: Pat Id -> Pat Id -> Pat Id  
ppair p1 p2 
  = PCon (pairCon t1 t2) [p1, p2] 
    where 
      t1 = tyFromPat p1 
      t2 = tyFromPat p2

-- invariant: We should remove wildcards before.

tyFromPat :: Pat Id -> Ty
tyFromPat (PVar (Id _ t)) = t 
tyFromPat (PCon (Id _ t) _) = t 
tyFromPat (PLit l) = tyFromLit l 

tyFromLit :: Literal -> Ty 
tyFromLit (IntLit _) = word 
tyFromLit (StrLit _) = string 

pairCon :: Ty -> Ty -> Id 
pairCon t1 t2 = Id (Name "pair") (pairTy t1 t2)

epair :: Exp Id -> Exp Id -> Exp Id 
epair e1 e2 = Con (pairCon t1 t2) [e1, e2]
  where 
    t1 = tyFromExp e1 
    t2 = tyFromExp e2

tyFromExp :: Exp Id -> Ty 
tyFromExp (Var (Id _ t)) = t 
tyFromExp (Con (Id _ t) _) = t 
tyFromExp (FieldAccess _ (Id _ t)) = t 
tyFromExp (Lit l) = tyFromLit l 
tyFromExp (Call _ (Id _ t) _) = t
-- here we assume that we have made lambda lift 
-- and infer types for the function of the lambda.
-- We do not have lambdas here.
tyFromExp (Lam _ _ (Just t)) = t
tyFromExp (TyExp _ t) = t

mkPat :: Ty -> TcM (Pat Id, [Exp Id])
mkPat (TyCon (Name "pair") [t1, t2])
  = do 
      n1 <- freshName 
      p <- freshName
      (p1,e1) <- mkPat t2 
      pure (ppair (PVar (Id n1 t1)) p1, (Var (Id n1 t1)) : e1)
mkPat t = do 
  n <- freshName 
  let v = Id n t
  pure (PVar v,  [Var v])


mkArgTypes :: [Ty] -> Ty -> [Ty] 
mkArgTypes args ret 
  = case mkArgTy args of 
      Just arg' -> [arg', ret] 
      Nothing -> [ret] 

mkArgTy :: [Ty] -> Maybe Ty 
mkArgTy [] = Nothing 
mkArgTy [arg] = Just arg 
mkArgTy (arg : args) 
  = case mkArgTy args of 
      Just arg' -> Just (pair arg arg')
      Nothing -> Just arg 

tyParam :: Param a -> TcM Ty 
tyParam (Typed _ t) = pure t 
tyParam (Untyped _) = freshTyVar

-- creating unique types 

createTypeFunDef :: FunDef Id -> TcM (FunDef Id)
createTypeFunDef (FunDef sig bdy)
  = do 
      dt <- lookupUniqueType (sigName sig)
      case dt of 
        Nothing -> do 
          sig' <- freshSignature sig  
          createUniqueType' (sigName sig) (schemeFromSig sig')
          pure (FunDef sig bdy)
        Just _ -> pure (FunDef sig bdy)

freshSignature :: Signature Id -> TcM (Signature Id)
freshSignature sig 
  = do 
      let ctx = sigContext sig 
          args = sigParams sig 
          ret = sigReturn sig
          fvArg (Typed _ ty) = fv ty 
          fvArg _ = []
          fv' = foldr (union . fvArg) []
          vs = vars $ fv ctx `union` fv' args `union` fv ret
      args' <- mapM freshParam args 
      ret' <- freshReturn ret 
      pure sig {
             sigParams = args' 
           , sigReturn = ret'
           } 

freshParam :: Param Id -> TcM (Param Id)
freshParam p@(Typed _ _) = pure p 
freshParam (Untyped n@(Id _ t)) 
  = pure (Typed n t)

freshReturn :: Maybe Ty -> TcM (Maybe Ty)
freshReturn Nothing 
  = do 
      v <- freshTyVar
      pure (Just v)
freshReturn p = pure p 

vars :: [Tyvar] -> [Name]
vars = map (\ (TVar n _) -> n) 

fv' :: [Param Id] -> [Tyvar]
fv' = foldr step []
  where 
    step (Typed _ ty) ac = fv ty `union` ac 
    step (Untyped _) ac = ac 

schemeFromSig :: Signature Id -> Scheme 
schemeFromSig sig 
  = let 
      ctx = sigContext sig 
      argTys = map tyFromParam (sigParams sig)
      retTy = fromJust $ sigReturn sig 
      ty = funtype argTys retTy 
      vs = fv (ctx :=> ty)
    in Forall vs (ctx :=> ty)

tyFromParam :: Param Id -> Ty 
tyFromParam (Typed _ ty) = ty 

createUniqueType' :: Name -> Scheme -> TcM ()
createUniqueType' n (Forall vs _)
     = do
        dt <- uniqueType n vs 
        writeDataTy dt
        checkDataType dt 

uniqueType :: Name -> [Tyvar] -> TcM DataTy 
uniqueType n vs
  = do 
      m <- incCounter
      let 
          argVar = TVar (Name "args") False 
          retVar = TVar (Name "ret") False 
          nt = Name $ "t_" ++ pretty n ++ show m
          dc = Constr nt []
          dt = DataTy nt [argVar, retVar] [dc]
      modify (\ env -> env {uniqueTypes = Map.insert n dt (uniqueTypes env)})
      pure dt


