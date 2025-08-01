module Solcore.Desugarer.LambdaLifting where

import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.State
import Data.List
import qualified Data.Map as Map
import Solcore.Desugarer.UniqueTypeGen (mkUniqueType, UniqueTyMap)
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcEnv (primCtx)


-- lambda lifting transformation top level function for capture free lambdas

lambdaLifting :: UniqueTyMap ->
                 CompUnit Name ->
                 Either String (CompUnit Name, UniqueTyMap, [String])
lambdaLifting utm cunit
  = case runLiftM (liftLambda cunit) (Map.keys utm) utm of
      Left err -> Left err
      Right (CompUnit imps ds, env) ->
        let decls' = combine (generated env) ds
        in Right (CompUnit imps decls', uniqueTyMap env, debugInfo env)

combine :: [TopDecl Name] -> [TopDecl Name] -> [TopDecl Name]
combine gs ds
  = filter (\ x -> name' x `notElem` ns) gs ++ ds
  where
    ns = map name' ds
    name' (TContr c) = name c
    name' (TClassDef c) = className c
    name' (TInstDef is) = Name $ pretty (instName is) ++ pretty (mainTy is)
    name' (TDataDef d) = dataName d
    name' (TFunDef f) = sigName $ funSignature f
    name' (TSym t) = symName t
    name' (TPragmaDecl d) = Name $ pretty d
    name' (TMutualDef ms) = Name $ concatMap pretty ms

-- lifting lambdas

class LiftLambda a where
  liftLambda :: a -> LiftM a

instance LiftLambda a => LiftLambda [a] where
  liftLambda = mapM liftLambda

instance LiftLambda a => LiftLambda (Maybe a) where
  liftLambda Nothing = pure Nothing
  liftLambda (Just x) = Just <$> liftLambda x

instance LiftLambda (CompUnit Name) where
  liftLambda (CompUnit imps ds)
    = CompUnit imps <$> liftLambda ds

instance LiftLambda (TopDecl Name) where
  liftLambda (TContr c)
    = TContr <$> liftLambda c
  liftLambda (TFunDef fd)
    = TFunDef <$> liftLambda fd
  liftLambda (TInstDef ids)
    = TInstDef <$> liftLambda ids
  liftLambda (TMutualDef tds)
    = TMutualDef <$> liftLambda tds
  liftLambda d = pure d

instance LiftLambda (Contract Name) where
  liftLambda (Contract n vs ds)
    = Contract n vs <$> liftLambda ds

instance LiftLambda (ContractDecl Name) where
  liftLambda (CFunDecl fd)
    = CFunDecl <$> liftLambda fd
  liftLambda (CMutualDecl cs)
    = CMutualDecl <$> liftLambda cs
  liftLambda (CFieldDecl fd)
    = CFieldDecl <$> liftLambda fd
  liftLambda (CConstrDecl cd)
    = CConstrDecl <$> liftLambda cd
  liftLambda d = pure d

instance LiftLambda (Instance Name) where
  liftLambda (Instance d vs ctx n ts t fs)
    = Instance d vs ctx n ts t <$> liftLambda fs

instance LiftLambda (Constructor Name) where
  liftLambda (Constructor ps bd)
    = Constructor ps <$> liftLambda bd

instance LiftLambda (Field Name) where
  liftLambda (Field n t me)
    = Field n t <$> liftLambda me

instance LiftLambda (FunDef Name) where
  liftLambda (FunDef sig bd)
    = FunDef sig <$> liftLambda bd

instance LiftLambda (Stmt Name) where
  liftLambda (e1 := e2)
    = (:=) <$> liftLambda e1 <*> liftLambda e2
  liftLambda (Let n t me)
    = Let n t <$> liftLambda me
  liftLambda (StmtExp e)
    = StmtExp <$> liftLambda e
  liftLambda (Return e)
    = Return <$> liftLambda e
  liftLambda (Match es eqns)
    = Match <$> liftLambda es <*> liftLambda eqns
  liftLambda d = pure d

instance LiftLambda (Equation Name) where
  liftLambda (ps , bd)
    = (ps,) <$> liftLambda bd

instance LiftLambda (Exp Name) where
  liftLambda (Con n es)
    = Con n <$> liftLambda es
  liftLambda (FieldAccess e n)
    = flip FieldAccess n <$> liftLambda e
  liftLambda (Call me n es)
    = do
        me' <- liftLambda me
        es' <- liftLambda es
        pure (Call me' n es')
  liftLambda (Lam ps bd _)
    = do
        fs <- gets functionNames
        let
          defs = fs ++ vars ps ++ Map.keys primCtx
          free = vars bd \\ defs
        if null free then do
          fd <- createFunction ps bd
          let nm = sigName (funSignature fd)
          tf <- freshName ("t_" ++ pretty nm)
          let dt = mkUniqueType tf
          addUniqueTyMap nm dt
          addDecl (TDataDef dt)
          addDecl (TFunDef fd)
          pure $ Con (dataName dt) []
        else do
          arg <- Untyped <$> freshName "env"
          (dt, dn) <- createClosureType free
          let bd' = mkMatchBody dn free arg bd
          fd <- createFunction (arg : ps) [bd']
          let nm = sigName (funSignature fd)
          udt <- createUniqueTypeClosure nm dt
          addDecl (TDataDef dt)
          addDecl (TDataDef udt)
          addUniqueTyMap nm udt
          addDecl (TFunDef fd)
          pure $ Con (dataName udt)
                     [Con dn (Var <$> free)]
  liftLambda d = pure d

createClosureType :: [Name] -> LiftM (DataTy, Name)
createClosureType ns
  = do
      n <- freshName "t_closure"
      let vs = TVar <$> ns
      pure (DataTy n vs [Constr n (TyVar <$> vs)], n)

createUniqueTypeClosure :: Name -> DataTy -> LiftM DataTy
createUniqueTypeClosure n (DataTy _ vs ((Constr c ts) : _))
  = do
      tn <- freshName ("t_" ++ pretty n)
      let
          c' = Constr tn [TyCon c ts]
      pure (DataTy tn vs [c'])
createUniqueTypeClosure _ _ = error "Panic! LambdaLifting.createUniqueTypeClosure!"

mkMatchBody :: Name -> [Name] -> Param Name -> Body Name -> Stmt Name
mkMatchBody dn ns (Untyped n) bd
  = Match [Var n] [([PCon dn (PVar <$> ns)], bd)]
mkMatchBody _ _ _ _ = error "Panic! LambdaLifting.mkMatchBody!"

createFunction :: [Param Name] ->
                  Body Name ->
                  LiftM (FunDef Name)
createFunction ps bdy
  = (flip FunDef bdy) <$> createSignature ps

createSignature :: [Param Name] ->
                   LiftM (Signature Name)
createSignature ps
  = do
      n <- freshName "lambda_impl"
      pure (Signature [] [] n ps Nothing)

-- monad definition

data Env
  = Env {
      generated :: [TopDecl Name]
    , functionNames :: [Name]
    , uniqueTyMap :: UniqueTyMap
    , fresh :: Int
    , debugInfo :: [String]
    }

type LiftM a = StateT Env (ExceptT String Identity) a

runLiftM :: LiftM a -> [Name] -> UniqueTyMap -> Either String (a, Env)
runLiftM m ns utm = runIdentity (runExceptT (runStateT m initEnv))
    where
      initEnv = Env [] ns' utm 0 []
      ns' = map Name ["primEqWord", "primAddWord", "invoke"] ++ ns

inc :: LiftM Int
inc = do
  n <- gets fresh
  modify (\ env -> env {fresh = n + 1})
  pure n

freshName :: String -> LiftM Name
freshName s
  = do
      n <- inc
      pure $ Name (s ++ show n)

addDecl :: TopDecl Name -> LiftM ()
addDecl d
  = modify (\ env -> env{ generated = d : generated env })

addUniqueTyMap :: Name -> DataTy -> LiftM ()
addUniqueTyMap n t
  = modify (\env -> env{ uniqueTyMap = Map.insert n t (uniqueTyMap env)})

addDebugInfo :: String -> LiftM ()
addDebugInfo s
  = modify (\env -> env{ debugInfo = s : debugInfo env })

-- collecting function names, for determining indirect calls

class Collect a where
  collect :: a -> [Name]

instance Collect a => Collect [a] where
  collect = foldr (union . collect) []

instance Collect (CompUnit Name) where
  collect (CompUnit _ ds) = collect ds

instance Collect (TopDecl Name) where
  collect (TContr c) = collect c
  collect (TFunDef fd) = collect fd
  collect (TClassDef c) = collect c
  collect (TInstDef ins) = collect ins
  collect (TMutualDef ds) = collect ds
  collect _ = []

instance Collect (Contract Name) where
  collect (Contract _ _ ds)
    = collect ds

instance Collect (ContractDecl Name) where
  collect (CFunDecl fd) = collect fd
  collect (CMutualDecl ds) = collect ds
  collect _ = []

instance Collect (Class Name) where
  collect = collect . signatures

instance Collect (Instance Name) where
  collect = collect . instFunctions

instance Collect (Signature Name) where
  collect sig = [sigName sig]

instance Collect (FunDef Name) where
  collect fd = collect (funSignature fd)

-- determining free variables

class Vars a where
  vars :: a -> [Name]

instance Vars a => Vars [a] where
  vars = foldr (union . vars) []

instance Vars (Pat Name) where
  vars (PVar v) = [v]
  vars (PCon _ ps) = vars ps
  vars _ = []

instance Vars (Param Name) where
  vars (Typed n _) = [n]
  vars (Untyped n) = [n]

instance Vars (Stmt Name) where
  vars (e1 := e2) = vars [e1,e2]
  vars (Let _ _ (Just e)) = vars e
  vars (Let _ _ _) = []
  vars (StmtExp e) = vars e
  vars (Return e) = vars e
  vars (Match e eqns) = vars e `union` vars eqns
  vars _ = []

instance Vars (Equation Name) where
  vars (_, ss) = vars ss

instance Vars (Exp Name) where
  vars (Var n) = [n]
  vars (Con _ es) = vars es
  vars (FieldAccess Nothing _) = []
  vars (FieldAccess (Just e) _) = vars e
  vars (Call (Just e) n es) = [n] `union` vars (e : es)
  vars (Call Nothing n es) = [n] `union` vars es
  vars (Lam ps bd _) = vars bd \\ vars ps
  vars _ = []
