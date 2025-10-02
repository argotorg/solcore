module Solcore.Frontend.TypeInference.TcContract where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.State

import Data.Generics hiding (Constr)
import Data.List
import qualified Data.List.NonEmpty as N
import qualified Data.Map as Map
import Data.Maybe

import Solcore.Desugarer.UniqueTypeGen (UniqueTyMap)
import Solcore.Frontend.Pretty.ShortName
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.InvokeGen
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcStmt
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Pipeline.Options
import Solcore.Primitives.Primitives

typeInfer :: Option ->
             CompUnit Name ->
             IO (Either String (CompUnit Id, TcEnv))
typeInfer options (CompUnit imps decls)
  = do
      r <- runTcM (tcCompUnit (CompUnit imps decls)) (initTcEnv options)
      case r of
        Left err -> pure $ Left err
        Right ((CompUnit imps ds), env) -> do
          let ds1 = (ds ++ generated env)
          pure (Right (CompUnit imps ds1, env))

-- type inference for a compilation unit

tcCompUnit :: CompUnit Name -> TcM (CompUnit Id)
tcCompUnit (CompUnit imps cs)
  = do
      setupPragmas ps
      mapM_ checkTopDecl cls
      mapM_ checkTopDecl cs'
      cs' <- mapM tcTopDecl' cs
      pure (CompUnit imps cs')
    where
      ps = foldr step [] cs
      step (TPragmaDecl p) ac = p : ac
      step _ ac = ac
      (cls, cs') = partition isClass cs
      isClass (TClassDef _) = True
      isClass _ = False
      tcTopDecl' d = timeItNamed (shortName d) $ do
        clearSubst
        tcTopDecl d

-- setting up pragmas for type checking

setupPragmas :: [Pragma] -> TcM ()
setupPragmas ps = do
  setBoundVariableCondition (getStatus NoBoundVariableCondition)
  setPattersonCondition (getStatus NoPattersonCondition)
  setCoverage (getStatus NoCoverageCondition)
  where
    getStatus :: PragmaType -> PragmaStatus
    getStatus ptype =
      case [s | Pragma t s <- ps, t == ptype] of
        [] -> Enabled
        statuses -> mergeStatuses statuses

    mergeStatuses :: [PragmaStatus] -> PragmaStatus
    mergeStatuses ss
      | DisableAll `elem` ss = DisableAll
      | otherwise =
          case [n | DisableFor ns <- ss, n <- N.toList ns] of
            [] -> Enabled
            (x:xs) -> DisableFor (x N.:| xs)

tcTopDecl :: TopDecl Name -> TcM (TopDecl Id)
tcTopDecl (TContr c)
  = do
      (c', assumps) <- tcContract c
      mapM_ (uncurry extEnv) assumps
      pure (TContr c')
tcTopDecl (TFunDef fd)
  = do
      fd' <- tcBindGroup [fd]
      case fd' of
        (fd1 : _) -> pure (TFunDef fd1)
        _ -> throwError "Impossible! Empty binding group!"
tcTopDecl (TClassDef c)
  = TClassDef <$> tcClass c
tcTopDecl (TInstDef is)
  = TInstDef <$> tcInstance is
tcTopDecl (TMutualDef ts)
  = do
      let f (TFunDef fd) = fd
      ts' <- tcBindGroup (map f ts)
      pure (TMutualDef $ map TFunDef ts')
tcTopDecl (TDataDef d)
  = do
    pure (TDataDef d)
tcTopDecl (TPragmaDecl d)
  = pure (TPragmaDecl d)

checkTopDecl :: TopDecl Name -> TcM ()
checkTopDecl (TClassDef c)
  = checkClass c
checkTopDecl (TInstDef is)
  = checkInstance is
checkTopDecl (TDataDef dt)
  = checkDataType dt
checkTopDecl (TFunDef (FunDef sig _))
  = extSignature sig
checkTopDecl _ = pure ()

-- type inference for contracts

tcContract :: Contract Name -> TcM (Contract Id, [(Name, Scheme)])
tcContract c@(Contract n vs decls)
  = withLocalEnv $ withContractName n $ do
      ctx' <- gets ctx
      initializeEnv c
      decls' <- mapM tcDecl' decls
      ctx1 <- gets ctx
      let
        ctx2 = Map.toList $ Map.difference ctx1 ctx'
      pure (Contract n vs decls', ctx2)
    where
      tcDecl' d
        = do
          clearSubst
          d' <- tcDecl d
          s <- getSubst
          pure (everywhere (mkT (apply @Id s)) d')

-- initializing context for a contract

initializeEnv :: Contract Name -> TcM ()
initializeEnv (Contract n vs decls)
  = mapM_ checkDecl decls

checkDecl :: ContractDecl Name -> TcM ()
checkDecl (CDataDecl dt)
  = checkDataType dt
checkDecl (CFunDecl (FunDef sig _))
  = extSignature sig
checkDecl (CFieldDecl fd)
  = tcField fd >> return ()
checkDecl (CMutualDecl ds)
  = mapM_ checkDecl ds
checkDecl _ = return ()

-- type inference for declarations

tcDecl :: ContractDecl Name -> TcM (ContractDecl Id)
tcDecl (CFieldDecl fd) = CFieldDecl <$> tcField fd
tcDecl (CFunDecl d)
  = do
      d' <- tcBindGroup [d]
      case d' of
        [] -> throwError "Impossible! Empty function binding!"
        (x : _) -> pure (CFunDecl x)
tcDecl (CMutualDecl ds)
  = do
      let f (CFunDecl fd) = fd
      ds' <- tcBindGroup (map f ds)
      pure (CMutualDecl (map CFunDecl ds'))
tcDecl (CConstrDecl cd) = CConstrDecl <$> tcConstructor cd
tcDecl (CDataDecl d) = CDataDecl <$> tcDataDecl d

-- kind check data declarations

tcDataDecl :: DataTy -> TcM DataTy
tcDataDecl (DataTy n vs cs)
  = DataTy n vs <$> mapM tcConstr cs

tcConstr :: Constr -> TcM Constr
tcConstr (Constr n ts)
  = Constr n <$> mapM kindCheck ts

-- type checking fields

tcField :: Field Name -> TcM (Field Id)
tcField d@(Field n t (Just e))
  = do
      (e', ps', t') <- tcExp e
      kindCheck t `wrapError` d
      s <- mgu t t' `wrapError` d
      extEnv n (monotype t)
      return (Field n t (Just e'))
tcField d@(Field n t _)
  = do
      kindCheck t `wrapError` d
      extEnv n (monotype t)
      pure (Field n t Nothing)

tcClass :: Class Name -> TcM (Class Id)
tcClass iclass@(Class bvs ctx n vs v sigs)
  = do
      let bvs' = bv ctx `union` bv (map TyVar (v : vs)) `union` bv sigs
          ns = map sigName sigs
          qs = map (QualName n . pretty) ns
      when (any (`notElem` bvs) bvs') $ do
         let unbound_vars = bvs' \\ bvs
         unboundTypeVars iclass unbound_vars
      schs <- mapM askEnv qs `wrapError` iclass
      sigs' <- mapM tcSig (zip sigs schs) `wrapError` iclass
      pure (Class bvs ctx n vs v sigs')

tcSig :: (Signature Name, Scheme) -> TcM (Signature Id)
tcSig (sig, (Forall _ (_ :=> t)))
  = do
      let (ts,r) = splitTy t
          param (Typed n t) t1 = Typed (Id n t1) t1
          param (Untyped n) t1 = Typed (Id n t1) t1
          params' = zipWith param (sigParams sig) ts
      kindCheck t `wrapError` sig
      pure (Signature (sigVars sig)
                      (sigContext sig)
                      (sigName sig)
                      params'
                      (Just r))

-- type checking binding groups

extractSignatures :: [FunDef Name] -> TcM [(Name, Scheme)]
extractSignatures fds = forM fds extractSig
  where
    extractSig (FunDef sig _)
      | hasAnn sig = do
        scheme <- annotatedScheme [] sig
        return (sigName sig, scheme)
      | otherwise = do
        tvar <- freshTyVar
        return (sigName sig, monotype tvar)

tcBindGroup :: [FunDef Name] -> TcM [FunDef Id]
tcBindGroup binds
  = do
      nmschs <- extractSignatures binds
      (funs', schs) <- unzip <$> (withLocalCtx nmschs $  mapM (tcFunDef True [] []) binds)
      let names = map (sigName . funSignature) funs'
      mapM_ (uncurry extEnv) (zip names schs)
      noDesugarCalls <- getNoDesugarCalls
      let funs1 = everywhere (mkT gen) funs'
      unless noDesugarCalls $ generateTopDeclsFor (zip funs1 schs)
      pure funs1

generateTopDeclsFor :: [(FunDef Id, Scheme)] -> TcM ()
generateTopDeclsFor ps
  = do
      gen <- askGeneratingDefs
      if gen then do
        (dts, instds) <- unzip <$> mapM generateDecls ps
        -- mapM_ checkDataType dts
        s <- getSubst
        clearSubst
        disableBoundVariableCondition (mapM_ checkInstance instds)
        insts' <- withNoGeneratingDefs (mapM tcInstance instds)
        mapM_ writeTopDecl ((TDataDef <$> dts) ++ (TInstDef <$> insts'))
        putSubst s
      else pure ()

-- type checking contract constructors

tcConstructor :: Constructor Name -> TcM (Constructor Id)
tcConstructor (Constructor ps bd)
  = do
      -- building parameters for constructors
      ps' <- mapM tcParam ps
      let f (Typed (Id n t) _) = pure (n, monotype t)
          f (Untyped (Id n _)) = ((n,) . monotype) <$> freshTyVar
      lctx <- mapM f ps'
      (bd', _ ,_) <- withLocalCtx lctx (tcBody bd)
      pure (Constructor ps' bd')

-- checking class definitions and adding them to environment

checkClasses :: [Class Name] -> TcM ()
checkClasses = mapM_ checkClass

checkClass :: Class Name -> TcM ()
checkClass icls@(Class bvs ps n vs v sigs)
  = do
      let p = InCls n (TyVar v) (TyVar <$> vs)
          ms' = map sigName sigs
      bound <- askBoundVariableCondition n
      unless bound (checkBoundVariable ps (v:vs) `wrapError` icls)
      addClassInfo n (length vs) ms' ps p
      mapM_ (checkSignature p) sigs
    where
      checkSignature p sig@(Signature vs ctx f ps mt)
        = do
            pst <- mapM tyParam ps
            t' <- maybe (pure unit) pure mt
            let ft = funtype pst t'
            unless (all (`elem` bvs) (v : vs))
                   (unboundTypeVars sig ((v : vs) \\ bvs))
            addClassMethod p sig `wrapError` icls

addClassInfo :: Name -> Arity -> [Method] -> [Pred] -> Pred -> TcM ()
addClassInfo n ar ms ps p
  = do
      ct <- gets classTable
      when (Map.member n ct) (duplicatedClassDecl n)
      modify (\ env ->
        env{ classTable = Map.insert n (ClassInfo ar ms p ps)
                                       (classTable env)})

addClassMethod :: Pred -> Signature Name -> TcM ()
addClassMethod p@(InCls c _ _) sig@(Signature _ ctx f ps t)
  = do
      tps <- mapM tyParam ps
      t' <- maybe (pure unit) pure t
      let ty = funtype tps t'
          vs = bv ty
          ctx' = [p] `union` ctx
          qn = if isQual f then f else QualName c (pretty f)
          sch = Forall vs (ctx' :=> ty)
      r <- maybeAskEnv f
      unless (isNothing r) (duplicatedClassMethod f `wrapError` sig)
      extEnv qn sch
      pure ()
addClassMethod p@(_ :~: _) (Signature _ _ n _ _)
  = throwError $ unlines [
                    "Invalid constraint:"
                  , pretty p
                  , "in class method:"
                  , pretty n
                  ]

-- error for class definitions

signatureError :: Name -> Tyvar -> Signature Name -> Ty -> TcM ()
signatureError n v sig@(Signature _ ctx f _ _) t
  | null ctx = throwError $ unlines ["Impossible! Class context is empty in function:"
                                    , pretty f
                                    , "which is a member of the class declaration:"
                                    , pretty n
                                    ]
  | v `notElem` fv t = throwError $ unlines ["Main class type variable"
                                            , pretty v
                                            , "does not occur in type"
                                            , pretty t
                                            , "which is the defined type for function"
                                            , pretty f
                                            , "that is a member of class definition"
                                            , pretty n
                                            ]

duplicatedClassDecl :: Name -> TcM ()
duplicatedClassDecl n
  = throwError $ "Duplicated class definition:" ++ pretty n

duplicatedClassMethod :: Name -> TcM ()
duplicatedClassMethod n
  = throwError $ "Duplicated class method definition:" ++ pretty n

invalidPragmaDecl :: [Pragma] -> TcM ()
invalidPragmaDecl ps
  = throwError $ unlines $ ["Invalid pragma definitions:"] ++ map pretty ps


