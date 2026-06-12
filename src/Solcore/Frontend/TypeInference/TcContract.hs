module Solcore.Frontend.TypeInference.TcContract where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Generics hiding (Constr)
import Data.List
import Data.List.NonEmpty qualified as N
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import Solcore.Frontend.Pretty.ShortName
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.InvokeGen
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcStmt
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Pipeline.Options
import Solcore.Primitives.Primitives

data TopDeclCheckMode
  = CheckTopDeclBody
  | TrustTopDeclBody
  deriving (Eq, Show)

data TopDeclCheck a
  = TopDeclCheck
  { topDeclCheckMode :: TopDeclCheckMode,
    topDeclCheckDecl :: TopDecl a
  }
  deriving (Eq, Show)

typeInferTopDeclChecks ::
  Option ->
  [Import] ->
  [InstanceHead] ->
  [(Name, [Name])] ->
  [TopDeclCheck Name] ->
  IO (Either String (CompUnit Id, TcEnv))
typeInferTopDeclChecks opts imps trustedInstances partialTypes topDeclChecks =
  do
    r <-
      runTcM
        (tcTopDeclChecks topDeclChecks)
        ( (initTcEnv opts)
            { trustedInstanceHeads = trustedInstances,
              partialDataTypes = Set.fromList (map fst partialTypes),
              partialDataTypeConstructors =
                Map.fromList
                  [ (partialTypeName, Set.fromList constructorNames)
                    | (partialTypeName, constructorNames) <- partialTypes
                  ]
            }
        )
    case r of
      Left err1 -> pure $ Left err1
      Right (ds, env) -> do
        let ds1 = (ds ++ generated env)
        pure (Right (CompUnit imps ds1, env))

-- Build a pure synonym table from parsed synonym declarations
buildSynTable :: [TySym] -> SynTable
buildSynTable syns =
  Map.fromList [(symName s, SynInfo (length (symVars s)) (symVars s) (symType s)) | s <- syns]

-- Monadic recursive expansion of a single Ty using a SynTable.
-- Reports an error when a synonym is applied to the wrong number of arguments.
expandTyM :: SynTable -> Ty -> TcM Ty
expandTyM st (TyCon n ts) = do
  ts' <- mapM (expandTyM st) ts
  case Map.lookup n st of
    Just (SynInfo _ params body)
      | length params == length ts' ->
          expandTyM st (insts (zip params ts') body)
      | otherwise ->
          throwError $
            unlines
              [ "Type synonym arity mismatch for '" ++ pretty n ++ "':",
                "  expected " ++ show (length params) ++ " argument(s)",
                "  but got  " ++ show (length ts')
              ]
    Nothing -> pure (TyCon n ts')
expandTyM st (t1 :-> t2) = (:->) <$> expandTyM st t1 <*> expandTyM st t2
expandTyM _ t = pure t

tcTopDeclChecks :: [TopDeclCheck Name] -> TcM [TopDecl Id]
tcTopDeclChecks topDeclChecks =
  do
    setupPragmas ps
    checkSynonymCycles syns
    let st = buildSynTable syns
    csExpanded <- everywhereM (mkM (expandTyM st)) cs
    mapM_ checkTopDecl (filter isClass csExpanded)
    mapM_ checkTopDecl (filter (not . isClass) csExpanded)
    trustImportedDecls csExpanded
    catMaybes <$> zipWithM tcTopDeclWithVisibility topDeclChecks csExpanded
  where
    cs = map topDeclCheckDecl topDeclChecks
    ps = foldr step [] cs
    step (TPragmaDecl p) ac = p : ac
    step _ ac = ac
    isClass (TClassDef _) = True
    isClass _ = False
    syns = [s | TSym s <- cs]
    trustImportedDecls expandedDecls =
      mapM_
        (withPartialDataTypesDisabled . trustImportedTopDecl)
        [ expandedDecl
          | (topDeclCheck, expandedDecl) <- zip topDeclChecks expandedDecls,
            topDeclCheckMode topDeclCheck == TrustTopDeclBody
        ]
    tcTopDeclWithVisibility topDeclCheck expandedDecl =
      case topDeclCheckMode topDeclCheck of
        CheckTopDeclBody ->
          Just <$> tcTopDecl' expandedDecl
        TrustTopDeclBody ->
          withPartialDataTypesDisabled $
            pure Nothing
    tcTopDecl' d = timeItNamed (shortName d) $ do
      clearSubst
      tcTopDecl d

-- check for recursive type synonyms

checkSynonymCycles :: [TySym] -> TcM ()
checkSynonymCycles syns =
  do
    let synNames = map symName syns
        deps = [(symName s, synDeps synNames s) | s <- syns]
    case findCycle deps of
      Nothing -> pure ()
      Just cyclePath -> recursiveSynonymError cyclePath

synDeps :: [Name] -> TySym -> [Name]
synDeps synNames (TySym _ _ t) = filter (`elem` synNames) (tyNames t)

tyNames :: Ty -> [Name]
tyNames (TyCon n ts) = n : concatMap tyNames ts
tyNames _ = []

findCycle :: [(Name, [Name])] -> Maybe [Name]
findCycle deps = go [] (map fst deps)
  where
    depMap = Map.fromList deps

    go _ [] = Nothing
    go visited (n : ns)
      | n `elem` visited =
          -- visited is in reverse order (newest first), extract the cycle path
          -- e.g., if visited = [B, A] and we found n = A, the cycle is A -> B -> A
          let revPath = takeWhile (/= n) visited ++ [n]
           in Just (reverse revPath ++ [n])
      | otherwise = case Map.lookup n depMap of
          Nothing -> go visited ns
          Just ds -> case go (n : visited) ds of
            Just cyclePath -> Just cyclePath
            Nothing -> go visited ns

recursiveSynonymError :: [Name] -> TcM a
recursiveSynonymError cyclePath =
  throwError $
    unlines
      [ "Recursive type synonym detected:",
        "  " ++ intercalate " -> " (map pretty cyclePath)
      ]

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
            (x : xs) -> DisableFor (x N.:| xs)

tcTopDecl :: TopDecl Name -> TcM (TopDecl Id)
tcTopDecl (TContr c) =
  do
    (c', assumps) <- tcContract c
    mapM_ (uncurry extEnv) assumps
    pure (TContr c')
tcTopDecl (TFunDef fd) =
  do
    requireAnnotations fd
    fd' <- tcBindGroup [fd]
    case fd' of
      (fd1 : _) -> pure (TFunDef fd1)
      _ -> throwError "Impossible! Empty binding group!"
tcTopDecl (TClassDef c) =
  TClassDef <$> tcClass c
tcTopDecl (TInstDef is) =
  TInstDef <$> tcInstance is
tcTopDecl (TMutualDef ts) =
  do
    let f (TFunDef fd) = fd
        f td = error $ "tcTopDecl: expected function in mutual definition, got " ++ show td
    let fds = map f ts
    mapM_ requireAnnotations fds
    ts' <- tcBindGroup fds
    pure (TMutualDef $ map TFunDef ts')
tcTopDecl (TDataDef d) =
  do
    pure (TDataDef d)
tcTopDecl (TSym s) =
  pure (TSym s)
tcTopDecl (TExportDecl d) =
  pure (TExportDecl d)
tcTopDecl (TPragmaDecl d) =
  pure (TPragmaDecl d)

trustImportedTopDecl :: TopDecl Name -> TcM ()
trustImportedTopDecl (TFunDef fd) =
  extImportedFunDefs [fd]
trustImportedTopDecl (TMutualDef ds) =
  extImportedFunDefs [fd | TFunDef fd <- ds]
trustImportedTopDecl _ =
  pure ()

extImportedFunDefs :: [FunDef Name] -> TcM ()
extImportedFunDefs fds = do
  nmschs <- extractSignatures fds
  mapM_ (uncurry extEnv) nmschs
  noDesugarCalls <- getNoDesugarCalls
  unless noDesugarCalls $ do
    typedFds <- mapM typedImportedFunDef fds
    generateTopDeclsFor (zip typedFds (map snd nmschs))

typedImportedFunDef :: FunDef Name -> TcM (FunDef Id)
typedImportedFunDef (FunDef sig _body) = do
  params <- mapM tcParam (sigParams sig)
  pure (FunDef sig {sigParams = params} [])

checkTopDecl :: TopDecl Name -> TcM ()
checkTopDecl (TClassDef c) =
  checkClass c
checkTopDecl (TInstDef is) =
  checkInstance is
checkTopDecl (TDataDef dt) =
  checkDataType dt
checkTopDecl (TSym s) =
  checkSynonym s
checkTopDecl (TFunDef (FunDef sig _)) =
  extSignature sig
checkTopDecl (TExportDecl _) = pure ()
checkTopDecl _ = pure ()

-- type inference for contracts

tcContract :: Contract Name -> TcM (Contract Id, [(Name, Scheme)])
tcContract c@(Contract n vs cdecls) =
  withLocalEnv $ withContractName n $ do
    ctx' <- gets ctx
    initializeEnv c
    decls' <- mapM tcDecl' cdecls
    ctx1 <- gets ctx
    let ctx2 = Map.toList $ Map.difference ctx1 ctx'
    pure (Contract n vs decls', ctx2)
  where
    tcDecl' d =
      do
        clearSubst
        d' <- tcDecl d
        s <- getSubst
        pure (everywhere (mkT (apply @Id s)) d')

-- initializing context for a contract

initializeEnv :: Contract Name -> TcM ()
initializeEnv (Contract _ _ cdecls) = do
  mapM_ checkDecl cdecls
  -- Pre-register annotated function signatures in ctx so that forward references
  -- (e.g. the dispatch-generated 'main') can resolve user-defined functions
  -- before their bodies are type-checked.  Only annotated functions are
  -- pre-registered; unannotated ones would produce a stale fresh type variable
  -- that could interfere with later inference.
  let fds =
        [fd | CFunDecl fd@(FunDef sig _) <- cdecls, hasAnn sig]
          ++ [fd | CMutualDecl ds <- cdecls, CFunDecl fd@(FunDef sig _) <- ds, hasAnn sig]
  nmschs <- extractSignatures fds
  mapM_ (uncurry extEnv) nmschs

checkDecl :: ContractDecl Name -> TcM ()
checkDecl (CDataDecl dt) =
  checkDataType dt
checkDecl (CFunDecl (FunDef sig _)) =
  extSignature sig
checkDecl (CFieldDecl fd) =
  tcField fd >> return ()
checkDecl (CMutualDecl ds) =
  mapM_ checkDecl ds
checkDecl _ = return ()

-- type inference for declarations

tcDecl :: ContractDecl Name -> TcM (ContractDecl Id)
tcDecl (CFieldDecl fd) = CFieldDecl <$> tcField fd
tcDecl (CFunDecl d) =
  do
    requireAnnotations d
    d' <- tcBindGroup [d]
    case d' of
      [] -> throwError "Impossible! Empty function binding!"
      (x : _) -> pure (CFunDecl x)
tcDecl (CMutualDecl ds) =
  do
    let f (CFunDecl fd) = fd
        f d = error $ "tcDecl: expected function declaration in mutual group, got " ++ show d
    let fds = map f ds
    mapM_ requireAnnotations fds
    ds' <- tcBindGroup fds
    pure (CMutualDecl (map CFunDecl ds'))
tcDecl (CConstrDecl cd) = CConstrDecl <$> tcConstructor cd
tcDecl (CDataDecl d) = CDataDecl <$> tcDataDecl d

-- kind check data declarations

tcDataDecl :: DataTy -> TcM DataTy
tcDataDecl (DataTy n vs cs) =
  DataTy n vs <$> mapM tcConstr cs

tcConstr :: Constr -> TcM Constr
tcConstr (Constr n ts) =
  Constr n <$> mapM kindCheck ts

-- type checking fields

tcField :: Field Name -> TcM (Field Id)
tcField d@(Field n t (Just e)) =
  do
    (e', _, t') <- tcExp e
    t1 <- kindCheck t `wrapError` d
    _ <- mgu t t' `wrapError` d
    extEnv n (monotype t1)
    return (Field n t1 (Just e'))
tcField d@(Field n t _) =
  do
    t1 <- kindCheck t `wrapError` d
    extEnv n (monotype t1)
    pure (Field n t1 Nothing)

tcClass :: Class Name -> TcM (Class Id)
tcClass iclass@(Class bvs classCtx n vs v sigs) =
  do
    let freeInSig (Signature sv _ _ ps _ mt _) = bv (ps, mt) \\ sv
        bvs' = bv classCtx `union` bv (map TyVar (v : vs)) `union` concatMap freeInSig sigs
        ns = map sigName sigs
        qs = map (QualName n . pretty) ns
    when (any (`notElem` bvs) bvs') $ do
      let unbound_vars = bvs' \\ bvs
      unboundTypeVars iclass unbound_vars
    schs <- mapM askEnv qs `wrapError` iclass
    sigs' <- mapM tcSig (zip sigs schs) `wrapError` iclass
    pure (Class bvs classCtx n vs v sigs')

tcSig :: (Signature Name, Scheme) -> TcM (Signature Id)
tcSig (sig, (Forall _ (_ :=> t))) =
  do
    t1 <- kindCheck t `wrapError` sig
    let (ts, r) = splitTy t1
        param (Typed c n _) t2 = Typed c (Id n t2) t2
        param (Untyped c n) t2 = Typed c (Id n t2) t2
        params' = zipWith param (sigParams sig) ts
    pure
      ( Signature
          (sigVars sig)
          (sigContext sig)
          (sigName sig)
          params'
          (sigRetComptime sig)
          (Just r)
          (sigPayable sig)
      )

-- type checking binding groups

extractSignatures :: [FunDef Name] -> TcM [(Name, Scheme)]
extractSignatures fds = forM fds extractSig
  where
    extractSig (FunDef sig _)
      | hasAnn sig = do
          scheme <- annotatedScheme [] [] sig
          return (sigName sig, scheme)
      | otherwise = do
          tvar <- freshTyVar
          return (sigName sig, monotype tvar)

tcBindGroup :: [FunDef Name] -> TcM [FunDef Id]
tcBindGroup binds =
  do
    nmschs <- extractSignatures binds
    (funs', schs) <- unzip <$> (withLocalCtx nmschs $ mapM (tcFunDef True [] []) binds)
    let names = map (sigName . funSignature) funs'
    mapM_ (uncurry extEnv) (zip names schs)
    noDesugarCalls <- getNoDesugarCalls
    let funs1 = everywhere (mkT gen) funs'
    unless noDesugarCalls $ generateTopDeclsFor (zip funs1 schs)
    pure funs1

generateTopDeclsFor :: [(FunDef Id, Scheme)] -> TcM ()
generateTopDeclsFor ps =
  do
    generating <- askGeneratingDefs
    if generating
      then do
        (dts, instds) <- unzip <$> mapM generateDecls ps
        s <- getSubst
        clearSubst
        disableBoundVariableCondition (mapM_ checkInstance instds)
        insts' <- withNoGeneratingDefs (mapM tcInstance instds)
        mapM_ writeTopDecl ((TDataDef <$> dts) ++ (TInstDef <$> insts'))
        putSubst s
      else pure ()

-- type checking contract constructors

tcConstructor :: Constructor Name -> TcM (Constructor Id)
tcConstructor (Constructor ps bd) =
  do
    -- building parameters for constructors
    ps' <- mapM tcParam ps
    let f (Typed _ (Id n t) _) = pure (n, monotype t)
        f (Untyped _ (Id n _)) = ((n,) . monotype) <$> freshTyVar
    lctx <- mapM f ps'
    (bd', _, _) <- withLocalCtx lctx (tcBody bd)
    pure (Constructor ps' bd')

-- checking class definitions and adding them to environment

checkClasses :: [Class Name] -> TcM ()
checkClasses = mapM_ checkClass

checkClass :: Class Name -> TcM ()
checkClass icls@(Class bvs ps n vs v sigs) =
  do
    let p = InCls n (TyVar v) (TyVar <$> vs)
        ms' = map sigName sigs
    checkAllTypeVarsBound icls (v : vs) bvs
    boundEnabled <- askBoundVariableCondition n
    unless boundEnabled (checkBoundVariable ps (v : vs) `wrapError` icls)
    addClassInfo n (length vs) ms' ps p
    mapM_ (checkSignature p) sigs
  where
    checkSignature p sig@(Signature methodVars _ _ params _ mt _) =
      do
        fullSignature sig `wrapError` icls
        _ <- mapM tyParam params
        _ <- maybe (pure unit) pure mt
        checkAllTypeVarsBound sig (bv sig) (bvs ++ methodVars)
        addClassMethod p sig `wrapError` icls

addClassInfo :: Name -> Arity -> [Method] -> [Pred] -> Pred -> TcM ()
addClassInfo n ar ms ps p =
  do
    ct <- gets classTable
    when (Map.member n ct) (duplicatedClassDecl n)
    modify
      ( \env ->
          env
            { classTable =
                Map.insert
                  n
                  (ClassInfo ar ms p ps)
                  (classTable env)
            }
      )

addClassMethod :: Pred -> Signature Name -> TcM ()
addClassMethod p@(InCls c _ _) sig@(Signature _ methodCtx f ps _ t _) =
  do
    tps <- mapM tyParam ps
    t' <- maybe (pure unit) pure t
    let ty = funtype tps t'
        vs = bv ty
        ctx' = [p] `union` methodCtx
        qn = if isQual f then f else QualName c (pretty f)
        sch = Forall vs (ctx' :=> ty)
    r <- maybeAskEnv f
    unless (isNothing r) (duplicatedClassMethod f `wrapError` sig)
    extEnv qn sch
    pure ()
addClassMethod p@(_ :~: _) (Signature _ _ n _ _ _ _) =
  throwError $
    unlines
      [ "Invalid constraint:",
        pretty p,
        "in class method:",
        pretty n
      ]

-- error for class definitions

signatureError :: Name -> Tyvar -> Signature Name -> Ty -> TcM ()
signatureError n v (Signature _ methodCtx f _ _ _ _) t
  | null methodCtx =
      throwError $
        unlines
          [ "Impossible! Class context is empty in function:",
            pretty f,
            "which is a member of the class declaration:",
            pretty n
          ]
  | v `notElem` fv t =
      throwError $
        unlines
          [ "Main class type variable",
            pretty v,
            "does not occur in type",
            pretty t,
            "which is the defined type for function",
            pretty f,
            "that is a member of class definition",
            pretty n
          ]
  | otherwise = pure ()

duplicatedClassDecl :: Name -> TcM ()
duplicatedClassDecl n =
  throwError $ "Duplicated class definition:" ++ pretty n

duplicatedClassMethod :: Name -> TcM ()
duplicatedClassMethod n =
  throwError $ "Duplicated class method definition:" ++ pretty n

invalidPragmaDecl :: [Pragma] -> TcM ()
invalidPragmaDecl ps =
  throwError $ unlines $ ["Invalid pragma definitions:"] ++ map pretty ps
