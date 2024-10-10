module Solcore.Frontend.TypeInference.TcContract where 

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Control.Monad.State

import Data.Generics hiding (Constr)
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcInvokeGen
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcStmt
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives

-- top level type inference function 

typeInfer :: CompUnit Name -> IO (Either String (CompUnit Id, TcEnv))
typeInfer (CompUnit imps decls) 
  = do
      r <- runTcM (tcCompUnit (CompUnit imps decls)) initTcEnv
      case r of 
        Left err -> pure $ Left err 
        Right (((CompUnit imps ds), ts), env) -> 
          pure (Right ((CompUnit imps (ds ++ ts)), env)) 

-- type inference for a compilation unit 

tcCompUnit :: CompUnit Name -> TcM (CompUnit Id)
tcCompUnit (CompUnit imps cs)
  = do 
      loadImports imps
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
      tcTopDecl' d = do 
        clearSubst
        d' <- tcTopDecl d 
        s <- getSubst 
        pure (everywhere (mkT (applyI s)) d')

-- setting up pragmas for type checking

setupPragmas :: [Pragma] -> TcM ()
setupPragmas ps 
  = do
      unless validPragmas (invalidPragmaDecl ps)
      mapM_ setupPragma ps 
    where
      setupPragma (Pragma NoBoundVariableCondition ns)
        = setBoundVariableCondition ns 
      setupPragma (Pragma NoPattersonCondition ns) 
        = setPattersonCondition ns 
      setupPragma (Pragma NoCoverageCondition ns) 
        = setCoverage ns 
      single [] = True 
      single [ _ ] = True 
      single _ = False 
      isBound NoBoundVariableCondition = True 
      isBound _ = False 
      isPatterson NoPattersonCondition = True 
      isPatterson _ = False 
      isCoverage NoCoverageCondition = True 
      isCoverage _ = False 

      unique p xs = single (filter (p . pragmaType) xs)

      validPragmas = and [ unique isBound ps 
                         , unique isPatterson ps 
                         , unique isCoverage ps 
                         ] 

tcTopDecl :: TopDecl Name -> TcM (TopDecl Id)
tcTopDecl (TContr c) 
  = TContr <$> tcContract c
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
    checkDataType d
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

-- TODO load import information

loadImports :: [Import] -> TcM ()
loadImports _ = return ()

-- type inference for contracts 

tcContract :: Contract Name -> TcM (Contract Id) 
tcContract c@(Contract n vs decls) 
  = withLocalEnv do
      initializeEnv c
      decls' <- mapM tcDecl' decls
      pure (Contract n vs decls')
    where 
      tcDecl' d 
        = do 
          clearSubst 
          d' <- tcDecl d
          s <- getSubst
          pure (everywhere (mkT (applyI s)) d')

-- initializing context for a contract

initializeEnv :: Contract Name -> TcM ()
initializeEnv (Contract n vs decls)
  = do 
      setCurrentContract n (length vs) 
      mapM_ checkDecl decls 

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

extSignature :: Signature Name -> TcM ()
extSignature sig@(Signature _ preds n ps t)
  = do
      -- checking if the function is previously defined
      te <- gets ctx
      when (Map.member n te) (duplicatedFunDef n) `wrapError` sig
      argTys <- mapM tyParam ps
      t' <- maybe freshTyVar pure t
      let 
        ty = funtype argTys t'
        vs = fv (preds :=> ty)
      sch <- generalize (preds, ty) 
      extEnv n sch

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

tcInstance :: Instance Name -> TcM (Instance Id)
tcInstance idecl@(Instance ctx n ts t funs) 
  = do
      checkCompleteInstDef n (map (sigName . funSignature) funs) 
      funs' <- buildSignatures n ts t funs `wrapError` idecl 
      (funs1, pss', ts') <- unzip3 <$> mapM tcFunDef  funs' `wrapError` idecl
      withCurrentSubst (Instance ctx n ts t funs1)

checkCompleteInstDef :: Name -> [Name] -> TcM ()
checkCompleteInstDef n ns 
  = do 
      mths <- methods <$> askClassInfo n 
      let remaining = mths \\ ns 
      when (not $ null remaining) do 
        warning $ unlines $ ["Incomplete definition for class:"
                            , pretty n
                            , "missing definitions for:"
                            ] ++ map pretty remaining

buildSignatures :: Name -> [Ty] -> Ty -> [FunDef Name] -> TcM [FunDef Name]
buildSignatures n ts t funs 
  = do 
      cpred <- classpred <$> askClassInfo n 
      sm <- matchPred cpred (InCls n t ts)
      let qname m = QualName n (pretty m)
      schs <- mapM (askEnv . qname . sigName . funSignature) funs
      let  
          app (Forall vs (_ :=> t1)) = apply sm t1
          tinsts = map app schs
      zipWithM (buildSignature n) tinsts funs 

buildSignature :: Name -> Ty -> FunDef Name -> TcM (FunDef Name)
buildSignature n t (FunDef sig bd)
  = do 
      let (args, ret) = splitTy t 
          sig' = typeSignature n args ret sig
      pure (FunDef sig' bd)

typeSignature :: Name -> [Ty] -> Ty -> Signature Name -> Signature Name 
typeSignature nm args ret sig 
  = sig { 
          sigName = QualName nm (pretty $ sigName sig)
        , sigParams = zipWith paramType args (sigParams sig)
        , sigReturn = Just ret
        }
    where 
      paramType t (Typed n _) = Typed n (skolemize t)
      paramType t (Untyped n) = Typed n (skolemize t)

tcClass :: Class Name -> TcM (Class Id)
tcClass iclass@(Class ctx n vs v sigs) 
  = do
      let ns = map sigName sigs
          qs = map (QualName n . pretty) ns 
      schs <- mapM askEnv qs `wrapError` iclass 
      sigs' <- mapM tcSig (zip sigs schs) `wrapError` iclass
      pure (Class ctx n vs v sigs')

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

tcBindGroup :: [FunDef Name] -> TcM [FunDef Id]
tcBindGroup binds 
  = do
      funs <- mapM scanFun binds
      (funs', pss, ts) <- unzip3 <$> mapM tcFunDef funs 
      ts' <- withCurrentSubst ts  
      schs <- mapM generalize (zip pss ts')
      let names = map (sigName . funSignature) funs 
      let p (x,y) = pretty x ++ " :: " ++ pretty y
      mapM_ (uncurry extEnv) (zip names schs)
      pure funs'

-- type checking a single bind

tcFunDef :: FunDef Name -> TcM (FunDef Id, [Pred], Ty)
tcFunDef d@(FunDef sig bd) 
  = withLocalEnv do
      (params', schs, ts) <- tcArgs (sigParams sig)
      (bd', ps1, t') <- withLocalCtx schs (tcBody bd) `wrapError` d
      s1 <- getSubst
      sch <- askEnv (sigName sig) `wrapError` d 
      (ps :=> t) <- freshInst sch
      let t1 = apply s1 $ foldr (:->) t' ts
      sch' <- generalize (ps1, t1) `wrapError` d
      s <- match t t1 `wrapError` d
      extSubst s 
      rTy <- withCurrentSubst t'
      let sig' = apply s1 $ Signature (sigVars sig) 
                           (sigContext sig) 
                           (sigName sig)
                           params' 
                           (Just rTy)
      ps2 <- reduceContext (ps ++ ps1) `wrapError` d
      info ["> Infered type for ", pretty (sigName sig), " is ", pretty sch']
      -- generateDecls (FunDef sig' bd', sch')
      pure (apply s1 $ FunDef sig' bd', apply s1 ps2, apply s1 t1)

scanFun :: FunDef Name -> TcM (FunDef Name)
scanFun (FunDef sig bd)
  = flip FunDef bd <$> fillSignature sig 
    where 
      f (Typed n t) = pure $ Typed n (skolemize t)
      f (Untyped n) = Typed n <$> freshTyVar
      fillSignature (Signature vs ctx n ps t)
        = do 
            ps' <- mapM f ps 
            pure (Signature vs ctx n ps' t)

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
checkClass icls@(Class ps n vs v sigs) 
  = do 
      let p = InCls n (TyVar v) (TyVar <$> vs)
          ms' = map sigName sigs
      bound <- askBoundVariableCondition n
      unless bound (checkBoundVariable ps (v:vs) `wrapError` icls)
      addClassInfo n (length vs) ms' p
      mapM_ (checkSignature p) sigs 
    where
      checkSignature p sig@(Signature vs ctx f ps mt)
        = do
            pst <- mapM tyParam ps
            t' <- maybe freshTyVar pure mt
            let ft = funtype pst t' 
            unless (v `elem` fv ft)
                   (signatureError n v sig ft)
            addClassMethod p sig `wrapError` icls 

addClassInfo :: Name -> Arity -> [Method] -> Pred -> TcM ()
addClassInfo n ar ms p
  = do 
      ct <- gets classTable
      when (Map.member n ct) (duplicatedClassDecl n)
      modify (\ env -> 
        env{ classTable = Map.insert n (ClassInfo ar ms p) 
                                       (classTable env)})

addClassMethod :: Pred -> Signature Name -> TcM ()
addClassMethod p@(InCls c _ _) sig@(Signature _ ctx f ps t) 
  = do
      tps <- mapM tyParam ps
      t' <- maybe freshTyVar pure t
      let ty = funtype tps t'
          vs = fv ty
          ctx' = [p] `union` ctx
          qn = QualName c (pretty f)
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

-- checking instances and adding them in the environment

checkInstances :: [Instance Name] -> TcM ()
checkInstances = mapM_ checkInstance 

checkInstance :: Instance Name -> TcM ()
checkInstance idef@(Instance ctx n ts t funs)
  = do
      let ipred = InCls n t ts
      -- checking the coverage condition 
      insts <- askInstEnv n `wrapError` ipred
      checkOverlap ipred insts
      coverage <- askCoverage n
      unless coverage (checkCoverage n ts t `wrapError` idef)
      -- checking Patterson condition
      patterson <- askPattersonCondition n 
      unless patterson (checkMeasure ctx ipred `wrapError` idef)
      -- checking bound variable condition
      bound <- askBoundVariableCondition n 
      unless bound (checkBoundVariable ctx (fv (t : ts)) `wrapError` idef)
      -- checking instance methods
      mapM_ (checkMethod ipred) funs
      let ninst = anfInstance $ ctx :=> InCls n t ts 
      -- add to the environment
      addInstance n ninst 

-- bound variable check 

checkBoundVariable :: [Pred] -> [Tyvar] -> TcM () 
checkBoundVariable ps vs 
  = unless (all (\ v -> v `elem` vs) (fv ps)) $ do 
      throwError "Bounded variable condition fails!"


checkOverlap :: Pred -> [Inst] -> TcM ()
checkOverlap _ [] = pure ()
checkOverlap p@(InCls _ t _) (i:is) 
  = do 
        i' <- renameVars (fv t) i
        case i' of 
          (ps :=> (InCls _ t' _)) -> 
            case mgu t t' of
              Right _ -> throwError (unlines [ "Overlapping instances are not supported" 
                                             , "instance:"
                                             , pretty p
                                             , "overlaps with:"
                                             , pretty i'])
              Left _ -> checkOverlap p is
        return ()

-- check coverage condition 

checkCoverage :: Name -> [Ty] -> Ty -> TcM ()
checkCoverage cn ts t 
  = do 
      let strongTvs = fv t 
          weakTvs = fv ts 
          undetermined = weakTvs \\ strongTvs
      unless (null undetermined) $ 
          throwError (unlines [
            "Coverage condition fails for class:"
          , pretty cn 
          , "- the type:"
          , pretty t 
          , "does not determine:"
          , intercalate ", " (map pretty undetermined)
          ])

checkMethod :: Pred -> FunDef Name -> TcM () 
checkMethod ih@(InCls n t ts) d@(FunDef sig _) 
  = do
      -- getting current method signature in class
      let qn = QualName n (show (sigName sig))
      st@(Forall _ (qs :=> ty)) <- askEnv qn `wrapError` d 
      p <- maybeToTcM (unwords [ "Constraint for"
                               , show n
                               , "not found in type of"
                               , show $ sigName sig])
                      (findPred n qs)
      -- matching substitution of instance head and class predicate
      _ <- liftEither (matchPred p ih) `wrapError` ih
      pure ()

findPred :: Name -> [Pred] -> Maybe Pred 
findPred _ [] = Nothing 
findPred n (p@(InCls n' _ _) : ps) 
  | n == n' = Just p 
  | otherwise = findPred n ps

-- checking Patterson conditions 

checkMeasure :: [Pred] -> Pred -> TcM ()
checkMeasure ps c 
  = if all smaller ps then return ()
    else throwError $ unlines [ "Instance "
                              , pretty c
                              , "does not satisfy the Patterson conditions."]
    where smaller p = measure p < measure c

-- error for class definitions 

signatureError :: Name -> Tyvar -> Signature Name -> Ty -> TcM ()
signatureError n v sig@(Signature _ ctx f _ _) t
  | null ctx = throwError $ unlines ["Impossible! Class context is empty in function:" 
                                    , pretty f
                                    , "which is a membre of the class declaration:"
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

duplicatedFunDef :: Name -> TcM () 
duplicatedFunDef n 
  = throwError $ "Duplicated function definition:" ++ pretty n

invalidPragmaDecl :: [Pragma] -> TcM () 
invalidPragmaDecl ps 
  = throwError $ unlines $ ["Invalid pragma definitions:"] ++ map pretty ps 

-- Instances for elaboration 

instance HasType (FunDef Id) where 
  apply s (FunDef sig bd)
    = FunDef (apply s sig) (apply s bd)
  fv (FunDef sig bd)
    = fv sig `union` fv bd

instance HasType (Instance Id) where 
  apply s (Instance ctx n ts t funs) 
    = Instance (apply s ctx) n (apply s ts) (apply s t) (apply s funs)
  fv (Instance ctx n ts t funs) 
    = fv ctx `union` fv (t : ts) `union` fv funs
