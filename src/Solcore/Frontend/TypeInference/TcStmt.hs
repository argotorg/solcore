module Solcore.Frontend.TypeInference.TcStmt where

import Common.Pretty
import Control.Monad
import Control.Monad.Except
import Control.Monad.State

import Data.Generics hiding (Constr)
import Data.List
import qualified Data.Map as Map
import Data.Maybe
import GHC.Stack

import Solcore.Frontend.Pretty.ShortName
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.InvokeGen
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcReduce
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives

import Language.Yul


-- type inference for statements


type Infer f = f Name -> TcM (f Id, [Pred], Ty)

tcStmt :: Infer Stmt
tcStmt e@(lhs := rhs)
  = do
      (lhs1, ps1, t1) <- tcExp lhs
      (rhs1, ps2, t2) <- tcExp rhs
      s <- match t2 t1 `wrapError` e
      _ <- extSubst s
      pure (lhs1 := rhs1, apply s $ ps1 ++ ps2, unit)
tcStmt e@(Let n mt me)
  = do
      (me', psf, tf) <- case (mt, me) of
                      (Just t, Just e1) -> do
                        (e', ps1, t1) <- tcExp e1
                        _ <- kindCheck t1 `wrapError` e
                        let bvs = bv t
                        sks <- mapM (const freshTyVar) bvs
                        let t' = insts (zip bvs sks) t
                        s <- match t1 t' `wrapError` e
                        _ <- extSubst s
                        withCurrentSubst (Just e', ps1, t')
                      (Just t, Nothing) -> do
                        return (Nothing, [], t)
                      (Nothing, Just e1) -> do
                        (e', ps, t1) <- tcExp e1
                        return (Just e', ps, t1)
                      (Nothing, Nothing) ->
                        (Nothing, [],) <$> freshTyVar
      extEnv n (monotype tf)
      let e' = Let (Id n tf) (Just tf) me'
      withCurrentSubst (e', psf, unit)
tcStmt (StmtExp e)
  = do
      (e', ps', _) <- tcExp e
      pure (StmtExp e', ps', unit)
tcStmt (Return e)
  = do
      (e', ps, t) <- tcExp e
      pure (Return e', ps, t)
tcStmt (Match es eqns)
  = do
      (es', pss', ts') <- unzip3 <$> mapM tcExp es
      (eqns', pss1, resTy) <- tcEquations ts' eqns
      withCurrentSubst (Match es' eqns', concat (pss1 : pss'), resTy)
tcStmt (Asm yblk)
  = withLocalCtx yulPrimOps $ do
      (newBinds,t) <- tcYulBlock yblk
      let word' = monotype word
      mapM_ (flip extEnv word') newBinds
      pure (Asm yblk, [], t)


tcEquations :: [Ty] -> Equations Name -> TcM (Equations Id, [Pred], Ty)
tcEquations ts eqns
  = do
      resTy <- freshTyVar
      (eqns', ps, _) <- unzip3 <$> mapM (tcEquation resTy ts) eqns
      withCurrentSubst (eqns', concat ps, resTy)

tcEquation :: Ty -> [Ty] -> Equation Name -> TcM (Equation Id, [Pred], Ty)
tcEquation ret ts eqn@(ps, ss)
  = withLocalEnv do
      (ps', _, res) <- tcPats ts ps
      (ss', pss', t) <- withLocalCtx res (tcBody ss)
      s <- unify t ret `wrapError` eqn
      withCurrentSubst ((ps', ss'), pss', apply s t)

tcPats :: [Ty] -> [Pat Name] -> TcM ([Pat Id], [Ty], [(Name,Scheme)])
tcPats ts ps
  | length ts /= length ps = wrongPatternNumber ts ps
  | otherwise = do
      (ps', ts', ctxs) <- unzip3 <$> mapM (\(t, p) -> tcPat t p)
                                          (zip ts ps)
      pure (ps', ts', concat ctxs)

tcPat :: Ty -> Pat Name -> TcM (Pat Id, Ty, [(Name, Scheme)])
tcPat t (PVar n)
  = do
      let v = PVar (Id n t)
      pure (v, t, [(n, monotype t)])
tcPat t p@(PCon n ps)
  = do
      vs0 <- mapM (const freshTyVar) ps
      -- typing parameters
      (ps1, ts, lctxs) <- unzip3 <$> zipWithM tcPat vs0 ps
      -- asking type from environment
      st <- askEnv n `wrapError` p
      (_ :=> tc) <- freshInst st
      -- unifying the infered pattern type with constructor type
      s <- unify tc (funtype ts t) `wrapError` p
      let t' = apply s t
      tn <- typeName t'
      -- checking if it is a defined constructor
      checkConstr tn n
      -- building typing assumptions for introduced names
      let lctx' = map (\(n',t1) -> (n', apply s t1)) (concat lctxs)
      pure (PCon (Id n tc) ps1, t', apply s lctx')
tcPat t PWildcard
  = pure (PWildcard, t, [])
tcPat t' (PLit l)
  = do
      t <- tcLit l
      s <- unify t t'
      pure (PLit l, apply s t, [])

-- type inference for expressions

mkCon :: DataTy -> TcM (Exp Id, Ty)
mkCon (DataTy nt vs ((Constr n _) : _))
  = do
      mvs <- mapM (const freshTyVar) vs
      let
        t1 = TyCon nt mvs
      pure (Con (Id n t1) [], t1)
mkCon d = tcmError $ unlines ["Panic!!! This should not happen: mkCon", pretty d]

tcLit :: Literal -> TcM Ty
tcLit (IntLit _) = return word
tcLit (StrLit _) = return string

tcExp :: HasCallStack => Infer Exp
tcExp (Lit l)
  = do
      t <- tcLit l
      pure (Lit l, [], t)
tcExp (Var n)
  = do
      s <- askEnv n
      (ps :=> t) <- freshInst s
      noDesugarCalls <- getNoDesugarCalls
      if noDesugarCalls then pure (Var (Id n t), ps , t)
      else do
        -- checks if it is a function name, and return
        -- its corresponding unique type
        r <- lookupUniqueTy n
        p <- maybe (pure $ (Var (Id n t), t)) mkCon r
        withCurrentSubst (fst p, ps, snd p)
tcExp e@(Con n es)
  = do
      -- typing parameters
      (es', pss, ts) <- unzip3 <$> mapM tcExp es
      -- getting the type from the environment
      sch <- askEnv n `wrapError` e
      (ps :=> t) <- freshInst sch
      -- unifying inferred parameter types
      t' <- freshTyVar
      s <- unify (funtype ts t') t `wrapError` e
      tn <- typeName (apply s t')
      -- checking if the constructor belongs to type tn
      checkConstr tn n
      let ps' = concat (ps : pss)
          e1 = Con (Id n t) es'
      withCurrentSubst (e1, ps', t')
tcExp e@(FieldAccess Nothing n)
  = notImplementedS "tcExp" e

tcExp (FieldAccess (Just e) n)
  = do
      -- inferring expression type
      (e', ps,t) <- tcExp e
      -- getting type name
      tn <- typeName t
      -- getting field type
      s <- askField tn n
      (ps' :=> t') <- freshInst s
      withCurrentSubst (FieldAccess (Just e') (Id n t'), ps ++ ps', t')
tcExp ex@(Call me n args)
  = tcCall me n args `wrapError` ex
tcExp e@(Lam args bd _)
   = do
       (args', schs, ts') <- tcArgs args
       (bd', ps, t') <- withLocalCtx schs (tcBody bd)
       s <- getSubst
       let ps1 = apply s ps
           ts1 = apply s ts'
           t1 = apply s t'
           vs0 = mv ps1 `union` mv t1 `union` mv ts1
           vs = map (TVar . metaName) vs0
           ty = funtype ts1 t1
       noDesugarCalls <- getNoDesugarCalls
       if noDesugarCalls then withCurrentSubst (Lam args' bd' (Just t1), ps1, ty)
       else do
         (e, t) <- closureConversion vs (apply s args') (apply s bd') ps1 ty
         -- here we do not need the constraints in ps1, since after closure conversion
         -- the lambda is replaced by a unique type constructor for it.
         withCurrentSubst (e, [], t)
tcExp e1@(TyExp e ty)
  = do
      kindCheck ty `wrapError` e1
      (e', ps, ty') <- tcExp e
      s <- match ty' ty
      extSubst s
      withCurrentSubst (TyExp e' ty, ps, ty)

closureConversion :: [Tyvar] ->
                     [Param Id] ->
                     Body Id ->
                     [Pred] ->
                     Ty -> TcM (Exp Id, Ty)
closureConversion vs args bdy ps ty
  = do
      i <- incCounter
      fs <- Map.keys <$> gets uniqueTypes
      sch <- generalize (ps, ty)
      let
          fn = Name $ "lambda_impl" ++ show i
          argsn = map idName (vars args)
          defs = fs ++ argsn ++ Map.keys primCtx
          free = filter (\ x -> notElem (idName x) defs) (vars bdy)
      if null free then do
        -- no closure needed for monomorphic
        -- lambdas!
        --
        -- creating the lambda function by lifting it.
        fun1 <- createClosureFreeFun fn args bdy ps ty
        info [">> Creating lambda lifted function(free):\n", pretty fun1]
        sch <- generalize (ps, ty)
        -- creating the invoke instance and unique type def.
        (udt@(DataTy dn vs _), instd) <- generateDecls (fun1, sch)
        mvs <- mapM (const freshTyVar) vs
        let t = TyCon dn mvs
        -- updating the type inference state
        writeFunDef fun1
        writeDataTy udt
        checkDataType udt
        -- type checking generated instance
        checkInstance instd
        extEnv fn sch
        instd' <- tcInstance instd
        writeInstance instd'
        pure (Con (Id dn t) [], t)
      else do
        (cdt, e', t') <- createClosureType free vs ty
        addUniqueType fn cdt
        (fun, sch) <- createClosureFun fn free cdt args bdy ps ty
        info [">> Create lambda lifted function(closure):\n", pretty fun]
        writeFunDef fun
        writeDataTy cdt
        checkDataType cdt
        instd <- createInstance cdt fun sch
        checkInstance instd
        extEnv fn sch
        s <- getSubst
        clearSubst
        instd' <- tcInstance instd
        writeInstance instd'
        putSubst s
        pure (e', t')

createClosureType :: [Id] -> [Tyvar] -> Ty -> TcM (DataTy, Exp Id, Ty)
createClosureType ids vs ty
  = do
      i <- incCounter
      s <- getSubst
      let
          (args,ret) = splitTy ty
          argTy = tupleTyFromList args
          dn = Name $ "t_closure" ++ show i
          ts = map idType (apply s ids)
          ts' = everywhere (mkT gen) ts
          ns = map Var $ (apply s ids)
          vs' = nub $ (mv ts) ++ (map (MetaTv . var) vs)
          ty' = TyCon dn (Meta <$> vs')
          cid = Id dn (funtype ts ty')
          d = DataTy dn (map gvar vs') [Constr dn ts']
      info [">> Create closure type:", pretty d]
      pure (d, Con cid ns, ty')

createClosureFun :: Name ->
                    [Id] ->
                    DataTy ->
                    [Param Id] ->
                    Body Id ->
                    [Pred] ->
                    Ty ->
                    TcM (FunDef Id, Scheme)
createClosureFun fn free cdt args bdy ps ty
  = do
      j <- incCounter
      ct <- closureTyCon cdt
      let
          args0 = everywhere (mkT gen) args
          ps0  = everywhere (mkT gen) ps
          cName = Name $ "env" ++ show j
          cParam = Typed (Id cName ct) ct
          args' = cParam : args0
          (_,retTy) = splitTy ty
          vs' = union (bv ct) (bv ps0)
          ty' = ct :-> ty
          sig = Signature vs' ps0 fn args' (Just retTy)
      bdy' <- createClosureBody cName cdt free bdy
      sch <- generalize (ps0, ty')
      pure (everywhere (mkT gen) $ FunDef sig bdy', sch)



closureTyCon :: DataTy -> TcM Ty
closureTyCon (DataTy dn vs _)
  = pure (TyCon dn (TyVar <$> vs))

createClosureBody :: Name -> DataTy -> [Id] -> Body Id -> TcM (Body Id)
createClosureBody n cdt@(DataTy dn vs [Constr cn ts]) ids bdy
  = do
      ct <- closureTyCon cdt
      let ps = map PVar ids
          tc = funtype ts ct
      pure [Match [Var (Id n ct)] [([PCon (Id cn tc) ps], bdy)]]
createClosureBody _ cdt _ _ = "createClosureBody" `notImplemented` cdt

createClosureFreeFun :: Name ->
                        [Param Id] ->
                        Body Id ->
                        [Pred] ->
                        Ty ->
                        TcM (FunDef Id)
createClosureFreeFun fn args bdy ps ty
  = do
      let
        (_, retTy) = splitTy ty
        vs = bv ty `union` bv ps
        sig = Signature vs ps fn args (Just retTy)
      pure (everywhere (mkT gen) $ FunDef sig bdy)

tcArgs :: [Param Name] -> TcM ([Param Id], [(Name, Scheme)], [Ty])
tcArgs params
  = do
      (ps, schs, ts) <- unzip3 <$> mapM tcArg params
      pure (ps, schs, ts)

tcArg :: Param Name -> TcM (Param Id, (Name, Scheme), Ty)
tcArg (Untyped n)
  = do
      v <- freshTyVar
      let ty = monotype v
      pure (Typed (Id n v) v, (n, ty), v)
tcArg a@(Typed n ty)
  = do
      ty1 <- kindCheck ty `wrapError` a
      pure (Typed (Id n ty1) ty1, (n, monotype ty1), ty1)

hasAnn :: Signature Name -> Bool
hasAnn (Signature _ _ _ args rt)
  = any isAnn args || isJust rt
    where
      isAnn (Typed _ t) = True
      isAnn _ = False

-- boolean flag indicates if the assumption for the
-- function should be included in the context. It
-- is necessary to not include the type of instance
-- functions which should have the type of its underlying
-- type class definition.

tiArg :: Param Name -> TcM (Param Id, (Name, Scheme), Ty)
tiArg (Untyped n)
  = do
      t <- freshTyVar
      pure (Typed (Id n t) t, (n, monotype t), t)
tiArg (Typed n _)
  = do
      t <- freshTyVar
      pure (Typed (Id n t) t, (n, monotype t), t)

tiArgs :: [Param Name] -> TcM ([Param Id], [(Name, Scheme)], [Ty])
tiArgs args = unzip3 <$> mapM tiArg args

tiFunDef :: FunDef Name -> TcM (FunDef Id, Scheme, [Pred])
tiFunDef d@(FunDef sig@(Signature _ _ n args _) bd)
  = do
      -- getting fresh type variables for arguments
      (args', lctx, ts') <- tiArgs args
      -- fresh type for the function
      nt <- freshTyVar
      -- extended typing context for typing function body
      let lctx' = (n, monotype nt) : lctx
      -- typing function body
      (bd1, ps1, t1) <- withLocalCtx lctx' (tcBody bd) `wrapError` d
      -- unifying context introduced type with infered function type
      s <- unify nt (funtype ts' t1)
      -- building the function type scheme
      vs <- getEnvMetaVars
      (ds, rs) <- splitContext ps1 [] vs `wrapError` d
      ty <- withCurrentSubst nt
      sch <- generalize (rs, ty)
      -- checking ambiguity
      info [">>> Infered type for ", pretty n, " :: ", pretty sch]
      when (ambiguous sch) $ do
        ambiguousTypeError sch sig
      -- elaborating the type signature
      sig' <- elabSignature [] sig sch
      withCurrentSubst (FunDef sig' bd1, sch, ds)


argumentAnnotation :: Param Name -> TcM Ty
argumentAnnotation (Untyped _)
  = freshTyVar
argumentAnnotation (Typed _ t)
  = pure t

annotatedScheme :: [Tyvar] -> Signature Name -> TcM Scheme
annotatedScheme vs' sig@(Signature vs ps n args rt)
  = do
      ts <- mapM argumentAnnotation args
      t <- maybe freshTyVar pure rt
      -- check if all variables are bound in signature.
      when (any (\ v -> v `notElem` (vs ++ vs')) (bv sig)) $ do
         let unbound_vars = bv sig \\ (vs ++ vs')
         unboundTypeVars sig unbound_vars
      pure (Forall vs (ps :=> (funtype ts t)))


tcFunDef :: Bool -> [Tyvar] -> [Pred] -> FunDef Name -> TcM (FunDef Id, Scheme, [Pred])
tcFunDef incl vs' qs d@(FunDef sig@(Signature vs ps n args rt) bd)
  | hasAnn sig = do
      info ["\n# tcFunDef ", pretty sig]
      -- check if all variables are bound in signature.
      when (any (\ v -> v `notElem` (vs ++ vs')) (bv sig)) $ do
         let unbound_vars = bv sig \\ (vs ++ vs')
         unboundTypeVars sig unbound_vars
      -- instantiate signatures in function definition
      sks <- mapM (const freshTyVar) (vs ++ vs')
      let
          env = zip (vs' ++ vs) sks
          d1@(FunDef sig1@(Signature vs1 ps1 _ args1 rt1) bd1) = everywhere (mkT (insts @Ty env)) d
          qs1 = everywhere (mkT (insts @Ty env)) qs
      -- getting argument / return types in annotations
      (args', lctx, ts') <- tcArgs args1
      rt1' <- maybe freshTyVar pure rt1
      nt <- freshTyVar
      -- building the typing context with new assumptions
      let lctx' = if incl then (n, monotype nt) : lctx else lctx
      -- typing function body
      (bd1', ps1', t1') <- withLocalCtx lctx' (tcBody bd1) `wrapError` d
      unify rt1' t1'
      unify nt (funtype ts' rt1')
      -- building the function type scheme
      free <- getEnvMetaVars
      (ds, rs) <- splitContext ps1' (ps1 ++ qs1) free
      info [" - splitContext retained: ", prettys rs]
      ty <- withCurrentSubst nt
      inf <- generalize (rs, ty)
      info [" - generalized inferred type: ", pretty inf]
      ann <- annotatedScheme vs' sig
     -- checking subsumption
      subsCheck sig inf ann `wrapError` d
      -- elaborating function body
      fdt <- elabFunDef vs' sig1 bd1' inf ann
      withCurrentSubst (fdt, ann, [])
  | otherwise = do
        (fd2, sch, ds) <- tiFunDef d
        -- infered constraints cannot be defered.
        unless (null ds) $ do
            (tcmError $ unlines [ "Cannot entail:"
                                , pretty ds
                                , "from"
                                , pretty sig]) `wrapError` d
        pure (fd2, sch, [])
-- elaborating function definition

elabFunDef :: [Tyvar] -> -- additional variables which came from outer scope
              Signature Name -> -- original function signature
              Body Id ->  -- elaborated function body (with fresh variables)
              Scheme -> -- function infered type
              Scheme -> -- function annotated type
              TcM (FunDef Id)
elabFunDef vs sig bdy inf@(Forall _ (_ :=> tinf)) ann@(Forall _ (_ :=> tann))
  = do
      let
        tinf' = everywhere (mkT toMeta) tinf
        tann' = everywhere (mkT toMeta) tann
      s <- unify tinf' tann'
      sig2 <- elabSignature vs sig ann
      let fd2 = everywhere (mkT (apply @Ty s)) (FunDef sig2 bdy)
      pure (everywhere (mkT gen) fd2)

toMeta :: Ty -> Ty
toMeta (TyVar (TVar n)) = Meta (MetaTv n)
toMeta (TyCon n ts) = TyCon n (map toMeta ts)
toMeta t = t

-- testing ambiguity

ambiguous :: Scheme -> Bool
ambiguous (Forall vs (ps :=> t))
  = not $ null $ bv ps \\ bv (closure ps (bv t))

reachable :: [Pred] -> [Tyvar] -> [Pred]
reachable ps vs
  = [p | p <- ps, disjunct (bv p) vs]

closure :: [Pred] -> [Tyvar] -> [Pred]
closure ps vs
  | subset (bv $ reachable ps vs) vs = reachable ps vs
  | otherwise = closure ps (bv (reachable ps vs))

subset :: Eq a => [a] -> [a] -> Bool
subset xs ys = all (\ x -> x `elem` ys) xs

disjunct :: Eq a => [a] -> [a] -> Bool
disjunct xs ys = not $ null $ intersect xs ys

-- only invokable constraints can be inserted freely

isValid :: [Pred] -> Bool
isValid rs = null rs || all isInvoke rs
  where
    isInvoke (InCls n _ _)
      = n == invokableName
    isInvoke _ = False

-- update types in signature

elabSignature :: [Tyvar] -> Signature Name -> Scheme -> TcM (Signature Id)
elabSignature vs1 sig sch@(Forall vs (ps :=> t))
  = do
      let
        params = sigParams sig
        nparams = length params
        (ts, t') = splitTy t
        (ts', rs) = splitAt nparams ts
        ctx = sigContext sig
      params' <- zipWithM elabParam ts' params
      let
        -- here we build the return type.
        -- Note that, since we can return functions, we need to check if the
        -- formal parameters are present in the signature.
        ret = Just $ if null params' then t else (funtype rs t')
        vs' = bv params' `union` bv ret `union` bv ps
      sig2 <- withCurrentSubst (Signature (vs' \\ vs1) ps (sigName sig) params' ret)
      pure sig2

elabParam :: Ty -> Param Name -> TcM (Param Id)
elabParam t p@(Typed n _) = pure $ Typed (Id n t) t
elabParam t (Untyped n) = pure $ Typed (Id n t) t

annotateSignature :: Scheme -> Signature Name -> TcM (Signature Name)
annotateSignature (Forall vs (ps :=> t)) sig
  = pure $ Signature vs ps (sigName sig) params' ret
    where
      (ts,t') = splitTy t
      params' = zipWith annotateParam ts (sigParams sig)
      ret = Just t'

annotateParam :: Ty -> Param Name -> Param Name
annotateParam t (Typed n _) = Typed n t
annotateParam t (Untyped n) = Typed n t

-- qualify name for contract functions

correctName :: Name -> TcM Name
correctName n@(QualName _ _) = pure n
correctName (Name s)
  = do
      c <- gets contract
      if isJust c then pure (QualName (fromJust c) s)
        else pure (Name s)

extSignature :: Signature Name -> TcM ()
extSignature sig@(Signature _ preds n ps t)
  = do
      te <- gets directCalls
      -- checking if the function is previously defined
      when (n `elem` te) (duplicatedFunDef n) `wrapError` sig
      addFunctionName n

-- typing instance

tcInstance :: Instance Name -> TcM (Instance Id)
tcInstance idecl@(Instance d vs ctx n ts t funs)
  = do
      vs' <- mapM (const freshVar) vs
      let env = zip vs (map Meta vs')
          idecl'@(Instance _ _ ctx' _ ts' t' funs')
            = everywhere (mkT (insts @Ty env)) idecl
      tcInstance' (Instance d [] ctx' n ts' t' funs')

tcInstance' :: Instance Name -> TcM (Instance Id)
tcInstance' idecl@(Instance d vs ctx n ts t funs)
  = do
      checkCompleteInstDef n (map (sigName . funSignature) funs)
      funs' <- buildSignatures n ts t funs `wrapError` idecl
      (funs1, schss, pss') <- unzip3 <$> mapM (tcFunDef False vs ctx) funs' `wrapError` idecl
      instd <- withCurrentSubst (Instance d vs ctx n ts t funs1)
      let
        ind@(Instance _ _ ctx' _ ts' t' funs2) = everywhere (mkT gen) instd
        vs1 = bv ind
        funs3 = map (remVars vs1) funs2
      pure (Instance d vs1 ctx' n ts' t' funs3)

remVars :: [Tyvar] -> FunDef Id -> FunDef Id
remVars vs' (FunDef (Signature vs ps n args rt) bd)
  = FunDef (Signature (vs \\ vs') ps n args rt) bd

checkDeferedConstraints :: [(FunDef Id, [Pred])] -> TcM ()
checkDeferedConstraints = mapM_ checkDeferedConstraint
  where
    checkDeferedConstraint (fd, ps)
      = unless (null ps) $ tcmError $ unlines ["Cannot satisfy:"
                                              , pretty ps
                                              , "from:"
                                              , if null ctx then "<empty context>" else pretty ctx
                                              , "in:"
                                              , pretty sig
                                              ]
       where
        sig = funSignature fd
        ctx = sigContext sig


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
      let vs = bv cpred
      mvs <- mapM (const freshTyVar) vs
      sm <- match (insts (zip vs mvs) cpred) (InCls n t ts)
      let qname m = QualName n (pretty m)
      schs <- mapM (askEnv . qname . sigName . funSignature) funs
      let
          app (Forall _ qt) = apply sm (insts (zip vs mvs) qt)
          tinsts = map app schs
      zipWithM (buildSignature n) tinsts funs

buildSignature :: Name -> Qual Ty -> FunDef Name -> TcM (FunDef Name)
buildSignature n (ps :=> t) (FunDef sig bd)
  = do
      let (args, ret) = splitTy t
          sig' = typeSignature n args ret ps sig
      pure (FunDef sig' bd)

typeSignature :: Name -> [Ty] -> Ty -> [Pred] -> Signature Name -> Signature Name
typeSignature nm args ret ps sig
  = sig {
          sigName = QualName nm (pretty $ sigName sig)
        , sigContext = sigContext sig
        , sigParams = zipWith paramType args (sigParams sig)
        , sigReturn = Just ret
        }
    where
      paramType _ (Typed n t) = Typed n t
      paramType t (Untyped n) = Typed n t

schemeFromSignature :: Pretty a => Signature a -> Qual Ty
schemeFromSignature (Signature vs ps n args ret)
  = ps :=> (funtype (map (\ (Typed _ t) -> t) args) (fromJust ret))
schemeFromSignature sig = notImplemented "schemeFromSignature" sig

-- checking instances and adding them in the environment

checkInstances :: [Instance Name] -> TcM ()
checkInstances = mapM_ checkInstance

checkInstance :: Instance Name -> TcM ()
checkInstance idef@(Instance d vs ctx n ts t funs)
  = do
      let ipred = InCls n t ts
      -- checking the coverage condition
      insts <- askInstEnv n `wrapError` ipred
      -- check overlapping only for non-default instances
      unless d (checkOverlap ipred insts `wrapError` idef)
      -- check if default instance has a type variable as main argument.
      when d (checkDefaultInst (ctx :=> ipred) `wrapError` idef)
      coverage <- askCoverage n
      unless coverage (checkCoverage n ts t `wrapError` idef)
      -- checking Patterson condition
      patterson <- askPattersonCondition n
      unless patterson (checkMeasure ctx ipred `wrapError` idef)
      -- checking bound variable condition
      bound <- askBoundVariableCondition n
      unless bound (checkBoundVariable ctx (fv (t : ts)) `wrapError` idef)
      -- checking instance methods
      mapM_ (checkMethod ipred) funs `wrapError` idef
      let ninst = anfInstance $ ctx :=> InCls n t ts
      -- add to the environment
      if d then addDefaultInstance n ninst
      else addInstance n ninst

-- checking a default instance

checkDefaultInst :: Qual Pred -> TcM ()
checkDefaultInst p@(ps :=> InCls n t ts)
  = unless (isTyVar t) (invalidDefaultInst p)

isTyVar :: Ty -> Bool
isTyVar (TyVar _) = True
isTyVar _ = False

-- bound variable check

checkBoundVariable :: [Pred] -> [Tyvar] -> TcM ()
checkBoundVariable ps vs
  = unless (all (\ v -> v `elem` vs) (fv ps)) $ do
      throwError "Bounded variable condition fails!"


checkOverlap :: Pred -> [Inst] -> TcM ()
checkOverlap _ [] = pure ()
checkOverlap p@(InCls _ t _) (i:is)
  = do
        i' <- freshInst i
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
      let strongTvs = bv t
          weakTvs = bv ts
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
      sch <- askEnv qn `wrapError` d
      (qs :=> ty) <- freshInst sch
      p <- maybeToTcM (unwords [ "Constraint for"
                               , show n
                               , "not found in type of"
                               , show $ sigName sig])
                      (findPred n qs)
      -- matching substitution of instance head and class predicate
      _ <- liftEither (match p ih) `wrapError` d
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


-- type generalization

generalize :: ([Pred], Ty) -> TcM Scheme
generalize (ps,t)
  = do
      envVars <- getEnvMetaVars
      (ps1,t1) <- withCurrentSubst (ps,t)
      let
          vs = map gvar $ mv (ps1,t1) \\ envVars
          sch = Forall vs (everywhere (mkT gen) $ ps1 :=> t1)
      return sch

-- kind check

kindCheck :: Ty -> TcM Ty
kindCheck (t1 :-> t2)
  = (:->) <$> kindCheck t1 <*> kindCheck t2
kindCheck t@(TyCon n ts)
  = do
      ti <- askTypeInfo n `wrapError` t
      unless (n == Name "pair" || arity ti == length ts) $
        throwError $ unlines [ "Invalid number of type arguments!"
                             , "Type " ++ pretty n ++ " is expected to have " ++
                               show (arity ti) ++ " type arguments"
                             , "but, type " ++ pretty t ++
                               " has " ++ (show $ length ts) ++ " arguments"]
      mapM_ kindCheck ts
      pure t
kindCheck t = pure t



tcBody :: Body Name -> TcM (Body Id, [Pred], Ty)
tcBody [] = pure ([], [], unit)
tcBody [s]
  = do
      (s', ps', t') <- tcStmt s
      pure ([s'], ps', t')
tcBody (Return _ : _)
  = throwError "Illegal return statement"
tcBody (s : ss)
  = do
      (s', ps', t') <- tcStmt s
      (bd', ps1, t1) <- tcBody ss
      pure (s' : bd', ps' ++ ps1, t1)

tcCall :: Maybe (Exp Name) -> Name -> [Exp Name] -> TcM (Exp Id, [Pred], Ty)
tcCall Nothing n args
  = do
      s <- askEnv n
      (ps :=> t) <- freshInst s
      t' <- freshTyVar
      (es', pss', ts') <- unzip3 <$> mapM tcExp args
      s' <- unify t (funtype ts' t')
      extSubst s'
      let ps' = foldr union [] (ps : pss')
          t1 = funtype ts' t'
      withCurrentSubst (Call Nothing (Id n t1) es', ps', t')
tcCall (Just e) n args
  = do
      (e', ps , ct) <- tcExp e
      s <- askEnv n
      (ps1 :=> t) <- freshInst s
      t' <- freshTyVar
      (es', pss', ts') <- unzip3 <$> mapM tcExp args
      s' <- unify (foldr (:->) t' ts') t
      let ps' = foldr union [] ((ps ++ ps1) : pss')
      withCurrentSubst (Call (Just e') (Id n t') es', ps', t')

tcParam :: Param Name -> TcM (Param Id)
tcParam (Typed n t)
  = pure $ Typed (Id n t) t
tcParam (Untyped n)
  = do
      t <- freshTyVar
      pure (Typed (Id n t) t)

typeName :: Ty -> TcM Name
typeName (TyCon n _) = pure n
typeName t = throwError $ unlines ["Expected type, but found:"
                                  , pretty t
                                  ]

-- typing Yul code

tcYulBlock :: YulBlock -> TcM ([Name], Ty)
tcYulBlock []
  = pure ([], unit)
tcYulBlock [s]
  = tcYulStmt s
tcYulBlock (s : ss)
  = do
      (ns,_) <- tcYulStmt s
      (nss, t) <- tcYulBlock ss
      pure (ns ++ nss, t)

tcYulStmt :: YulStmt -> TcM ([Name], Ty)
tcYulStmt (YAssign ns e)
  = do
      -- do not define names
      tcYulExp e
      pure ([], unit)
tcYulStmt (YBlock yblk)
  = do
      _ <- tcYulBlock yblk
      -- names defined in should not return
      pure ([], unit)
tcYulStmt (YLet ns (Just e))
  = do
      tcYulExp e
      mapM_ (flip extEnv mword) ns
      pure (ns, unit)
tcYulStmt (YExp e)
  = do
      t <- tcYulExp e
      pure ([], t)
tcYulStmt (YIf e yblk)
  = do
      tcYulExp e
      _ <- tcYulBlock yblk
      pure ([], unit)
tcYulStmt (YSwitch e cs df)
  = do
      tcYulExp e
      tcYulCases cs
      tcYulDefault df
      pure ([], unit)
tcYulStmt (YFor init e bdy upd)
  = do
      ns <- fst <$> tcYulBlock init
      _ <- withLocalEnv do
              mapM_ (flip extEnv mword) ns
              tcYulExp e
              tcYulBlock bdy
              tcYulBlock upd
      pure ([], unit)
tcYulStmt _ = pure ([], unit)

tcYulExp :: YulExp -> TcM Ty
tcYulExp (YLit l)
  = tcYLit l
tcYulExp (YIdent v)
  = do
      sch <- askEnv v
      -- writeln $ unwords ["! tcYulExp/YIdent: ", pretty v, "::", pretty sch]
      (_ :=> t) <- freshInst sch
      unless (t == word) (invalidYulType v t)
      pure t
tcYulExp (YCall n es)
  = do
      sch <- askEnv n
      (_ :=> t) <- freshInst sch
      ts <- mapM tcYulExp es
      t' <- freshTyVar
      unless (all (== word) ts) (invalidYulType n t)
      unify t (foldr (:->) t' ts)
      withCurrentSubst t'

tcYLit :: YLiteral -> TcM Ty
tcYLit (YulString _) = return string
tcYLit (YulNumber _) = return word
tcYLit lit = notImplemented "tcYLit" lit

tcYulCases :: YulCases -> TcM ()
tcYulCases = mapM_ tcYulCase

tcYulCase :: YulCase -> TcM ()
tcYulCase (_,yblk)
  = do
      tcYulBlock yblk
      return ()

tcYulDefault :: Maybe YulBlock -> TcM ()
tcYulDefault (Just b)
  = do
      _ <- tcYulBlock b
      pure ()
tcYulDefault Nothing = pure ()

mword :: Scheme
mword = monotype word

-- determining free variables

class Vars a where
  vars :: a -> [Id]

instance Vars a => Vars [a] where
  vars = foldr (union . vars) []

instance Vars Id where
  vars n = [n]

instance Vars a => Vars (Pat a) where
  vars (PVar v) = vars v
  vars (PCon _ ps) = vars ps
  vars _ = []

instance Vars a => Vars (Param a) where
  vars (Typed n _) = vars n
  vars (Untyped n) = vars n

instance Vars a => Vars (Stmt a) where
  vars (e1 := e2) = vars [e1,e2]
  vars (Let _ _ (Just e)) = vars e
  vars (Let _ _ _) = []
  vars (StmtExp e) = vars e
  vars (Return e) = vars e
  vars (Match e eqns) = vars e `union` vars eqns

instance Vars a => Vars (Equation a) where
  vars (_, ss) = vars ss

instance Vars a => Vars (Exp a) where
  vars (Var n) = vars n
  vars (Con _ es) = vars es
  vars (FieldAccess Nothing _) = []
  vars (FieldAccess (Just e) _) = vars e
  vars (Call (Just e) n es) = vars n `union` vars (e : es)
  vars (Call Nothing n es) = vars n `union` vars es
  vars (Lam ps bd _) = vars bd \\ vars ps
  vars _ = []

-- rename type variables

rename :: Ty -> Ty
rename t = let vs = bv t
               s = zip vs (map (TyVar . TVar) namePool)
           in insts s t

-- errors

unboundTypeVars :: Pretty a => a -> [Tyvar] -> TcM b
unboundTypeVars sig vs
  = throwError $ unlines ["Type variables:", vs', "are unbound in:", pretty sig]
  where
    vs' = unwords $ map pretty vs

typeMatch :: Scheme -> Scheme -> TcM ()
typeMatch t1 t2
  = unless (t1 == t2) $
      throwError $ unwords ["Types"
                           , pretty t1
                           , "and"
                           , pretty t2
                           , "do not match"
                           ]

invalidYulType :: Name -> Ty -> TcM a
invalidYulType (Name n) ty
  = throwError $ unlines ["Yul values can only be of word type:", unwords[n,":", pretty ty]]

expectedFunction :: Ty -> TcM a
expectedFunction t
  = throwError $ unlines ["Expected function type. Found:"
                         , pretty t
                         ]

wrongPatternNumber :: [Ty] -> [Pat Name] -> TcM a
wrongPatternNumber qts ps
  = throwError $ unlines [ "Wrong number of patterns in:"
                         , unwords (map pretty ps)
                         , "expected:"
                         , show (length qts)
                         , "patterns"]

duplicatedFunDef :: Name -> TcM ()
duplicatedFunDef n
  = throwError $ "Duplicated function definition:" ++ pretty n

entailmentError :: [Pred] -> [Pred] -> TcM ()
entailmentError base nonentail
  = tcmError $ unwords [ "Could not deduce"
                         , pretty nonentail
                         , "from"
                         , if null base then "<empty context>" else pretty base
                         ]

rigidVariableError :: [(Tyvar, Ty)] -> TcM ()
rigidVariableError vts
  = tcmError $ "Cannot unify the following rigid variables with types:" ++
                (unlines $ map (\ (v,t) -> pretty v ++ " with " ++ pretty t) vts)

invalidDefaultInst :: Inst -> TcM ()
invalidDefaultInst p
  = tcmError $ "Cannot have a default instance with a non-type variable as main argument:" ++ pretty p

ambiguousTypeError :: Scheme -> Signature Name -> TcM ()
ambiguousTypeError sch sig
  = tcmError $ unlines ["Ambiguous infered type", pretty sch, "in", pretty sig]

notImplemented :: (HasCallStack, Pretty a) => String -> a -> b
notImplemented funName a = error $ concat [funName, " not implemented yet for ", pretty a]

notImplementedS :: (HasCallStack, Show a) => String -> a -> b
notImplementedS funName a = error $ concat [funName, " not implemented yet for ", show(pShow a)]
