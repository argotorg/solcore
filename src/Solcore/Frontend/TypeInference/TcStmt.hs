module Solcore.Frontend.TypeInference.TcStmt where

import Common.Pretty
import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Data.Generics hiding (Constr)
import Data.List
import Data.Map qualified as Map
import Data.Maybe
import Data.Set qualified as Set
import GHC.Stack
import Language.Yul
import Solcore.Frontend.Pretty.ShortName
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.InvokeGen
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcSimplify
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives

-- type inference for statements

type Infer f = f Name -> TcM (f Id, [Pred], Ty)

tcStmt :: Infer Stmt
tcStmt = tcStmtWithExpectedReturn Nothing

tcStmtWithExpectedReturn :: Maybe Ty -> Infer Stmt
tcStmtWithExpectedReturn _ e@(lhs := rhs) =
  do
    (lhs1, ps1, t1) <- tcExp lhs
    s0 <- getSubst
    let expectedRhsTy = apply s0 t1
    (rhs1, ps2, t2) <- tcExpWithExpected (Just expectedRhsTy) rhs
    s <- unify t1 t2 `wrapError` e
    _ <- extSubst s
    pure (lhs1 := rhs1, apply s $ ps1 ++ ps2, unit)
tcStmtWithExpectedReturn _ e@(Let n mt me) =
  do
    (me', psf, tf) <- case (mt, me) of
      (Just t, Just e1) -> do
        t2 <- kindCheck t `wrapError` e
        let bvs = bv t2
        sks <- mapM (const freshTyVar) bvs
        let t' = insts (zip bvs sks) t2
        (e', ps1, t1) <- tcExpWithExpected (Just t') e1
        s <- tcmMatch t1 t' `wrapError` e
        _ <- extSubst s
        withCurrentSubst (Just e', ps1, t')
      (Just t, Nothing) -> do
        return (Nothing, [], t)
      (Nothing, Just e1) -> do
        (e', ps, t1) <- tcExp e1
        return (Just e', ps, t1)
      (Nothing, Nothing) ->
        (Nothing,[],) <$> freshTyVar
    extEnv n (monotype tf)
    let e' = Let (Id n tf) (Just tf) me'
    withCurrentSubst (e', psf, unit)
tcStmtWithExpectedReturn _ (StmtExp e) =
  do
    (e', ps', _) <- tcExp e
    pure (StmtExp e', ps', unit)
tcStmtWithExpectedReturn mExpectedReturn (Return e) =
  do
    (e', ps, t) <- tcExpWithExpected mExpectedReturn e
    pure (Return e', ps, t)
tcStmtWithExpectedReturn mExpectedReturn (Match es eqns) =
  do
    (es', pss', ts') <- unzip3 <$> mapM tcExp es
    ensureVisiblePatternCoverage ts' eqns
    (eqns', pss1, resTy) <- tcEquationsWithExpectedReturn mExpectedReturn ts' eqns
    withCurrentSubst (Match es' eqns', concat (pss1 : pss'), resTy)
tcStmtWithExpectedReturn _ (Asm yblk) =
  withLocalCtx yulPrimOps $ do
    (newBinds, t) <- tcYulBlock yblk
    let word' = monotype word
    mapM_ (flip extEnv word') newBinds
    pure (Asm yblk, [], t)
tcStmtWithExpectedReturn mExpectedReturn s@(If e blk1 blk2) =
  do
    (e', ps, t) <- tcExp e
    -- condition should have the boolean type
    _ <-
      unify t boolTy
        `catchError` ( \_ ->
                         tcmError $
                           unlines
                             [ "Expression:",
                               pretty e,
                               "has type:",
                               pretty t,
                               "while it is expected to have type:",
                               pretty boolTy
                             ]
                     )
        `wrapError` s
    (blk1', ps1, t1) <- tcBodyWithExpectedReturn mExpectedReturn blk1
    (blk2', ps2, t2) <- tcBodyWithExpectedReturn mExpectedReturn blk2
    -- here we check if "else" branch is present.
    let t2' = if null blk2 then t1 else t2
        ps3 = ps ++ ps1 ++ ps2
    -- we force that both blocks should return the same type.
    _ <-
      unify t1 t2'
        `catchError` ( \_ ->
                         tcmError $
                           unlines
                             [ "If blocks should produce the same return type but, block:",
                               pretty blk1,
                               "has return type:",
                               pretty t1,
                               "while block:",
                               pretty blk2,
                               "has return type:",
                               pretty t2'
                             ]
                     )
        `wrapError` s
    withCurrentSubst (If e' blk1' blk2', ps3, t1)

tcEquations :: [Ty] -> Equations Name -> TcM (Equations Id, [Pred], Ty)
tcEquations = tcEquationsWithExpectedReturn Nothing

tcEquationsWithExpectedReturn :: Maybe Ty -> [Ty] -> Equations Name -> TcM (Equations Id, [Pred], Ty)
tcEquationsWithExpectedReturn mExpectedReturn ts eqns =
  do
    resTy <- freshTyVar
    (eqns', ps, _) <- unzip3 <$> mapM (tcEquationWithExpectedReturn mExpectedReturn resTy ts) eqns
    withCurrentSubst (eqns', concat ps, resTy)

tcEquation :: Ty -> [Ty] -> Equation Name -> TcM (Equation Id, [Pred], Ty)
tcEquation = tcEquationWithExpectedReturn Nothing

tcEquationWithExpectedReturn :: Maybe Ty -> Ty -> [Ty] -> Equation Name -> TcM (Equation Id, [Pred], Ty)
tcEquationWithExpectedReturn mExpectedReturn ret ts eqn@(ps, ss) =
  withLocalEnv do
    (ps', _, res) <- tcPats ts ps
    (ss', pss', t) <- withLocalCtx res (tcBodyWithExpectedReturn mExpectedReturn ss)
    s <- unify t ret `wrapError` eqn
    withCurrentSubst ((ps', ss'), pss', apply s t)

ensureVisiblePatternCoverage :: [Ty] -> Equations Name -> TcM ()
ensureVisiblePatternCoverage scrutineeTys eqns =
  mapM_ checkScrutinee (zip [0 ..] scrutineeTys)
  where
    checkScrutinee (index, scrutineeTy) = do
      scrutineeTy' <- maybeExpandSynonym scrutineeTy
      case scrutineeTy' of
        TyCon scrutineeTypeName _ -> do
          isPartial <- isPartialDataType scrutineeTypeName
          when (isPartial && not (hasCatchAllAt index eqns)) $
            throwError $
              unlines
                [ "Pattern match on type with hidden constructors requires a wildcard arm:",
                  pretty scrutineeTypeName
                ]
        _ ->
          pure ()

    hasCatchAllAt index =
      any hasCatchAllInEquation
      where
        hasCatchAllInEquation (patterns, _) =
          case drop index patterns of
            (patternAtIndex : _) -> isCatchAllPattern patternAtIndex
            [] -> False

    isCatchAllPattern (PVar _) = True
    isCatchAllPattern PWildcard = True
    isCatchAllPattern _ = False

tcPats :: [Ty] -> [Pat Name] -> TcM ([Pat Id], [Ty], [(Name, Scheme)])
tcPats ts ps
  | length ts /= length ps = wrongPatternNumber ts ps
  | otherwise = do
      (ps', ts', ctxs) <-
        unzip3
          <$> mapM
            (\(t, p) -> tcPat t p)
            (zip ts ps)
      pure (ps', ts', concat ctxs)

tcPat :: Ty -> Pat Name -> TcM (Pat Id, Ty, [(Name, Scheme)])
tcPat t (PVar n) =
  do
    let v = PVar (Id n t)
    pure (v, t, [(n, monotype t)])
tcPat t p@(PCon n ps) =
  do
    n' <- resolvePatternConstructor n t `wrapError` p
    -- asking type from environment
    st <- askEnv n' `wrapError` p
    (_ :=> tc) <- freshInst st
    let (argTys, resultTy) = splitTy tc
    when (length argTys /= length ps) $
      throwError $
        unlines
          [ "Wrong number of pattern arguments for constructor:",
            pretty n',
            "expected:",
            show (length argTys),
            "arguments"
          ]
    -- Refine argument expectations first so nested dot-shorthand patterns
    -- can resolve against the constructor result type context.
    _ <- unify resultTy t `wrapError` p
    argTys' <- withCurrentSubst argTys
    -- typing parameters
    (ps1, ts, lctxs) <- unzip3 <$> zipWithM tcPat argTys' ps
    -- unifying the infered pattern type with constructor type
    s <- unify tc (funtype ts t) `wrapError` p
    let t' = apply s t
    tn <- typeName t'
    -- checking if it is a defined constructor
    checkConstr tn n'
    -- building typing assumptions for introduced names
    let lctx' = map (\(boundName, t1) -> (boundName, apply s t1)) (concat lctxs)
    pure (PCon (Id n' tc) ps1, t', apply s lctx')
tcPat t PWildcard =
  pure (PWildcard, t, [])
tcPat t' (PLit l) =
  do
    t <- tcLit l
    s <- unify t t'
    pure (PLit l, apply s t, [])

-- type inference for expressions

mkCon :: DataTy -> TcM (Exp Id, Ty)
mkCon (DataTy nt vs ((Constr n _) : _)) =
  do
    mvs <- mapM (const freshTyVar) vs
    let t1 = TyCon nt mvs
    pure (Con (Id n t1) [], t1)
mkCon d = tcmError $ unlines ["Panic!!! This should not happen: mkCon", pretty d]

tcLit :: Literal -> TcM Ty
tcLit (IntLit _) = return word
tcLit (StrLit _) = return string

tcExp :: (HasCallStack) => Infer Exp
tcExp = tcExpWithExpected Nothing

tcExpWithExpected :: (HasCallStack) => Maybe Ty -> Exp Name -> TcM (Exp Id, [Pred], Ty)
tcExpWithExpected _ (Lit l) =
  do
    t <- tcLit l
    pure (Lit l, [], t)
tcExpWithExpected _ (Var n) =
  do
    s <- askEnv n `wrapError` (Var n)
    (ps :=> t) <- freshInst s
    noDesugarCalls <- getNoDesugarCalls
    if noDesugarCalls
      then pure (Var (Id n t), ps, t)
      else do
        -- checks if it is a function name, and return
        -- its corresponding unique type
        r <- lookupUniqueTy n
        p <- maybe (pure $ (Var (Id n t), t)) mkCon r
        withCurrentSubst (fst p, ps, snd p)
tcExpWithExpected mExpected e@(Con n es) =
  do
    expectedArgTys <- mapM (const freshTyVar) es
    n' <- resolveExpressionConstructor n expectedArgTys mExpected `wrapError` e
    -- getting the type from the environment
    sch <- askEnv n' `wrapError` e
    (ps :=> t) <- freshInst sch
    t' <- freshTyVar
    s0 <- unify t (funtype expectedArgTys t') `wrapError` e
    _ <- extSubst s0
    case mExpected of
      Just expectedTy -> do
        expectedTy' <- maybeExpandSynonym expectedTy
        sExpected <- unify t' expectedTy' `wrapError` e
        _ <- extSubst sExpected
        pure ()
      Nothing ->
        pure ()
    expectedArgTys' <- withCurrentSubst expectedArgTys
    -- typing parameters with expected constructor argument types
    (es', pss, ts) <-
      unzip3
        <$> zipWithM
          (\arg expectedTy -> tcExpWithExpected (Just expectedTy) arg)
          es
          expectedArgTys'
    -- unifying inferred parameter types
    s <- unify (funtype ts t') t `wrapError` e
    _ <- extSubst s
    -- expand synonyms before extracting type name
    t'' <- maybeExpandSynonym (apply s t')
    tn <- typeName t''
    -- checking if the constructor belongs to type tn
    checkConstr tn n'
    let ps' = concat (ps : pss)
        e1 = Con (Id n' t) es'
    withCurrentSubst (e1, ps', t')
tcExpWithExpected _ e@(FieldAccess Nothing _) =
  -- = notImplementedS "tcExp" e
  throwError ("tcExp not implemented for: " ++ pretty e ++ "\n" ++ show e)
tcExpWithExpected _ (FieldAccess (Just e) n) =
  do
    -- inferring expression type
    (e', ps, t) <- tcExpWithExpected Nothing e
    -- expand synonyms before extracting type name
    tExp <- maybeExpandSynonym t
    tn <- typeName tExp
    -- getting field type
    s <- askField tn n
    (ps' :=> t') <- freshInst s
    withCurrentSubst (FieldAccess (Just e') (Id n t'), ps ++ ps', t')
tcExpWithExpected _ ex@(Call me n args) =
  tcCall me n args `wrapError` ex
tcExpWithExpected _ (Lam args bd _) =
  do
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
    if noDesugarCalls
      then withCurrentSubst (Lam args' bd' (Just t1), ps1, ty)
      else do
        (exp1, t) <- closureConversion vs (apply s args') (apply s bd') ps1 ty
        withCurrentSubst (exp1, ps1, t)
tcExpWithExpected _ e1@(TyExp e ty) =
  do
    ty1 <- kindCheck ty `wrapError` e1
    (e', ps, ty') <- tcExpWithExpected (Just ty1) e
    s <- tcmMatch ty' ty1
    _ <- extSubst s
    withCurrentSubst (TyExp e' ty1, ps, ty1)
tcExpWithExpected _ e@(Cond e1 e2 e3) =
  do
    (e1', ps1, t1) <- tcExpWithExpected Nothing e1 `wrapError` e
    (e2', ps2, t2) <- tcExpWithExpected Nothing e2 `wrapError` e
    (e3', ps3, t3) <- tcExpWithExpected Nothing e3 `wrapError` e
    -- condition should have the boolean type
    _ <-
      unify t1 boolTy
        `catchError` ( \_ ->
                         tcmError $
                           unlines
                             [ "Expression:",
                               pretty e1,
                               "has type:",
                               pretty t1,
                               "while it is expected to have type:",
                               pretty boolTy
                             ]
                     )
        `wrapError` e
    -- we force that both blocks should return the same type.
    _ <-
      unify t2 t3
        `catchError` ( \_ ->
                         tcmError $
                           unlines
                             [ "Conditional expressions should produce the same return type, but:",
                               pretty e2,
                               "has return type:",
                               pretty t2,
                               "while:",
                               pretty e3,
                               "has return type:",
                               pretty t3
                             ]
                     )
        `wrapError` e
    withCurrentSubst (Cond e1' e2' e3', ps1 ++ ps2 ++ ps3, t2)
tcExpWithExpected _ e@(Indexed arrExp idx) =
  do
    (arr', psArr, tArr) <- tcExp arrExp `wrapError` e
    (idx', psIdx, tIdx) <- tcExp idx `wrapError` e
    tRes <- freshTyVar
    s <- unify tArr (tIdx :-> tRes) `wrapError` e
    withCurrentSubst (Indexed arr' idx', psArr ++ psIdx, apply s tRes)

closureConversion ::
  [Tyvar] ->
  [Param Id] ->
  Body Id ->
  [Pred] ->
  Ty ->
  TcM (Exp Id, Ty)
closureConversion vs args bdy ps ty =
  do
    i <- incCounter
    fs <- Map.keys <$> gets uniqueTypes
    ps' <- reduce [] ps
    let fn = Name $ "lambda_impl" ++ show i
        argsn = map idName $ bound args ++ bound bdy
        defs = fs ++ argsn ++ Map.keys primCtx
        freevs = [x | x <- free bdy, idName x `notElem` defs]
    if null freevs
      then do
        -- no closure needed for monomorphic
        -- lambdas!
        --
        -- creating the lambda function by lifting it.
        fun1 <- createClosureFreeFun fn args bdy ps' ty
        info [">> Creating lambda lifted function(free):\n", pretty fun1, show ty]
        sch <- generalize (ps', ty)
        -- creating the invoke instance and unique type def.
        (udt@(DataTy dn tvs _), instd) <- generateDecls (fun1, sch)
        let t = TyCon dn (map (Meta . MetaTv . tyvarName) tvs)
        -- updating the type inference state
        writeFunDef fun1
        writeDataTy udt
        -- type checking generated instance
        checkInstance instd
        extEnv fn sch
        s <- getSubst
        clearSubst
        instd' <- tcInstance instd
        putSubst s
        writeInstance instd'
        pure (Con (Id dn t) [], t)
      else do
        (cdt, e', t') <- createClosureType freevs vs ty
        addUniqueType fn cdt
        (fun, sch) <- createClosureFun fn freevs cdt args bdy ps' ty
        info [">> Create lambda lifted function(closure):\n", pretty fun]
        writeFunDef fun
        writeDataTy cdt
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
createClosureType ids vs ty =
  do
    i <- incCounter
    s <- getSubst
    let ts = map idType ids
        dn = Name $ "t_closure" ++ show i
        ts' = everywhere (mkT gen) ts
        ns = map Var $ (apply s ids)
        vs' = nub $ (mv ts) `union` (map (MetaTv . var) vs)
        ty' = TyCon dn (Meta <$> vs')
        cid = Id dn (funtype ts ty')
        d = DataTy dn (map gvar vs') [Constr dn ts']
    info [">> Create closure type:", pretty d, " for type :", pretty ty]
    pure (d, Con cid ns, ty')

createClosureFun ::
  Name ->
  [Id] ->
  DataTy ->
  [Param Id] ->
  Body Id ->
  [Pred] ->
  Ty ->
  TcM (FunDef Id, Scheme)
createClosureFun fn freeIds cdt args bdy ps ty =
  do
    j <- incCounter
    ct <- closureTyCon cdt
    let args0 = everywhere (mkT gen) args
        ps0 = everywhere (mkT gen) ps
        cName = Name $ "env" ++ show j
        cParam = Typed (Id cName ct) ct
        args' = cParam : args0
        (_, retTy1) = splitTy ty
        vs' = union (bv ct) (bv ps0)
        ty' = ct :-> ty
        sig = Signature vs' ps0 fn args' (Just retTy1)
    bdy' <- createClosureBody cName cdt freeIds bdy
    sch <- generalize (ps0, ty')
    pure (everywhere (mkT gen) $ FunDef sig bdy', sch)

closureTyCon :: DataTy -> TcM Ty
closureTyCon (DataTy dn vs _) =
  pure (TyCon dn (TyVar <$> vs))

createClosureBody :: Name -> DataTy -> [Id] -> Body Id -> TcM (Body Id)
createClosureBody n cdt@(DataTy _ _ [Constr cn ts]) ids bdy =
  do
    ct <- closureTyCon cdt
    let ps = map PVar ids
        tc = funtype ts ct
    pure [Match [Var (Id n ct)] [([PCon (Id cn tc) ps], bdy)]]
createClosureBody _ cdt _ _ = "createClosureBody" `notImplemented` cdt

createClosureFreeFun ::
  Name ->
  [Param Id] ->
  Body Id ->
  [Pred] ->
  Ty ->
  TcM (FunDef Id)
createClosureFreeFun fn args bdy ps ty =
  do
    let (_, retTy1) = splitTy ty
        vs = bv ty `union` bv ps
        sig = Signature vs ps fn args (Just retTy1)
    pure (everywhere (mkT gen) $ FunDef sig bdy)

tcArgs :: [Param Name] -> TcM ([Param Id], [(Name, Scheme)], [Ty])
tcArgs params =
  do
    (ps, schs, ts) <- unzip3 <$> mapM tcArg params
    pure (ps, schs, ts)

tcArg :: Param Name -> TcM (Param Id, (Name, Scheme), Ty)
tcArg (Untyped n) =
  do
    v <- freshTyVar
    let ty = monotype v
    pure (Typed (Id n v) v, (n, ty), v)
tcArg a@(Typed n ty) =
  do
    ty1 <- kindCheck ty `wrapError` a
    pure (Typed (Id n ty1) ty1, (n, monotype ty1), ty1)

hasAnn :: Signature Name -> Bool
hasAnn (Signature _ _ _ args rt) =
  any isAnn args || isJust rt
  where
    isAnn (Typed _ _) = True
    isAnn _ = False

-- boolean flag indicates if the assumption for the
-- function should be included in the context. It
-- is necessary to not include the type of instance
-- functions which should have the type of its underlying
-- type class definition.

tiArg :: Param Name -> TcM (Param Id, (Name, Scheme), Ty)
tiArg (Untyped n) =
  do
    t <- freshTyVar
    pure (Typed (Id n t) t, (n, monotype t), t)
tiArg (Typed n _) =
  do
    t <- freshTyVar
    pure (Typed (Id n t) t, (n, monotype t), t)

tiArgs :: [Param Name] -> TcM ([Param Id], [(Name, Scheme)], [Ty])
tiArgs args = unzip3 <$> mapM tiArg args

tiFunDef :: FunDef Name -> TcM (FunDef Id, Scheme)
tiFunDef d@(FunDef sig@(Signature _ _ n args _) bd) =
  do
    info ["# tiFunDef:", pretty sig]
    -- getting fresh type variables for arguments
    (_, lctx, ts') <- tiArgs args
    -- fresh type for the function
    nt <- freshTyVar
    -- extended typing context for typing function body
    let lctx' = (n, monotype nt) : lctx
    -- typing function body
    (bd1, ps1, t1) <- withLocalCtx lctx' (tcBody bd) `wrapError` d
    -- unifying context introduced type with infered function type
    _ <- unify nt (funtype ts' t1) `wrapError` d
    -- building the function type scheme
    rs <- reduce [] ps1 `wrapError` d
    ty <- withCurrentSubst nt
    sch <- generalize (rs, ty)
    -- check for phantom (unconstrained) meta variables from constructor applications
    checkPhantomMetaVars True n bd1 rs ty `wrapError` d
    -- checking ambiguity
    info [">>> Infered type for ", pretty n, " :: ", pretty sch]
    ambSch <- ambiguityCheck sch
    when ambSch $ do
      ambiguousTypeError sch sig
    -- elaborating the type signature
    sig' <- elabSignature [] sig sch
    withCurrentSubst (FunDef sig' bd1, sch)

ambiguityCheck :: Scheme -> TcM Bool
ambiguityCheck (Forall _ (ps :=> ty)) =
  do
    noDesugarCalls <- getNoDesugarCalls
    -- here we do not consider invokable constraints
    -- if the option of no desugar indirect calls is enabled,
    -- since they will not be satisfied, since no instance will
    -- be generated.
    let ps' =
          if noDesugarCalls
            then [p | p <- ps, not (isInvoke p)]
            else ps
        vs' = bv (ps' :=> ty)
        sch' = Forall vs' (ps' :=> ty)
    pure (ambiguous sch')

argumentAnnotation :: Param Name -> TcM Ty
argumentAnnotation (Untyped _) =
  freshTyVar
argumentAnnotation (Typed _ t) =
  pure t

checkAllTypeVarsBound :: (Pretty a) => a -> [Tyvar] -> [Tyvar] -> TcM ()
checkAllTypeVarsBound context used declared =
  let unbound = used \\ declared
   in unless (null unbound) $ unboundTypeVars context unbound

annotatedScheme :: [Tyvar] -> [Pred] -> Signature Name -> TcM Scheme
annotatedScheme vs' qs (Signature vs ps _ args rt) =
  do
    ts <- mapM argumentAnnotation args
    t <- maybe freshTyVar pure rt
    let vs1 = vs ++ vs' ++ fv qs
    pure (Forall vs1 ((qs ++ ps) :=> (funtype ts t)))

tcFunDef :: Bool -> [Tyvar] -> [Pred] -> FunDef Name -> TcM (FunDef Id, Scheme)
tcFunDef incl vs' qs d@(FunDef sig@(Signature vs ps n _ _) _)
  | hasAnn sig = do
      info ["\n# tcFunDef ", pretty d]
      let vars = vs `union` vs'
      -- check if all variables are bound in signature.
      checkAllTypeVarsBound sig (bv sig) vars
      -- instantiate signatures in function definition
      sks <- mapM (const freshTyVar) vars
      let env = zip vars sks
          FunDef sig1@(Signature _ ps1 _ args1 rt1) bd1 = everywhere (mkT (insts @Ty env)) d
          qs1 = everywhere (mkT (insts @Ty env)) qs
      -- checking if all constraints have a respective class and are well kinded
      checkConstraints ps `wrapError` d
      info ["## predicates in signature:", pretty (ps1 ++ qs1)]
      -- getting argument / return types in annotations
      (_, lctx, ts') <- tcArgs args1
      rt1' <- maybe freshTyVar kindCheck rt1
      nt <- freshTyVar
      -- building the typing context with new assumptions
      let lctx' = if incl then (n, monotype nt) : lctx else lctx
      -- typing function body
      (bd1', ps1', t1') <- withLocalCtx lctx' (tcBodyWithExpectedReturn (Just rt1') bd1) `wrapError` d
      -- checking if the type checking have changed the type
      -- due to unique type creation.
      let tynames = tyconNames t1'
      changeTy <- or <$> mapM isUniqueTyName tynames
      let rt2 = if changeTy then t1' else rt1'
      info ["Trying to unify: ", pretty rt2, " with ", pretty t1']
      _ <- unify rt2 t1' `wrapError` d
      info ["Trying to unify: ", pretty nt, " with ", pretty (funtype ts' rt2)]
      _ <- unify nt (funtype ts' rt2) `wrapError` d
      -- building the function type scheme
      rs <- reduce (qs1 `union` ps1) ps1' `wrapError` d
      info [" - Reduced context: ", prettys rs]
      ty <- withCurrentSubst nt
      checkConstraints rs
      inf <- generalize (rs, ty)
      -- check for phantom (unconstrained) meta variables from constructor applications
      checkPhantomMetaVars False n bd1' rs ty `wrapError` d
      info [" - generalized inferred type: ", pretty inf]
      ann <- annotatedScheme vs' qs sig
      info [" - annotated type:", pretty ann]
      -- checking ambiguity
      ambSch <- ambiguityCheck inf
      when ambSch $ do
        ambiguousTypeError inf sig
      -- checking subsumption
      unless changeTy $ do
        subsCheck sig inf ann `wrapError` d
      -- elaborating function body
      let ann' = if changeTy then inf else ann
      fdt <- elabFunDef vs' sig1 bd1' inf ann' `wrapError` d
      withCurrentSubst (fdt, ann')
  | otherwise = tiFunDef d

-- elaborating function definition

elabFunDef ::
  [Tyvar] -> -- additional variables which came from outer scope
  Signature Name -> -- original function signature
  Body Id -> -- elaborated function body (with fresh variables)
  Scheme -> -- function infered type
  Scheme -> -- function annotated type
  TcM (FunDef Id)
elabFunDef vs sig bdy (Forall _ (_ :=> tinf)) ann@(Forall _ (_ :=> tann)) =
  do
    let tinf' = everywhere (mkT toMeta) tinf
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
ambiguous (Forall _ (ps :=> t)) =
  not $ null $ bv ps \\ bv (closure ps (bv t))

-- Check for phantom meta variables: meta vars appearing in constructor
-- application result types in the body but not in the function's inferred
-- type or environment. Such variables arise when a constructor has phantom
-- type parameters (type parameters that do not appear in any constructor
-- field). They cannot be determined from context and indicate a type error.
-- The function uses a boolean flag, checkReturn:
-- checkReturn = True   (tiFunDef, unannotated): also flag constructor-result
--   meta vars that escaped into the return type without being determined by
--   the argument types or constraints.
-- checkReturn = False  (tcFunDef, annotated): skip that second check, because
--   the programmer explicitly declared the return type; any phantom variable
--   in it is intentional and will be resolved by the instance or call context.
checkPhantomMetaVars :: Bool -> Name -> Body Id -> [Pred] -> Ty -> TcM ()
checkPhantomMetaVars checkReturn n body rs ty = do
  envVars <- getEnvMetaVars
  bodySubst <- withCurrentSubst body
  tySubst <- withCurrentSubst (rs, ty)
  let (rsApplied, tyApplied) = tySubst
      legitimateMVs = mv tySubst ++ envVars
      conMVs = conResultMetaVars bodySubst
      -- Case 1: constructor-result MVs entirely absent from the function's type.
      phantomMVs = conMVs \\ legitimateMVs
      -- Case 2 (unannotated only): constructor-result MVs that appear in the
      -- return type but not in the argument types or constraints.  A meta var
      -- that appears in a constraint is determined at call sites through type
      -- class dispatch, so it must be excluded from the suspicious set.
      -- Compiler-generated closure types (from defunctionalization) are
      -- excluded: their phantom parameters are legitimately resolved at the
      -- call site via the Invokable instance.
      (argTys, retTy') = splitTy tyApplied
      determined = mv argTys `union` mv rsApplied
  escapedReturnMVs <-
    if not checkReturn
      then pure []
      else case outerTyCon retTy' of
        Nothing -> pure []
        Just retTyName -> do
          isGenerated <- isUniqueTyName retTyName
          if isGenerated
            then pure []
            else pure [m | m <- conMVs, m `elem` mv retTy', m `notElem` determined]
  let allPhantomMVs = nub (phantomMVs ++ escapedReturnMVs)
  unless (null allPhantomMVs) $ do
    let mvNames = intercalate ", " $ map (pretty . metaName) allPhantomMVs
    throwError $
      unlines
        [ "Ambiguous type variable(s) " ++ mvNames ++ " in definition of " ++ pretty n ++ ".",
          "This typically occurs when a constructor has phantom type parameters.",
          "Please, add a type signature to fix the ambiguous type variable."
        ]

outerTyCon :: Ty -> Maybe Name
outerTyCon (TyCon n _) = Just n
outerTyCon _ = Nothing

conResultMetaVars :: (Data a) => a -> [MetaTv]
conResultMetaVars = nub . everything (++) (mkQ [] collectConMVs)
  where
    collectConMVs :: Exp Id -> [MetaTv]
    collectConMVs (Con (Id _ ty) args) = mv (applyConArgs ty args)
    collectConMVs _ = []

    applyConArgs :: Ty -> [a] -> Ty
    applyConArgs (_ :-> rest) (_ : as) = applyConArgs rest as
    applyConArgs ty _ = ty

reachable :: [Pred] -> [Tyvar] -> [Pred]
reachable ps vs =
  [p | p <- ps, disjunct (bv p) vs]

closure :: [Pred] -> [Tyvar] -> [Pred]
closure ps vs
  | subset (bv $ reachable ps vs) vs = reachable ps vs
  | otherwise = closure ps (bv (reachable ps vs))

subset :: (Eq a) => [a] -> [a] -> Bool
subset xs ys = all (\x -> x `elem` ys) xs

disjunct :: (Eq a) => [a] -> [a] -> Bool
disjunct xs ys = not $ null $ intersect xs ys

-- only invokable constraints can be inserted freely

isValid :: [Pred] -> Bool
isValid rs = null rs || all isInvokePred rs
  where
    isInvokePred (InCls n _ _) =
      n == invokableName
    isInvokePred _ = False

-- update types in signature

elabSignature :: [Tyvar] -> Signature Name -> Scheme -> TcM (Signature Id)
elabSignature vs1 sig (Forall _ (ps :=> t)) =
  do
    let params = sigParams sig
        nparams = length params
        (ts, t') = splitTy t
        (ts', rs) = splitAt nparams ts
    params' <- zipWithM elabParam ts' params
    let -- here we build the return type.
        -- Note that, since we can return functions, we need to check if the
        -- formal parameters are present in the signature.
        ret = Just $ if null params' then t else (funtype rs t')
        vs' = bv params' `union` bv ret `union` bv ps
    sig2 <- withCurrentSubst (Signature (vs' \\ vs1) ps (sigName sig) params' ret)
    pure sig2

elabParam :: Ty -> Param Name -> TcM (Param Id)
elabParam t (Typed n _) = pure $ Typed (Id n t) t
elabParam t (Untyped n) = pure $ Typed (Id n t) t

annotateSignature :: Scheme -> Signature Name -> TcM (Signature Name)
annotateSignature (Forall vs (ps :=> t)) sig =
  pure $ Signature vs ps (sigName sig) params' ret
  where
    (ts, t') = splitTy t
    params' = zipWith annotateParam ts (sigParams sig)
    ret = Just t'

annotateParam :: Ty -> Param Name -> Param Name
annotateParam t (Typed n _) = Typed n t
annotateParam t (Untyped n) = Typed n t

-- qualify name for contract functions

correctName :: Name -> TcM Name
correctName n@(QualName _ _) = pure n
correctName (Name s) =
  do
    c <- gets contract
    if isJust c
      then pure (QualName (fromJust c) s)
      else pure (Name s)

extSignature :: Signature Name -> TcM ()
extSignature sig@(Signature _ _ n _ _) =
  do
    te <- gets directCalls
    -- checking if the function is previously defined
    when (n `elem` te) (duplicatedFunDef n) `wrapError` sig
    addFunctionName n

-- typing instance

tcInstance :: Instance Name -> TcM (Instance Id)
tcInstance idecl@(Instance d vs predCtx n ts t funs) =
  do
    -- checking instance type parameters
    t' <- kindCheck t `wrapError` idecl
    ts' <- mapM kindCheck ts `wrapError` idecl
    -- checking constraints
    qs' <- mapM checkConstraint predCtx `wrapError` idecl
    tcInstance' (Instance d vs qs' n ts' t' funs)

checkConstraint :: Pred -> TcM Pred
checkConstraint p@(InCls n t ts) =
  do
    cinfo <- askClassInfo n
    unless (length ts == classArity cinfo) $
      classArityError n cinfo p
    t' <- kindCheck t `wrapError` p
    ts' <- mapM kindCheck ts `wrapError` p
    pure (InCls n t' ts')
checkConstraint (t :~: t') =
  (:~:) <$> kindCheck t <*> kindCheck t'

tcInstance' :: Instance Name -> TcM (Instance Id)
tcInstance' idecl@(Instance d vs predCtx n ts t funs) =
  do
    checkCompleteInstDef n (map (sigName . funSignature) funs) `wrapError` idecl
    (funs1, _) <- unzip <$> mapM (tcFunDef False vs predCtx) funs `wrapError` idecl
    instd <- withCurrentSubst (Instance d vs predCtx n ts t funs1)
    let ind@(Instance _ _ ctx' _ ts' t' funs2) = everywhere (mkT gen) instd
        vs1 = bv ind
        funs3 =
          sortBy
            ( \f f' ->
                compare
                  (sigName (funSignature f))
                  (sigName (funSignature f'))
            )
            (map (updateSignature vs1 n) funs2)
    verifySignatures (Instance d vs1 ctx' n ts' t' funs3)

verifySignatures :: Instance Id -> TcM (Instance Id)
verifySignatures instd@(Instance _ _ ps n ts t funs) =
  do
    -- get class info
    mcinfo <- Map.lookup n <$> gets classTable
    when (isNothing mcinfo) (undefinedClass n) `wrapError` instd
    -- building instance constraint
    let -- this use of fromJust is safe, because is
        -- guarded by the isNothing test.
        cinfo = fromJust mcinfo
        instc = ps :=> (InCls n t ts)
        classc = classpred cinfo
        bvarsc = bv classc
        bvarsi = bv instc
    -- building the instantiation environments
    freshc <- mapM (const freshTyVar) bvarsc
    freshi <- mapM (const freshTyVar) bvarsi
    let envc = zip bvarsc freshc
        envi = zip bvarsi freshi
        (_ :=> ih) = insts envi instc
        classc' = insts envc classc
    -- getting matching substitution
    s <- match classc' ih `wrapError` instd
    -- getting method types
    let qnames = map qual (methods cinfo)
        qual v = if v == invoke then v else QualName n (pretty v)
    -- getting most general types and instantiate them
    aqts <-
      mapM
        ( \q -> do
            (Forall _ qt) <- askEnv q `wrapError` instd
            let qt' = insts envc qt
                vs' = bv qt'
            ts' <- mapM (const freshTyVar) vs'
            let env = zip vs' ts'
                tyr = insts env qt'
            pure (q, apply s tyr)
        )
        qnames
    -- getting infered types
    iqts <-
      mapM
        ( \f -> do
            let sig = funSignature f
            schf <- schemeFromSignature sig
            (sigName sig,) <$> freshInst schf
        )
        funs
    -- combine triples
    let m = [(q, it, at') | (q, it) <- iqts, (q', at') <- aqts, q == q']
    mapM_ checkMemberType m `wrapError` instd
    pure instd

checkMemberType :: (Name, Qual Ty, Qual Ty) -> TcM ()
checkMemberType (qn, _ :=> t, _ :=> t')
  -- whenever we have a closure, the infered type
  -- will change. This fact causes an error when
  -- the function has a signature, since the infered
  -- type will not match the annotated type.
  | hasClosureType t = pure ()
  | otherwise =
      do
        _ <- tcmMatch t t' `catchError` (\_ -> invalidMemberType qn t t')
        pure ()

hasClosureType :: Ty -> Bool
hasClosureType = any isClosureName . tyconNames

invalidMemberType :: Name -> Ty -> Ty -> TcM a
invalidMemberType n cls ins =
  throwError $
    unlines
      [ "The instance method:",
        pretty n,
        "has the following infered type:",
        pretty ins,
        "which is not an valid instance for:",
        pretty cls
      ]

schemeFromSignature :: Signature Id -> TcM Scheme
schemeFromSignature sig@(Signature vs ps _ args (Just rt)) =
  do
    unless (all isTyped args) $
      throwError $
        unwords ["Invalid instance member signature:", pretty sig]
    pure $ Forall vs (ps :=> (funtype ts rt))
  where
    isTyped (Typed _ _) = True
    isTyped _ = False

    extractType (Typed _ t) = t
    extractType p =
      error $ "schemeFromSignature: expected typed parameter, got " ++ show p

    ts = map extractType args
schemeFromSignature sig =
  throwError $
    unwords ["Invalid instance member signature (missing return type):", pretty sig]

updateSignature :: [Tyvar] -> Name -> FunDef Id -> FunDef Id
updateSignature vs' c (FunDef (Signature vs ps n args rt) bd) =
  FunDef (Signature (vs \\ vs') ps (QualName c (pretty n)) args rt) bd

checkDeferedConstraints :: [(FunDef Id, [Pred])] -> TcM ()
checkDeferedConstraints = mapM_ checkDeferedConstraint
  where
    checkDeferedConstraint (fd, ps) =
      unless (null ps) $
        tcmError $
          unlines
            [ "Cannot satisfy:",
              pretty ps,
              "from:",
              if null sigCtx then "<empty context>" else pretty sigCtx,
              "in:",
              pretty sig
            ]
      where
        sig = funSignature fd
        sigCtx = sigContext sig

checkCompleteInstDef :: Name -> [Name] -> TcM ()
checkCompleteInstDef n ns =
  do
    mths <- methods <$> askClassInfo n
    let unqual (QualName _ m) = Name m
        unqual m = m
        mths' = map unqual mths
        remaining = mths' \\ ns
    when (not $ null remaining) do
      throwError $
        unlines $
          [ "Incomplete definition for class:",
            pretty n,
            "missing definitions for:"
          ]
            ++ map pretty remaining

-- checking instances and adding them in the environment

checkInstances :: [Instance Name] -> TcM ()
checkInstances = mapM_ checkInstance

checkConstraints :: [Pred] -> TcM ()
checkConstraints = mapM_ checkConstraint

checkInstance :: Instance Name -> TcM ()
checkInstance idef@(Instance d vs predCtx n ts t funs) =
  do
    trustedImported <- isTrustedImportedInstance idef
    -- checking if all variables are declared
    checkAllTypeVarsBound idef (bv idef) vs
    -- kind check all types in instance head
    mapM_ kindCheck (t : ts) `wrapError` idef
    -- check if the class is defined
    cinfo <- askClassInfo n `wrapError` idef
    -- check if the instance arity is correct
    unless (length ts == classArity cinfo) $
      classArityError n cinfo idef
    -- check if all the types and classes in the context are valid
    checkConstraints predCtx
    tExp <- maybeExpandSynonym t
    tsExp <- mapM maybeExpandSynonym ts
    predCtxExp <- mapM expandPredSynonyms predCtx
    let ipred = InCls n tExp tsExp
    -- checking the coverage condition
    insts' <- askInstEnv n `wrapError` ipred
    -- check overlapping only for non-default instances
    let vs1 = bv ipred
    ts1 <- mapM (const freshTyVar) vs1
    let env = zip vs1 ts1
        ipred' = insts env ipred
    unless d (checkOverlap ipred' insts' `wrapError` idef)
    -- check if default instance has a type variable as main argument.
    when d (checkDefaultInst (predCtxExp :=> ipred) `wrapError` idef)
    coverageEnabled <- askCoverage n
    unless (trustedImported || coverageEnabled) (checkCoverage n tsExp tExp `wrapError` idef)
    -- checking Patterson condition
    pattersonEnabled <- askPattersonCondition n
    unless (trustedImported || pattersonEnabled) (checkMeasure predCtxExp ipred `wrapError` idef)
    -- checking bound variable condition
    boundEnabled <- askBoundVariableCondition n
    unless (trustedImported || boundEnabled) (checkBoundVariable predCtxExp (bv (tExp : tsExp)) `wrapError` idef)
    -- checking instance methods
    mapM_ (checkMethod ipred) funs `wrapError` idef
    let ninst = anfInstance $ predCtxExp :=> ipred
    -- add to the environment
    if d
      then addDefaultInstance n ninst
      else addInstance n ninst

maybeExpandSynonym :: Ty -> TcM Ty
maybeExpandSynonym (TyCon n ts) = do
  ts' <- mapM maybeExpandSynonym ts
  mSyn <- maybeAskSynInfo n
  case mSyn of
    Just (SynInfo ar params body)
      | ar == length ts' ->
          maybeExpandSynonym (insts (zip params ts') body)
      | otherwise ->
          throwError $
            unlines
              [ "Type synonym arity mismatch for '" ++ pretty n ++ "':",
                "  expected " ++ show ar ++ " argument(s)",
                "  but got  " ++ show (length ts')
              ]
    Nothing -> pure (TyCon n ts')
maybeExpandSynonym (t1 :-> t2) = (:->) <$> maybeExpandSynonym t1 <*> maybeExpandSynonym t2
maybeExpandSynonym t = pure t

expandPredSynonyms :: Pred -> TcM Pred
expandPredSynonyms (InCls n t ts) = do
  t' <- maybeExpandSynonym t
  ts' <- mapM maybeExpandSynonym ts
  pure (InCls n t' ts')
expandPredSynonyms (t1 :~: t2) =
  (:~:) <$> maybeExpandSynonym t1 <*> maybeExpandSynonym t2

-- checking a default instance

checkDefaultInst :: Qual Pred -> TcM ()
checkDefaultInst p@(_ :=> InCls _ t _) =
  unless (isTyVar t) (invalidDefaultInst p)
checkDefaultInst p = invalidDefaultInst p

isTyVar :: Ty -> Bool
isTyVar (TyVar _) = True
isTyVar _ = False

-- bound variable check

checkBoundVariable :: [Pred] -> [Tyvar] -> TcM ()
checkBoundVariable ps vs =
  unless (all (`elem` vs) (bv ps)) $ do
    throwError "Bounded variable condition fails!"

checkOverlap :: Pred -> [Inst] -> TcM ()
checkOverlap _ [] = pure ()
checkOverlap p@(InCls _ t _) (i : is) =
  do
    i' <- freshInst i
    case i' of
      (_ :=> (InCls _ t' _)) ->
        case mgu t t' of
          Right _ ->
            throwError
              ( unlines
                  [ "Overlapping instances are not supported",
                    "instance:",
                    pretty p,
                    "overlaps with:",
                    pretty i'
                  ]
              )
          Left _ -> checkOverlap p is
      _ -> checkOverlap p is
    return ()
checkOverlap p (_ : is) = checkOverlap p is

-- check coverage condition

checkCoverage :: Name -> [Ty] -> Ty -> TcM ()
checkCoverage cn ts t =
  do
    let strongTvs = bv t
        weakTvs = bv ts
        undetermined = weakTvs \\ strongTvs
    unless (null undetermined) $
      throwError
        ( unlines
            [ "Coverage condition fails for class:",
              pretty cn,
              "- the type:",
              pretty t,
              "does not determine:",
              intercalate ", " (map pretty undetermined)
            ]
        )

checkMethod :: Pred -> FunDef Name -> TcM ()
checkMethod ih@(InCls n _ _) d@(FunDef sig _) =
  do
    -- checking if the signature is fully annotated
    fullSignature sig
    -- getting current method signature in class
    let qn = QualName n (show (sigName sig))
    sch <- askEnv qn `wrapError` d
    (qs :=> _) <- freshInst sch
    p <-
      maybeToTcM
        ( unwords
            [ "Constraint for",
              show n,
              "not found in type of",
              show $ sigName sig
            ]
        )
        (findPred n qs)
    -- matching substitution of instance head and class predicate
    _ <- liftEither (match p ih) `wrapError` d
    pure ()
checkMethod p d = invalidMethodPred p d

fullSignature :: Signature Name -> TcM ()
fullSignature sig@(Signature _ _ _ ps t) =
  unless
    (all isTyped ps && maybe False (const True) t)
    (throwError $ unlines ["Instance methods must have complete type signatures:", pretty sig])
  where
    isTyped (Typed _ _) = True
    isTyped _ = False

findPred :: Name -> [Pred] -> Maybe Pred
findPred _ [] = Nothing
findPred n (p@(InCls n' _ _) : ps)
  | n == n' = Just p
  | otherwise = findPred n ps
findPred n (_ : ps) = findPred n ps

-- checking Patterson conditions

checkMeasure :: [Pred] -> Pred -> TcM ()
checkMeasure ps c =
  if all smaller ps
    then return ()
    else
      throwError $
        unlines
          [ "Instance ",
            pretty c,
            "does not satisfy the Patterson conditions."
          ]
  where
    smaller p = measure p < measure c

-- subsumption check

subsCheck :: Signature Name -> Scheme -> Scheme -> TcM ()
subsCheck sig inf ann =
  do
    info [">> Checking subsumption for:\n", pretty inf, "\nand\n", pretty ann]
    (skol_tvs, (ps2 :=> t2)) <- skolemise ann
    info [">>> Skolemization result:", pretty (ps2 :=> t2), " - Skolem constants:", unwords (map pretty skol_tvs)]
    (ps1 :=> t1) <- freshInst inf
    info [">>> Instantiation result:", pretty (ps1 :=> t1)]
    s <- mgu t1 t2 `catchError` (\_ -> typeNotPolymorphicEnough sig inf ann)
    _ <- extSubst s
    let esc_tvs = fv inf
        bad_tvs = filter (`elem` esc_tvs) skol_tvs
    unless (null bad_tvs) $
      typeNotPolymorphicEnough sig inf ann
    -- checking constraints
    _ <- enforceDependencies (apply s (ps1 ++ ps2))
    s1 <- getSubst
    unsolved <- hnfEntails (apply s1 ps2) (apply s1 ps1)
    unless (null unsolved) (unsolvedError unsolved)
    pure ()

hnfEntails :: [Pred] -> [Pred] -> TcM [Pred]
hnfEntails qs ps =
  do
    info ["Trying to entail:", pretty ps, " using:", pretty qs]
    ctable <- getClassEnv
    itable <- getInstEnv
    depth <- askMaxRecursionDepth
    let qs' = nub $ concatMap (bySuperM ctable) qs
        notInvoke p = not (isInvoke p)
        needSolving = filter (\p -> notInvoke p && not (entail ctable itable qs' p)) ps
    -- For predicates pure entailment couldn't discharge, try the monadic solver
    -- within a local substitution scope so that Skolem bindings don't escape.
    withLocalSubst $ do
      remaining <- toHnfs depth needSolving
      pure (filter notInvoke remaining)

-- type generalization

generalize :: ([Pred], Ty) -> TcM Scheme
generalize (ps, t) =
  do
    envVars <- getEnvMetaVars
    (ps1, t1) <- withCurrentSubst (ps, t)
    let vs = map gvar $ mv (ps1, t1) \\ envVars
        sch = Forall vs (everywhere (mkT gen) $ ps1 :=> t1)
    return sch

tcBody :: Body Name -> TcM (Body Id, [Pred], Ty)
tcBody = tcBodyWithExpectedReturn Nothing

tcBodyWithExpectedReturn :: Maybe Ty -> Body Name -> TcM (Body Id, [Pred], Ty)
tcBodyWithExpectedReturn _ [] = pure ([], [], unit)
tcBodyWithExpectedReturn mExpectedReturn [s] =
  do
    (s', ps', t') <- tcStmtWithExpectedReturn mExpectedReturn s
    pure ([s'], ps', t')
tcBodyWithExpectedReturn _ (Return _ : _) =
  throwError "Illegal return statement"
tcBodyWithExpectedReturn mExpectedReturn (s : ss) =
  do
    (s', ps', _) <- tcStmtWithExpectedReturn mExpectedReturn s
    (bd', ps1, t1) <- tcBodyWithExpectedReturn mExpectedReturn ss
    pure (s' : bd', ps' ++ ps1, t1)

tcCall :: Maybe (Exp Name) -> Name -> [Exp Name] -> TcM (Exp Id, [Pred], Ty)
tcCall Nothing n args =
  do
    s <- askEnv n `wrapError` (Call Nothing n args)
    (ps :=> t) <- freshInst s
    t' <- freshTyVar
    expectedArgTys <- mapM (const freshTyVar) args
    s0 <- unify t (funtype expectedArgTys t')
    _ <- extSubst s0
    (es', pss', ts') <-
      unzip3 <$> zipWithM (\e expectedTy -> tcExpWithExpected (Just expectedTy) e) args (apply s0 expectedArgTys)
    s1 <- unify t (funtype ts' t')
    _ <- extSubst s1
    let ps' = foldr union [] (ps : pss')
        t1 = funtype ts' t'
    withCurrentSubst (Call Nothing (Id n t1) es', ps', t')
tcCall (Just e) n args =
  do
    (e', ps, _) <- tcExp e
    s <- askEnv n `wrapError` (Call (Just e) n args)
    (ps1 :=> t) <- freshInst s
    t' <- freshTyVar
    expectedArgTys <- mapM (const freshTyVar) args
    s0 <- unify (foldr (:->) t' expectedArgTys) t
    _ <- extSubst s0
    (es', pss', ts') <-
      unzip3 <$> zipWithM (\arg expectedTy -> tcExpWithExpected (Just expectedTy) arg) args (apply s0 expectedArgTys)
    s' <- unify (foldr (:->) t' ts') t
    _ <- extSubst s'
    let ps' = foldr union [] ((ps ++ ps1) : pss')
    withCurrentSubst (Call (Just e') (Id n t') es', ps', t')

tcParam :: Param Name -> TcM (Param Id)
tcParam (Typed n t) =
  pure $ Typed (Id n t) t
tcParam (Untyped n) =
  do
    t <- freshTyVar
    pure (Typed (Id n t) t)

resolvePatternConstructor :: Name -> Ty -> TcM Name
resolvePatternConstructor n expectedTy
  | isDotConstructorMarker n = resolveDotPatternConstructor n expectedTy
  | otherwise = canonicalizeConstructorName n

resolveExpressionConstructor :: Name -> [Ty] -> Maybe Ty -> TcM Name
resolveExpressionConstructor n argTys mExpected
  | isDotConstructorMarker n = resolveDotExpressionConstructor n argTys mExpected
  | otherwise = canonicalizeConstructorName n

canonicalizeConstructorName :: Name -> TcM Name
canonicalizeConstructorName n@(QualName _ _) =
  pure n
canonicalizeConstructorName n =
  do
    mUnqual <- maybeAskEnv n
    case mUnqual of
      Just _ -> pure n
      Nothing -> do
        let qn = QualName n (pretty n)
        mQual <- maybeAskEnv qn
        pure (if isJust mQual then qn else n)

resolveDotExpressionConstructor :: Name -> [Ty] -> Maybe Ty -> TcM Name
resolveDotExpressionConstructor dotName argTys mExpected = do
  mcandidates <- candidatesForDotExpression dotName mExpected
  candidates <- case mcandidates of
    Just xs -> pure xs
    Nothing ->
      throwError $
        unlines
          [ "Cannot resolve shorthand constructor expression without expected constructor type:",
            pretty dotName
          ]
  valid <- filterM (\n -> constructorAcceptsArguments n argTys mExpected) (nub candidates)
  case valid of
    [] ->
      throwError $
        unlines
          [ "No matching constructor for shorthand expression:",
            pretty dotName
          ]
    [n] -> pure n
    xs ->
      throwError $
        unlines
          [ "Ambiguous shorthand constructor expression:",
            pretty dotName,
            "Candidates:",
            unwords (map pretty xs)
          ]

constructorAcceptsArguments :: Name -> [Ty] -> Maybe Ty -> TcM Bool
constructorAcceptsArguments n argTys mExpected = do
  s0 <- getSubst
  r <-
    ( do
        sch <- askEnv n
        (_ :=> conTy) <- freshInst sch
        resultTy <- freshTyVar
        _ <- unify conTy (funtype argTys resultTy)
        case mExpected of
          Just expectedTy -> do
            expectedTy' <- maybeExpandSynonym expectedTy
            _ <- unify resultTy expectedTy'
            pure ()
          Nothing -> pure ()
        pure True
      )
      `catchError` const (pure False)
  putSubst s0
  pure r

candidatesForDotExpression :: Name -> Maybe Ty -> TcM (Maybe [Name])
candidatesForDotExpression dotName mExpected = do
  let leaf = dotMarkerLeafName dotName
  mExpected' <- traverse maybeExpandSynonym mExpected
  case mExpected' of
    Just (TyCon tyName _) -> do
      ti <- askTypeInfo tyName
      visibleConstructors <- visibleConstructorsForType tyName (constrNames ti)
      pure (Just (matchingConstructors leaf visibleConstructors))
    _ ->
      pure Nothing

resolveDotPatternConstructor :: Name -> Ty -> TcM Name
resolveDotPatternConstructor dotName expectedTy = do
  mcandidates <- candidatesForDotPattern dotName expectedTy
  candidates <- case mcandidates of
    Just xs -> pure xs
    Nothing ->
      throwError $
        unlines
          [ "Cannot resolve shorthand constructor pattern without expected constructor type:",
            pretty dotName
          ]
  case nub candidates of
    [] ->
      throwError $
        unlines
          [ "No matching constructor for shorthand pattern:",
            pretty dotName
          ]
    [n] -> pure n
    xs ->
      throwError $
        unlines
          [ "Ambiguous shorthand constructor pattern:",
            pretty dotName,
            "Candidates:",
            unwords (map pretty xs)
          ]

candidatesForDotPattern :: Name -> Ty -> TcM (Maybe [Name])
candidatesForDotPattern dotName expectedTy = do
  expectedTy' <- maybeExpandSynonym expectedTy
  let leaf = dotMarkerLeafName dotName
  case expectedTy' of
    TyCon tyName _ -> do
      ti <- askTypeInfo tyName
      visibleConstructors <- visibleConstructorsForType tyName (constrNames ti)
      pure (Just (matchingConstructors leaf visibleConstructors))
    _ ->
      pure Nothing

visibleConstructorsForType :: Name -> [Name] -> TcM [Name]
visibleConstructorsForType tyName allConstructors = do
  mVisibleLeafNames <- visibleConstructorsForPartialDataType tyName
  pure $
    case mVisibleLeafNames of
      Nothing -> allConstructors
      Just visibleLeafNames ->
        filter (\n -> constructorLeafName n `Set.member` visibleLeafNames) allConstructors

matchingConstructors :: Name -> [Name] -> [Name]
matchingConstructors leaf =
  filter (\n -> constructorLeafName n == leaf)

isDotConstructorMarker :: Name -> Bool
isDotConstructorMarker (Name ('.' : _)) = True
isDotConstructorMarker _ = False

dotMarkerLeafName :: Name -> Name
dotMarkerLeafName (Name ('.' : xs)) = Name xs
dotMarkerLeafName n = constructorLeafName n

constructorLeafName :: Name -> Name
constructorLeafName (QualName _ n) = Name n
constructorLeafName n = n

typeName :: Ty -> TcM Name
typeName (TyCon n _) = pure n
typeName t =
  throwError $
    unlines
      [ "Expected type, but found:",
        pretty t
      ]

-- typing Yul code

tcYulBlock :: YulBlock -> TcM ([Name], Ty)
tcYulBlock [] =
  pure ([], unit)
tcYulBlock [s] =
  tcYulStmt s
tcYulBlock (s : ss) =
  do
    (ns, _) <- tcYulStmt s
    (nss, t) <- tcYulBlock ss
    pure (ns ++ nss, t)

tcYulStmt :: YulStmt -> TcM ([Name], Ty)
tcYulStmt (YAssign _ e) =
  do
    -- do not define names
    _ <- tcYulExp e
    pure ([], unit)
tcYulStmt (YBlock yblk) =
  do
    _ <- tcYulBlock yblk
    -- names defined in should not return
    pure ([], unit)
tcYulStmt (YLet ns (Just e)) =
  do
    _ <- tcYulExp e
    mapM_ (flip extEnv mword) ns
    pure (ns, unit)
tcYulStmt (YExp e) =
  do
    t <- tcYulExp e
    pure ([], t)
tcYulStmt (YIf e yblk) =
  do
    _ <- tcYulExp e
    _ <- tcYulBlock yblk
    pure ([], unit)
tcYulStmt (YSwitch e cs df) =
  do
    _ <- tcYulExp e
    _ <- tcYulCases cs
    _ <- tcYulDefault df
    pure ([], unit)
tcYulStmt (YFor initBlk e bdy upd) =
  do
    ns <- fst <$> tcYulBlock initBlk
    _ <- withLocalEnv do
      mapM_ (flip extEnv mword) ns
      _ <- tcYulExp e
      _ <- tcYulBlock bdy
      _ <- tcYulBlock upd
      pure ()
    pure ([], unit)
tcYulStmt _ = pure ([], unit)

tcYulExp :: YulExp -> TcM Ty
tcYulExp (YLit l) =
  tcYLit l
tcYulExp (YIdent v) =
  do
    sch <- askEnv v `wrapError` (YIdent v)
    (_ :=> t) <- freshInst sch
    _ <- unify t word
    pure t
tcYulExp e@(YCall n es) =
  do
    sch <- askEnv n `wrapError` e
    (_ :=> t) <- freshInst sch
    ts <- mapM tcYulExp es
    t' <- freshTyVar
    s <- unify t (funtype ts t') `wrapError` e
    _ <- extSubst s
    withCurrentSubst t'
tcYulExp (YMeta _) = pure word

tcYLit :: YLiteral -> TcM Ty
tcYLit (YulString _) = return string
tcYLit (YulNumber _) = return word
tcYLit lit = notImplemented "tcYLit" lit

tcYulCases :: YulCases -> TcM ()
tcYulCases = mapM_ tcYulCase

tcYulCase :: YulCase -> TcM ()
tcYulCase (_, yblk) =
  do
    _ <- tcYulBlock yblk
    return ()

tcYulDefault :: Maybe YulBlock -> TcM ()
tcYulDefault (Just b) =
  do
    _ <- tcYulBlock b
    pure ()
tcYulDefault Nothing = pure ()

mword :: Scheme
mword = monotype word

-- determining free variables

class Vars a where
  free :: a -> [Id]
  bound :: a -> [Id]

instance (Vars a) => Vars [a] where
  free es = foldr (union . free) [] es \\ bound es
  bound = foldr (union . bound) []

instance Vars Id where
  free i@(Id n _)
    | isQual n = []
    | otherwise = [i]
  bound _ = []

instance Vars (Pat Id) where
  free _ = []

  bound (PVar v) = [v]
  bound (PCon _ ps) = bound ps
  bound _ = []

instance Vars (Param Id) where
  free _ = []

  bound (Typed n _) = [n]
  bound (Untyped n) = [n]

instance Vars (Stmt Id) where
  free (e1 := e2) = free [e1, e2]
  free (Let _ _ (Just e)) = free e
  free (Let _ _ _) = []
  free (StmtExp e) = free e
  free (Return e) = free e
  free (Match e eqns) = free e `union` free eqns
  free (If e blk1 blk2) = free e `union` free blk1 `union` free blk2
  free (Asm _) = []

  bound (Let n _ _) = [n]
  bound _ = []

instance Vars (Equation Id) where
  free (ps, ss) = free ss \\ bound ps
  bound _ = []

instance Vars (Exp Id) where
  free (Var n) = free n
  free (Con _ es) = free es
  free (FieldAccess Nothing _) = []
  free (FieldAccess (Just e) _) = free e
  free (Call (Just e) n es) = free e `union` free n `union` free es
  free (Call Nothing n es) = free n `union` free es
  free (Lam ps bd _) = free bd \\ bound ps
  free _ = []

  bound _ = []

-- rename type variables

rename :: Ty -> Ty
rename t =
  let vs = bv t
      s = zip vs (map (TyVar . TVar) namePool)
   in insts s t

-- errors

classArityError :: (Pretty a) => Name -> ClassInfo -> a -> TcM ()
classArityError n cinfo v =
  throwError $
    unlines
      [ "Type class " ++ pretty n,
        "requires " ++ show (classArity cinfo) ++ " weak parameter(s)",
        "which does not match:",
        pretty v
      ]

unboundTypeVars :: (Pretty a) => a -> [Tyvar] -> TcM b
unboundTypeVars sig vs =
  throwError $ unlines ["Type variables:", vs', "are unbound in:", pretty sig]
  where
    vs' = unwords $ map pretty vs

typeMatch :: Scheme -> Scheme -> TcM ()
typeMatch t1 t2 =
  unless (t1 == t2) $
    throwError $
      unwords
        [ "Types",
          pretty t1,
          "and",
          pretty t2,
          "do not match"
        ]

invalidYulType :: Name -> Ty -> TcM a
invalidYulType (Name n) ty =
  throwError $ unlines ["Yul values can only be of word type:", unwords [n, ":", pretty ty]]
invalidYulType qn ty =
  throwError $ unlines ["Yul values can only be of word type:", unwords [pretty qn, ":", pretty ty]]

invalidMethodPred :: Pred -> FunDef Name -> TcM a
invalidMethodPred p d =
  throwError $
    unlines
      [ "Expected class predicate in instance head for method check:",
        pretty p,
        "in:",
        pretty d
      ]

expectedFunction :: Ty -> TcM a
expectedFunction t =
  throwError $
    unlines
      [ "Expected function type. Found:",
        pretty t
      ]

wrongPatternNumber :: [Ty] -> [Pat Name] -> TcM a
wrongPatternNumber qts ps =
  throwError $
    unlines
      [ "Wrong number of patterns in:",
        unwords (map pretty ps),
        "expected:",
        show (length qts),
        "patterns"
      ]

duplicatedFunDef :: Name -> TcM ()
duplicatedFunDef n =
  throwError $ "Duplicated function definition:" ++ pretty n

entailmentError :: [Pred] -> [Pred] -> TcM ()
entailmentError base nonentail =
  tcmError $
    unwords
      [ "Could not deduce",
        pretty nonentail,
        "from",
        if null base then "<empty context>" else pretty base
      ]

rigidVariableError :: [(Tyvar, Ty)] -> TcM ()
rigidVariableError vts =
  tcmError $
    "Cannot unify the following rigid variables with types:"
      ++ (unlines $ map (\(v, t) -> pretty v ++ " with " ++ pretty t) vts)

invalidDefaultInst :: Inst -> TcM ()
invalidDefaultInst p =
  tcmError $ "Cannot have a default instance with a non-type variable as main argument:" ++ pretty p

ambiguousTypeError :: Scheme -> Signature Name -> TcM ()
ambiguousTypeError sch sig =
  tcmError $ unlines ["Ambiguous infered type", pretty sch, "in", pretty sig]

notImplemented :: (HasCallStack, Pretty a) => String -> a -> b
notImplemented funName a = error $ concat [funName, " not implemented yet for ", pretty a]

notImplementedS :: (HasCallStack, Show a) => String -> a -> b
notImplementedS funName a = error $ concat [funName, " not implemented yet for ", show (pShow a)]
