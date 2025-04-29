module Solcore.Frontend.TypeInference.TcStmt where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State
import Control.Monad.Trans

import Data.Generics hiding (Constr)
import Data.List
import qualified Data.Map as Map
import Data.Maybe

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Erase
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
      extSubst s
      pure (lhs1 := rhs1, apply s $ ps1 ++ ps2, unit)
tcStmt e@(Let n mt me)
  = do
      (me', psf, tf) <- case (mt, me) of
                      (Just t, Just e1) -> do
                        (e', ps1, t1) <- tcExp e1
                        kindCheck t1 `wrapError` e
                        s <- checkTyInst (fv t) t t1 `wrapError` e
                        extSubst s
                        withCurrentSubst (Just e', ps1, t)
                      (Just t, Nothing) -> do
                        return (Nothing, [], t)
                      (Nothing, Just e) -> do
                        (e', ps, t1) <- tcExp e
                        return (Just e', ps, t1)
                      (Nothing, Nothing) ->
                        (Nothing, [],) <$> freshTyVar
      (ds,rs) <- splitContext psf (fv mt)
      extEnv n (Forall [] (rs :=> tf))
      let e' = Let (Id n tf) (Just tf) me'
      withCurrentSubst (e', ds, unit)
tcStmt (StmtExp e)
  = do
      (e', ps', t') <- tcExp e
      pure (StmtExp e', ps', unit)
tcStmt m@(Return e)
  = do
      (e', ps, t) <- tcExp e
      pure (Return e', ps, t)
tcStmt (Match es eqns)
  = do
      (es', pss', ts') <- unzip3 <$> mapM tcExp es
      (eqns', pss1, resTy) <- tcEquations ts' eqns
      withCurrentSubst (Match es' eqns', concat (pss1 : pss'), resTy)
tcStmt s@(Asm yblk)
  = withLocalCtx yulPrimOps $ do
      (newBinds,t) <- tcYulBlock yblk
      let word' = monotype word
      mapM_ (flip extEnv word') newBinds
      pure (Asm yblk, [], t)


checkTyInst :: [Tyvar] -> -- Skolem constants 
               Ty -> -- annotated type 
               Ty -> -- inferred type
               TcM Subst
checkTyInst vs ann inf 
  = do
      s@(Subst x) <- match inf ann `catchError` 
          (\ _ -> tcmError $ unlines ["The annotated type and inferred type do not match."
                                     , "The annotated type is:"
                                     , "   " ++ (pretty $ rename ann)
                                     , "The inferred type is:"
                                     , "   " ++ (pretty $ rename inf)
                                     ])
      let rigids = filter (\ (v, t) -> v `elem` vs && isTyCon t) x
      unless (null rigids) $ rigidVariableError rigids 
      pure s 


tcEquations :: [Ty] -> Equations Name -> TcM (Equations Id, [Pred], Ty)
tcEquations ts eqns
  = do
      resTy <- freshTyVar
      (eqns', ps, ts') <- unzip3 <$> mapM (tcEquation resTy ts) eqns
      withCurrentSubst (eqns', concat ps, resTy)

tcEquation :: Ty -> [Ty] -> Equation Name -> TcM (Equation Id, [Pred], Ty)
tcEquation retTy ts eqn@(ps, ss)
  = withLocalEnv do
      (ps', ts', res) <- tcPats ts ps
      (ss', pss', t) <- withLocalCtx res (tcBody ss)
      s <- unify t retTy `wrapError` eqn 
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
      (ps' :=> tc) <- freshInst st
      s <- unify tc (funtype ts t) `wrapError` p
      let t' = apply s t
      tn <- typeName t'
      checkConstr tn n
      let lctx' = map (\(n',t') -> (n', apply s t')) (concat lctxs)
      pure (PCon (Id n tc) ps1, apply s t, apply s lctx')
tcPat t PWildcard
  = pure (PWildcard, t, [])
tcPat t' (PLit l)
  = do
      t <- tcLit l
      s <- unify t t' 
      pure (PLit l, apply s t, [])

-- type inference for expressions

tcLit :: Literal -> TcM Ty
tcLit (IntLit _) = return word
tcLit (StrLit _) = return string

tcExp :: Infer Exp
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
        let
          mkCon (DataTy nt vs [(Constr n _)])
            = let
                t1 = TyCon nt (map TyVar vs)
              in (Con (Id n t1) [], t1)
          p = maybe (Var (Id n t), t) mkCon r
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
          e = Con (Id n t) es'
      withCurrentSubst (e, ps', t')
tcExp (FieldAccess Nothing n)
  = throwError "Not Implemented yet!"
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
       (args', schs, ts', vs') <- tcArgs args
       (bd', ps, t') <- withLocalCtx schs (tcBody bd)
       s <- getSubst
       let ps1 = apply s ps
           ts1 = apply s ts'
           t1 = apply s t'
           vs = fv ps1 `union` fv t1 `union` fv ts1
           ty = funtype ts1 t1
       noDesugarCalls <- getNoDesugarCalls
       if noDesugarCalls then withCurrentSubst (Lam args' bd' (Just t1), ps1, ty)
       else do
         (e, t) <- closureConversion vs (apply s args') (apply s bd') ps1 ty
         withCurrentSubst (e, ps1, t)
tcExp e1@(TyExp e ty)
  = do
      kindCheck ty `wrapError` e1
      (e', ps, ty') <- tcExp e 
      s <- checkTyInst (fv ty) ty ty' `wrapError` e1
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
        fun <- createClosureFreeFun fn args bdy ps ty 
        let fun1 = erase fun
        st <- get 
        clearSubst 
        (fun', _, qs) <- tcFunDef False fun1 
        sch <- generalize (ps, ty)
        put st 
        (udt@(DataTy dn vs _), instd) <- generateDecls (fun', sch)
        let t = TyCon dn (TyVar <$> vs)
        writeFunDef fun'
        writeDataTy udt
        checkDataType udt 
        checkInstance instd
        extEnv fn sch 
        instd' <- tcInstance instd
        writeInstance instd'
        pure (Con (Id dn t) [], t) 
      else do 
        (cdt, e', t') <- createClosureType free vs ty
        addUniqueType fn cdt 
        (fun, sch) <- createClosureFun fn free cdt args bdy ps ty
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
      let 
          (args,ret) = splitTy ty 
          argTy = tupleTyFromList args
          dn = Name $ "t_closure" ++ show i 
          ts = map idType ids
          ns = map Var ids
          vs' = union (fv ts) vs 
          ty' = TyCon dn (TyVar <$> vs')
          cid = Id dn (funtype ts ty')
      pure (DataTy dn vs' [Constr dn ts], Con cid ns, ty')

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
      let cName = Name $ "env" ++ show j
          cParam = Typed (Id cName ct) ct 
          args' = cParam : args 
          (_,retTy) = splitTy ty
          vs' = union (fv ct) (fv ps)
          ty' = ct :-> ty 
          sig = Signature vs' ps fn args' (Just retTy)
      bdy' <- createClosureBody cName cdt free bdy
      sch <- generalize (ps, ty')
      pure (FunDef sig bdy', sch)
 

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
        vs = fv ty `union` fv ps 
        sig = Signature vs ps fn args (Just retTy)
      pure (FunDef sig bdy)

tcArgs :: [Param Name] -> TcM ([Param Id], [(Name, Scheme)], [Ty], [Tyvar])
tcArgs params
  = do
      (ps, schs, ts, vss) <- unzip4 <$> mapM tcArg params
      pure (ps, schs, ts, concat vss)

tcArg :: Param Name -> TcM (Param Id, (Name, Scheme), Ty, [Tyvar])
tcArg (Untyped n)
  = do
      v <- freshTyVar
      let ty = monotype v
      -- no Skolem constants here!
      pure (Typed (Id n v) v, (n, ty), v, [])
tcArg a@(Typed n ty)
  = do
      ty1 <- kindCheck ty `wrapError` a
      pure (Typed (Id n ty1) ty1, (n, monotype ty1), ty1, fv ty1)

-- type checking a single bind
-- create tcSignature which should return the 
-- function type together with its parameter types

tcSignature :: Bool -> Signature Name -> TcM ( (Name, Scheme)
                                             , [(Name, Scheme)]
                                             , [Ty]
                                             , [Tyvar]
                                             ) 
tcSignature b (Signature vs ps n args rt)
  = do 
      vs0 <- mapM (const freshVar) vs 
      let s = Subst (zip vs (map TyVar vs0)) 
          args0 = apply s args 
          rt0 = apply s rt 
          ps0 = apply s ps
      (args', pschs, ts, vs') <- tcArgs args0
      t' <- maybe freshTyVar pure rt0 
      sch <- generalize (ps0, funtype ts t')
      pure ((n, sch), pschs, ts, vs')

hasAnn :: Signature Name -> Bool 
hasAnn (Signature vs ps n args rt) 
  = any isAnn args && isJust rt 
    where 
      isAnn (Typed _ t) = True 
      isAnn _ = False 


-- boolean flag indicates if the assumption for the 
-- function should be included in the context. It 
-- is necessary to not include the type of instance 
-- functions which should have the type of its underlying 
-- type class definition.

tcFunDef :: Bool -> FunDef Name -> TcM (FunDef Id, Scheme, [Pred])
tcFunDef incl d@(FunDef sig bd)
  = withLocalEnv do
     info [">> Starting the typing of:", pretty sig]
     ((n,sch), pschs, ts, vs') <- tcSignature incl sig 
     let lctx = if incl then (n,sch) : pschs else pschs
     (bd', ps1, t') <- withLocalCtx lctx (tcBody bd) `wrapError` d
     (ps2 :=> ann) <- freshInst sch 
     let ty = funtype ts t'
     s <- getSubst
     -- checking if the constraints are valid
     ps3 <- withCurrentSubst ps1 -- filterM (\ p -> not <$> entails (apply s ps2) p) (apply s ps1)
     vs <- getEnvFreeVars
     (ds, rs) <- splitContext ps3 (vs `union` fv pschs)
     -- checking if the annotated type is a valid instance of the infered type.
     when (hasAnn sig) $ do 
        s' <- checkTyInst (fv pschs `union` fv (sigReturn sig)) ann ty `wrapError` d 
        extSubst s' 
        return ()
     -- checking constraint provability for annotated types.
     when (hasAnn sig && null (sigContext sig) && not (isValid rs) && incl) $ do 
      tcmError $ unlines [ "Could not deduce:"
                         ,  pretty rs 
                         , "from:" 
                         , pretty sig
                         ]
     sch' <- generalize (rs, ty)
     sig2 <- elabSignature incl sig sch' `wrapError` d
     info [">> Finished typing of:", pretty sig2]
     fd <- withCurrentSubst (FunDef sig2 bd')
     withCurrentSubst (fd, sch', ds)

-- only invokable constraints can be inserted freely 

isValid :: [Pred] -> Bool
isValid rs = null rs || all isInvoke rs 
  where 
    isInvoke (InCls n _ _) 
      = n == invokableName

-- update types in signature 

elabSignature :: Bool -> Signature Name -> Scheme -> TcM (Signature Id)
elabSignature incl sig sch@(Forall vs (ps :=> t)) 
  = do
      let 
        params = sigParams sig 
        nparams = length params 
        (ts, t') = splitTy t
        (ts', rs) = splitAt nparams ts
        ctx = sigContext sig
      params' <- zipWithM (elabParam incl) ts' params
      let 
        ret = Just $ if null params' then t else (funtype rs t')
        vs' = fv params' `union` fv ret `union` fv ps 
      withCurrentSubst $ Signature vs' ps (sigName sig) params' ret 

elabParam :: Bool -> Ty -> Param Name -> TcM (Param Id)
elabParam False t (Typed n _) = pure $ Typed (Id n t) t 
elabParam False t (Untyped n) = pure $ Typed (Id n t) t
elabParam _ t (Typed n t') 
  = do
      s <- match t t' 
      extSubst s 
      withCurrentSubst $ Typed (Id n t) t
elabParam _ t (Untyped n)
  = pure $ Typed (Id n t) t

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

-- typing instances

tcInstance :: Instance Name -> TcM (Instance Id)
tcInstance idecl@(Instance d ctx n ts t funs) 
  = do
      checkCompleteInstDef n (map (sigName . funSignature) funs) 
      funs' <- buildSignatures n ts t funs `wrapError` idecl
      (funs1, schss, pss') <- unzip3 <$> mapM (tcFunDef False) funs' `wrapError` idecl 
      let 
        qts1 = map (schemeFromSignature . funSignature) funs'
        qts2 = map (schemeFromSignature . funSignature) funs1
      -- check if types are proper instances of the most general type infered.
      s1 <- match qts1 qts2
      extSubst s1
      let
        instd = Instance d ctx n ts t funs1
      withCurrentSubst instd

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
      sm <- match cpred (InCls n t ts)
      let qname m = QualName n (pretty m)
      schs <- mapM (askEnv . qname . sigName . funSignature) funs
      let  
          app (Forall _ qt) = apply sm qt 
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

schemeFromSignature :: Signature a -> Qual Ty  
schemeFromSignature (Signature vs ps n args ret)
  = ps :=> (funtype (map (\ (Typed _ t) -> t) args) (fromJust ret))

-- checking instances and adding them in the environment

checkInstances :: [Instance Name] -> TcM ()
checkInstances = mapM_ checkInstance 

checkInstance :: Instance Name -> TcM ()
checkInstance idef@(Instance d ctx n ts t funs)
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
      mapM_ (checkMethod ipred) funs
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
      _ <- liftEither (match p ih) `wrapError` ih
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
      envVars <- getEnvFreeVars
      (ps1,t1) <- withCurrentSubst (ps,t)
      ps2 <- reduce ps1 
      t2 <- withCurrentSubst t1
      s <- getSubst 
      let 
          ps3 = apply s ps2 
          t3 = apply s t2 
          vs = fv (ps3,t3)
          sch = Forall (vs \\ envVars) (ps3 :=> t3)
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
      s' <- unify (funtype ts' t') t
      extSubst s'
      let ps' = apply s' $ foldr union [] (ps : pss')
          t1 = apply s' (funtype ts' t')
      withCurrentSubst (Call Nothing (Id n t1) es', ps', apply s' t')
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
      withLocalEnv do
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
rename t = let vs = fv t 
               s = Subst $ zip vs (map (TyVar . TVar) namePool)
           in apply s t

-- errors

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
