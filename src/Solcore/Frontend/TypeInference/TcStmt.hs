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
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcInvokeGen hiding (vars, tyFromParam, schemeFromSig)
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives

import Language.Yul

import Text.PrettyPrint.HughesPJ

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
                        s <- match t1 t `wrapError` e
                        extSubst s
                        pure (Just e', apply s ps1, apply s t1)
                      (Just t, Nothing) -> do
                        return (Nothing, [], t)
                      (Nothing, Just e) -> do
                        (e', ps, t1) <- tcExp e
                        return (Just e', ps, t1)
                      (Nothing, Nothing) ->
                        (Nothing, [],) <$> freshTyVar
      extEnv n (monotype tf)
      let e' = Let (Id n tf) (Just tf) me'
      pure (e', [], unit)
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
      newBinds <- tcYulBlock yblk
      let word' = monotype word
      mapM_ (flip extEnv word') newBinds
      pure (Asm yblk, [], unit)

isGeneratedType :: Ty -> TcM Bool
isGeneratedType (TyVar _) = pure False
isGeneratedType (TyCon n _)
  = ((elem n) . map dataName . Map.elems) <$> gets uniqueTypes

tcEquations :: [Ty] -> Equations Name -> TcM (Equations Id, [Pred], Ty)
tcEquations ts eqns
  = do
      resTy <- freshTyVar
      (eqns', ps, ts') <- unzip3 <$> mapM (tcEquation resTy ts) eqns
      s <- getSubst 
      withCurrentSubst (eqns', concat ps, resTy)

tcEquation :: Ty -> [Ty] -> Equation Name -> TcM (Equation Id, [Pred], Ty)
tcEquation retTy ts eqn@(ps, ss)
  = withLocalEnv do
      (ps', res, ts') <- tcPats ts ps
      (ss', pss', t) <- withLocalCtx res (tcBody ss)
      s <- unify t retTy `wrapError` eqn 
      withCurrentSubst ((ps', ss'), pss', apply s t)

tcPats :: [Ty] -> [Pat Name] -> TcM ([Pat Id], [(Name,Scheme)], [Ty])
tcPats ts ps
  | length ts /= length ps = wrongPatternNumber ts ps
  | otherwise = do
      (ps', ctxs, ts') <- unzip3 <$> mapM (\(t, p) -> tcPat t p)
                                          (zip ts ps)
      pure (ps', concat ctxs, ts')


tcPat :: Ty -> Pat Name -> TcM (Pat Id, [(Name, Scheme)], Ty)
tcPat t p
  = do
      (p', t', pctx) <- tiPat p
      s <- unify t t'
      let pctx' = map (\ (n,t1) -> (n, monotype $ apply s t1)) pctx
      pure (p', pctx', apply s t')

tiPat :: Pat Name -> TcM (Pat Id, Ty, [(Name, Ty)])
tiPat (PVar n)
  = do
      t <- freshTyVar
      let v = PVar (Id n t)
      pure (v, t, [(n,t)])
tiPat p@(PCon n ps)
  = do
      -- typing parameters
      (ps1, ts, lctxs) <- unzip3 <$> mapM tiPat ps
      -- asking type from environment
      st <- askEnv n `wrapError` p
      (ps' :=> tc) <- freshInst st
      tr <- freshTyVar
      s <- unify tc (funtype ts tr) `wrapError` p
      let t' = apply s tr
      tn <- typeName t'
      checkConstr tn n
      let lctx' = map (\(n',t') -> (n', apply s t')) (concat lctxs)
      pure (PCon (Id n tc) ps1, apply s tr, lctx')
tiPat PWildcard
  = f <$> freshTyVar
    where
      f t = (PWildcard, t, [])
tiPat (PLit l)
  = do
      t <- tcLit l
      pure (PLit l, t, [])

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
      -- checks if it is a function name, and return 
      -- its corresponding unique type 
      gen <- gets generateDefs
      r <- lookupFunAbs n
      let
        (args,ret) = splitTy t
        args' = tupleTyFromList args
        mkCon (DataTy nt vs [(Constr n _)])
          = let
              t1 = TyCon nt [args', ret]
            in (Con (Id n t1) [], t1)
        p = if gen then (Var (Id n t), t) 
            else maybe (Var (Id n t), t) mkCon r
      pure (fst p, ps, snd p)
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
      pure (e, apply s ps', apply s t')
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
      pure (FieldAccess (Just e') (Id n t'), ps ++ ps', t')
tcExp ex@(Call me n args)
  = do
      gen <- gets generateDefs
      let qn = QualName invokableName "invoke"
          args' = [Var n, indirectArgs args]
      isDirect <- isDirectCall n
      if gen && isDirect then do
        tcCall me n args `wrapError` ex
      else do
        if isDirect then tcCall me n args `wrapError` ex
          else
            tcCall me qn args' `wrapError` ex
-- tcExp e@(Lam args bd _)
--   = do
--       (args', schs, ts') <- tcArgs args
--       (bd',ps,t') <- withLocalCtx schs (tcBody bd)
--       s <- getSubst
--       let ps1 = apply s ps
--           ts1 = apply s (map unskol ts')
--           t1 = apply s (unskol t')
--           vs = fv ps1 `union` fv t' `union` fv ts1
--           ty = funtype ts1 t1
--       gen <- gets generateDefs
--       (lfun, (e', ty1)) <- createLambdaImpl ps1 args' vs (ts1,t1) bd'
--       pure (e', ps1, ty1)
tcExp e1@(TyExp e ty)
  = do
      kindCheck ty `wrapError` e1
      (e', ps, ty') <- tcExp e
      let ty1 = skolemize ty
      s <- match ty' ty  `wrapError` e1
      extSubst s
      pure (TyExp e' ty, apply s ps, ty)

unskol :: Ty -> Ty 
unskol (TyVar (TVar v _)) = TyVar (TVar v False)
unskol (TyCon n ts) = TyCon n (map unskol ts)

-- building indirect function call arguments 

indirectArgs :: [Exp Name] -> Exp Name 
indirectArgs [] = Con (Name "pair") []
indirectArgs [e] = e 
indirectArgs (e : es) = epair e (indirectArgs es)
  where 
    epair e1 e2 = Con (Name "pair") [e1, e2]

-- need to retrieve generated stuff 
-- from a lambda after first desugaring step.

createLambdaImpl :: [Pred] ->
                    [Param Id] ->
                    [Tyvar] ->
                    ([Ty],Ty) ->
                    Body Id -> TcM (FunDef Name, (Exp Id, Ty))
createLambdaImpl ps args vs (argTys, rty) bdy
  = do
      c <- incCounter
      funs <- Map.keys <$> gets uniqueTypes
      let n = Name $ "lambda_impl" ++ show c
          vs' = filter (\i -> idName i `notElem` funs) (vars bdy \\ vars args)
      (v,c',ctr,ty) <- createUniqueType vs' n (argTys, rty)
      s <- getSubst
      (sig',npp) <- createLambdaSig n ps (apply s args) vs rty ty vs'
      bdy' <- createLambdaBody ty ctr vs' bdy npp
      s <- getSubst
      let
          fd = everywhere (mkT (applyI s)) (FunDef sig' bdy')
      pure (fd, (c', ty))

createLambdaSig :: Name ->
                   [Pred] ->
                   [Param Id] ->
                   [Tyvar] ->
                   Ty ->
                   Ty ->
                   [Id] -> TcM (Signature Name, Param Name)
createLambdaSig n ps args vs rty ty ids
  = do
      np <- freshName
      let npp = Typed np ty
          args1 = map paramIdToName args
          args' = if null ids then args1 else npp : args1
      pure (Signature vs ps n args' (Just rty), npp)

paramIdToName :: Param Id -> Param Name
paramIdToName (Typed (Id n _) t) = Typed n t
paramIdToName (Untyped (Id n t)) = Typed n t

createLambdaBody :: Ty ->
                    Constr ->
                    [Id] ->
                    Body Id ->
                    Param Name ->
                    TcM (Body Name)
createLambdaBody ty ctr ids bdy (Typed pn _)
  | null ids = pure (erase bdy)
  | otherwise
    = do
        pats <- freshPat ctr ids
        let bdy' = erase bdy
        pure [Match [Var pn] [(pats, bdy')]]

freshPat :: Constr -> [Id] -> TcM [Pat Name]
freshPat (Constr n ts) ids
  = do
      let nps = map (PVar . idName) ids
      pure [PCon n nps]

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

createUniqueType :: [Id] -> Name -> ([Ty], Ty) -> TcM (Exp Id, Exp Id, Constr, Ty)
createUniqueType ids n (argTys, retTy)
     = do
        m <- incCounter
        argName <- freshName
        retName <- freshName
        let
            argVar = TVar argName False
            retVar = TVar retName False
            nt = Name $ "t_" ++ pretty n ++ show m
            dc = Constr nt (map idType ids)
            dt = DataTy nt [argVar, retVar] [dc]
            argTy' = tupleTyFromList argTys
            tc = TyCon nt [argTy', retTy]
        writeDataTy dt 
        checkDataType dt
        addUniqueType n dt
        pure (Var (Id n tc), Con (Id nt tc) (map Var ids), dc, tc)

applyI :: Subst -> Ty -> Ty
applyI s = apply s

tcArgs :: [Param Name] -> TcM ([Param Id], [(Name, Scheme)], [Ty])
tcArgs params
  = do
      res <- mapM tcArg params
      pure (unzip3 res)

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

skolemize :: Ty -> Ty
skolemize (TyVar (TVar n _)) = TyVar (TVar n True)
skolemize (TyCon n ts) = TyCon n (map skolemize ts)

-- type checking a single bind
-- create tcSignature which should return the 
-- function type together with its parameter types

tcSignature :: Signature Name -> TcM ( (Name, Scheme)
                                     , [(Name, Scheme)]
                                     , [Ty]
                                     )
tcSignature sig@(Signature _ ctx n ps rt)
  = do 
      (ps', pschs, ts) <- tcArgs ps 
      t <- maybe freshTyVar pure rt
      msch <- maybeAskEnv n
      let sch = maybe (monotype $ funtype ts t) id msch
      pure ((n, sch), pschs, ts)

tcFunDef :: FunDef Name -> TcM (FunDef Id, [Pred], Ty)
tcFunDef d@(FunDef sig bd)
  = withLocalEnv do
      -- checking if the function isn't defined
      ((n,sch), pschs, ts) <- tcSignature sig 
      let lctx = (n,sch) : pschs
      (bd', ps1, t') <- withLocalCtx lctx (tcBody bd) `wrapError` d
      (ps :=> t) <- freshInst sch
      t1 <- withCurrentSubst (foldr (:->) t' ts)
      nps <- withCurrentSubst (ps ++ ps1)
      ps2 <- reduceContext nps `wrapError` d
      s1 <- getSubst
      s' <- match (apply s1 $ unskol t) (apply s1 $ unskol t1) `wrapError` d
      extSubst s'
      s <- getSubst 
      sch'@(Forall svs (sps :=> st)) <- generalize (ps2, apply s t) `wrapError` d
      let sig2 = elabSignature sig sch'
      gen <- gets generateDefs
      when gen (generateDecls (FunDef sig2 bd', sch')) 
      info [">>> Infered type for ", pretty (sigName sig), " is ", pretty sch']
      pure (apply s $ FunDef sig2  bd', apply s ps2, apply s t1)

-- update types in signature 

elabSignature :: Signature Name -> Scheme -> Signature Id
elabSignature sig (Forall vs (ps :=> t)) 
  = Signature vs ps (sigName sig) params' ret 
    where 
      (ts, t') = splitTy t 
      params' = zipWith elabParam ts (sigParams sig)
      ret = Just $ if null params' then t else t' 
      elabParam t1 (Typed n _) = Typed (Id n t1) t1 
      elabParam t1 (Untyped n) = Typed (Id n t1) t1 

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
      -- checking if the function is previously defined
      addFunctionName n
      te <- gets ctx
      gen <- gets generateDefs
      when (Map.member n te && gen) (duplicatedFunDef n) `wrapError` sig

-- typing instances

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
        , sigReturn = Just (skolemize ret)
        }
    where 
      paramType _ (Typed n t) = Typed n (skolemize t)
      paramType t (Untyped n) = Typed n (skolemize t)

-- checking instances and adding them in the environment

checkInstances :: [Instance Name] -> TcM ()
checkInstances = mapM_ checkInstance 

checkInstance :: Instance Name -> TcM ()
checkInstance idef@(Instance ctx n ts t funs)
  = do
      let ipred = InCls n t ts
      -- checking the coverage condition 
      insts <- askInstEnv n `wrapError` ipred
      -- checkOverlap ipred insts `wrapError` idef
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

tcYulBlock :: YulBlock -> TcM [Name]
tcYulBlock yblk
  = withLocalEnv (concat <$> mapM tcYulStmt yblk)

tcYulStmt :: YulStmt -> TcM [Name]
tcYulStmt (YAssign ns e)
  = do
      -- do not define names
      tcYulExp e
      pure []
tcYulStmt (YBlock yblk)
  = do
      _ <- tcYulBlock yblk
      -- names defined in should not return
      pure []
tcYulStmt (YLet ns (Just e))
  = do
      tcYulExp e
      mapM_ (flip extEnv mword) ns
      pure ns
tcYulStmt (YExp e)
  = do
      tcYulExp e
      pure []
tcYulStmt (YIf e yblk)
  = do
      tcYulExp e
      _ <- tcYulBlock yblk
      pure []
tcYulStmt (YSwitch e cs df)
  = do
      tcYulExp e
      tcYulCases cs
      tcYulDefault df
      pure []
tcYulStmt (YFor init e bdy upd)
  = do
      ns <- tcYulBlock init
      withLocalEnv do
        mapM_ (flip extEnv mword) ns
        tcYulExp e
        tcYulBlock bdy
        tcYulBlock upd
tcYulStmt _ = pure []

tcYulExp :: YulExp -> TcM Ty
tcYulExp (YLit l)
  = tcYLit l
tcYulExp (YIdent v)
  = do
      sch <- askEnv v
      (_ :=> t) <- freshInst sch
      unless (t == word) (invalidYulType v)
      pure t
tcYulExp (YCall n es)
  = do
      sch <- askEnv n
      (_ :=> t) <- freshInst sch
      ts <- mapM tcYulExp es
      t' <- freshTyVar
      unless (all (== word) ts) (invalidYulType n)
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

instance HasType (Exp Id) where
  apply s (Var v) = Var (apply s v)
  apply s (Con n es)
    = Con (apply s n) (apply s es)
  apply s (FieldAccess e v)
    = FieldAccess (apply s e) (apply s v)
  apply s (Call m v es)
    = Call (apply s <$> m) (apply s v) (apply s es)
  apply s (Lam ps bd mt)
    = Lam (apply s ps) (apply s bd) (apply s <$> mt)
  apply _ e = e

  fv (Var v) = fv v
  fv (Con n es)
    = fv n `union` fv es
  fv (FieldAccess e v)
    = fv e `union` fv v
  fv (Call m v es)
    = maybe [] fv m `union` fv v `union` fv es
  fv (Lam ps bd mt)
    = fv ps `union` fv bd `union` maybe [] fv mt

instance HasType (Stmt Id) where
  apply s (e1 := e2)
    = (apply s e1) := (apply s e2)
  apply s (Let v mt me)
    = Let (apply s v)
          (apply s <$> mt)
          (apply s <$> me)
  apply s (StmtExp e)
    = StmtExp (apply s e)
  apply s (Return e)
    = Return (apply s e)
  apply s (Match es eqns)
    = Match (apply s es) (apply s eqns)
  apply _ s
    = s

  fv (e1 := e2)
    = fv e1 `union` fv e2
  fv (Let v mt me)
    = fv v `union` (maybe [] fv mt)
           `union` (maybe [] fv me)
  fv (StmtExp e) = fv e
  fv (Return e) = fv e
  fv (Match es eqns)
    = fv es `union` fv eqns
  fv (Asm blk) = []

instance HasType (Pat Id) where
  apply s (PVar v) = PVar (apply s v)
  apply s (PCon v ps)
    = PCon (apply s v) (apply s ps)
  apply _ p = p

  fv (PVar v) = fv v
  fv (PCon v ps) = fv v `union` fv ps

-- determining free variables

class Vars a where
  vars :: a -> [Id]

instance Vars a => Vars [a] where
  vars = foldr (union . vars) []

instance Vars (Pat Id) where
  vars (PVar v) = [v]
  vars (PCon _ ps) = vars ps
  vars _ = []

instance Vars (Param Id) where
  vars (Typed n _) = [n]
  vars (Untyped n) = [n]

instance Vars (Stmt Id) where
  vars (e1 := e2) = vars [e1,e2]
  vars (Let _ _ (Just e)) = vars e
  vars (Let _ _ _) = []
  vars (StmtExp e) = vars e
  vars (Return e) = vars e
  vars (Match e eqns) = vars e `union` vars eqns

instance Vars (Equation Id) where
  vars (_, ss) = vars ss

instance Vars (Exp Id) where
  vars (Var n) = [n]
  vars (Con _ es) = vars es
  vars (FieldAccess Nothing _) = []
  vars (FieldAccess (Just e) _) = vars e
  vars (Call (Just e) n es) = [n] `union` vars (e : es)
  vars (Call Nothing n es) = [n] `union` vars es
  vars (Lam ps bd _) = vars bd \\ vars ps
  vars (TyExp e _) = vars e
  vars _ = []

-- erasing Id's

class Erase a where
  type EraseRes a
  erase :: a -> EraseRes a

instance Erase a => Erase [a] where
  type EraseRes [a] = [EraseRes a]
  erase = map erase

instance Erase a => Erase (Maybe a) where
  type EraseRes (Maybe a) = Maybe (EraseRes a)
  erase Nothing = Nothing
  erase (Just x) = Just (erase x)

instance (Erase a, Erase b) => Erase (a,b) where
  type EraseRes (a,b) = (EraseRes a, EraseRes b)

  erase (x, y) = (erase x, erase y)

instance Erase (Stmt Id) where
  type EraseRes (Stmt Id) = Stmt Name

  erase (e1 := e2)
    = (erase e1) := (erase e2)
  erase (Let n mt me)
    = Let (idName n) mt (erase me)
  erase (StmtExp e)
    = StmtExp (erase e)
  erase (Return e)
    = Return (erase e)
  erase (Match es eqns)
    = Match (erase es) (erase eqns)
  erase (Asm blk)
    = Asm blk

instance Erase (Exp Id) where
  type EraseRes (Exp Id) = Exp Name

  erase (Var v)
    = Var (idName v)
  erase (Con n es)
    = Con (idName n) (map erase es)
  erase (FieldAccess me n)
    = FieldAccess (erase me) (idName n)
  erase (Call me n es)
    = Call (erase me) (idName n) (erase es)
  erase (Lam ps bd mt)
    = Lam (erase ps) (erase bd) mt
  erase (TyExp e t)
    = TyExp (erase e) t
  erase (Lit l) = Lit l

instance Erase (Param Id) where
  type EraseRes (Param Id) = Param Name

  erase (Typed n t)
    = Typed (idName n) t
  erase (Untyped n)
    = Untyped (idName n)

instance Erase (Pat Id) where
  type EraseRes (Pat Id) = Pat Name

  erase (PVar n)
    = PVar (idName n)
  erase (PCon n ps)
    = PCon (idName n) (erase ps)
  erase PWildcard
    = PWildcard
  erase (PLit l)
    = PLit l

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

invalidYulType :: Name -> TcM a
invalidYulType (Name n)
  = throwError $ unlines ["Yul values can only be of word type:", n]

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
