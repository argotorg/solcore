module Solcore.Frontend.TypeInference.TcStmt where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans 

import Data.Generics
import Data.List

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.TcEnv
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
      s <- unify t1 t2 `wrapError` e
      extSubst s
      pure (lhs1 := rhs1, apply s (ps1 ++ ps2), unit)
tcStmt e@(Let n mt me)
  = do
      (me', psf, tf) <- case (mt, me) of
                      (Just t, Just e1) -> do 
                        (e', ps1,t1) <- tcExp e1 
                        s <- match t t1 `wrapError` e
                        pure (Just e', apply s ps1, apply s t1)
                      (Just t, Nothing) -> do 
                        return (Nothing, [], t)
                      (Nothing, Just e) -> do 
                        (e', ps, t1) <- tcExp e 
                        return (Just e', ps, t1)
                      (Nothing, Nothing) -> 
                        (Nothing, [],) <$> freshTyVar
      extEnv n (monotype tf)
      pure (Let (Id n tf) mt me', psf, unit)
tcStmt (StmtExp e)
  = do 
      (e', ps', t') <- tcExp e 
      pure (StmtExp e', ps', t')
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


tcEquations :: [Ty] -> Equations Name -> TcM (Equations Id, [Pred], Ty)
tcEquations ts eqns  
  = do
      (eqns', ps, ts') <- unzip3 <$> mapM (tcEquation ts) eqns
      resTy <- freshTyVar
      mapM_ (unify resTy) ts'
      withCurrentSubst (eqns', concat ps, resTy)

tcEquation :: [Ty] -> Equation Name -> TcM (Equation Id, [Pred], Ty)
tcEquation ts (ps, ss) 
  = withLocalEnv do 
      (ps', res, ts') <- tcPats ts ps
      (ss', pss', t) <- withLocalCtx res (tcBody ss)
      withCurrentSubst ((ps', ss'), pss', t)

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
      st <- askEnv n
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
      pure (Var (Id n t), ps, t)
tcExp e@(Con n es)
  = do
      -- typing parameters 
      (es', pss, ts) <- unzip3 <$> mapM tcExp es 
      -- getting the type from the environment 
      sch <- askEnv n 
      (ps :=> t) <- freshInst sch
      -- unifying infered parameter types
      t' <- freshTyVar
      s <- unify (funtype ts t') t `wrapError` e 
      tn <- typeName (apply s t')
      -- checking if the constructor belongs to type tn 
      checkConstr tn n
      let ps' = concat (ps : pss)
      pure (Con (Id n t) es', apply s ps', apply s t')
tcExp (FieldAccess e n) 
  = do
      -- infering expression type 
      (e', ps,t) <- tcExp e
      -- getting type name 
      tn <- typeName t 
      -- getting field type 
      s <- askField tn n 
      (ps' :=> t') <- freshInst s 
      pure (FieldAccess e' (Id n t'), ps ++ ps', t')
tcExp (Call me n args)
  = tcCall me n args 
tcExp e@(Lam args bd _)
  = do 
      (args', schs, ts') <- tcArgs args 
      (bd',ps,t') <- withLocalCtx schs (tcBody bd)
      s <- getSubst
      let 
          (ps1,t1) = apply s (ps, funtype ts' t')
          e' = everywhere (mkT (applyI s)) (Lam args' bd' (Just t1))
      pure (e', ps1, t1)

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
tcArg (Typed n ty)
  = pure (Typed (Id n ty) ty, (n, monotype ty), ty)

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
      s' <- unify t (foldr (:->) t' ts')
      let ps' = foldr union [] (ps : pss')
          t1 = foldr (:->) t' ts'
      withCurrentSubst (Call Nothing (Id n t1) es', ps', t')
tcCall (Just e) n args 
  = do 
      (e', ps , ct) <- tcExp e
      s <- askEnv n 
      (ps1 :=> t) <- freshInst s
      t' <- freshTyVar
      (es', pss', ts') <- unzip3 <$> mapM tcExp args 
      s' <- unify t (foldr (:->) t' ts')
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

instance Pretty (Param Id) where 
  ppr (Typed (Id n t) _) = ppr n <+> text "::" <+> ppr t
  ppr (Untyped (Id n t)) = ppr n <+> text "::" <+> ppr t

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


-- errors 

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
