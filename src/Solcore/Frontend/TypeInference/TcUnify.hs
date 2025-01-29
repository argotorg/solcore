module Solcore.Frontend.TypeInference.TcUnify where

import Control.Monad.Except

import Data.List

import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcSubst
import Common.Pretty
import Solcore.Frontend.Pretty.SolcorePretty  hiding((<>))


-- standard unification machinery 

varBind :: MonadError String m => Tyvar -> Ty -> m Subst 
varBind v t
  | t == TyVar v = return mempty
  | v `elem` fv t = infiniteTyErr v t
  | otherwise = return (v +-> t)

isVar :: Ty -> Bool 
isVar (TyVar _) = True 
isVar _ = False 

-- type matching 

match :: MonadError String m => Ty -> Ty -> m Subst 
match (TyCon n ts) (TyCon n' ts') 
  | n == n' = go ts ts' 
    where 
      go [] [] = pure mempty 
      go (t : ts) (t' : ts') 
        = do 
            sl <- match t t' 
            sr <- go ts ts' 
            merge sl sr
      go _ _ = typesMatchListErr ts ts'
match t1@(TyVar v) t 
  | t1 == t = pure mempty 
  | otherwise = pure (v +-> t)
match t1 t2 = typesNotMatch t1 t2

matchTypes :: MonadError String m => [Ty] -> [Ty] -> m Subst 
matchTypes [] [] = pure mempty 
matchTypes (t : ts) (t' : ts') 
  = do 
      s1 <- match t t'
      s2 <- matchTypes ts ts' 
      merge s1 s2 
matchTypes ts ts' = typesMatchListErr ts ts'

matchPred :: MonadError String m => Pred -> Pred -> m Subst 
matchPred (InCls n t ts) (InCls n' t' ts') 
  | n == n' = matchTypes (t : ts) (t' : ts')
  | otherwise = throwError "Classes differ!"


-- most general unifier 

mgu :: MonadError String m => Ty -> Ty -> m Subst 
mgu (TyCon n ts) (TyCon n' ts') 
  | n == n' && length ts == length ts' 
    = solve (zip ts ts') mempty 
mgu (TyVar v) t = varBind v t 
mgu t (TyVar v) = varBind v t 
mgu t1 t2 = typesDoNotUnify t1 t2 

solve :: MonadError String m => [(Ty,Ty)] -> Subst -> m Subst 
solve [] s = pure s 
solve ((t1, t2) : ts) s 
  = do 
      s1 <- mgu (apply s t1) (apply s t2)
      s2 <- solve ts s1 
      pure (s2 <> s1)

unifyTypes :: MonadError String m => [Ty] -> [Ty] -> m Subst 
unifyTypes ts ts' = solve (zip ts ts') mempty

unifyAllTypes :: MonadError String m => [Ty] -> m Subst 
unifyAllTypes [] = pure mempty 
unifyAllTypes (t : ts) 
  = do 
      s1 <- unifyAllTypes ts 
      s2 <- mgu (apply s1 t) (apply s1 t)
      pure (s2 <> s1)

-- composition operator for matching

merge :: MonadError String m => Subst -> Subst -> m Subst
merge s1@(Subst p1) s2@(Subst p2) = if agree then pure (Subst (p1 ++ p2))
                                             else mergeError disagree 
  where
    disagree = foldr step [] (dom p1 `intersect` dom p2)
    step v ac 
      | alphaEq (apply s1 (TyVar v)) (apply s2 (TyVar v)) = ac 
      | otherwise = (apply s1 (TyVar v), apply s2 (TyVar v)) : ac 
    agree = all (\v -> alphaEq (apply s1 (TyVar v)) (apply s2 (TyVar v)))
                (dom p1 `intersect` dom p2)
    dom s = map fst s

mergeError :: MonadError String m => [(Ty, Ty)] -> m a 
mergeError ts = throwError $ unlines $ "Cannot match types:" : ss 
  where 
    ss = map go ts 
    go (x,y) = pretty x ++ " with " ++ pretty y

-- basic error messages 

infiniteTyErr :: MonadError String m => Tyvar -> Ty -> m a 
infiniteTyErr v t 
  = throwError $ unwords ["Cannot construct the infinite type:"
                         , pretty v 
                         , "~"
                         , pretty t
                         ] 

typesNotMatch :: MonadError String m => Ty -> Ty -> m a 
typesNotMatch t1 t2 
  = throwError $ unwords [ "Types do not match:"
                         , pretty t1 
                         , "and"
                         , pretty t2
                         ]

typesMatchListErr :: MonadError String m => [Ty] -> [Ty] -> m a 
typesMatchListErr ts ts' 
  = throwError (errMsg (zip ts ts'))
    where 
      errMsg ps = unwords  ["Types do not match:"] ++ 
                           concatMap tyList ps  
      tyList (t1, t2) = pretty t1 <> " and " <> pretty t2

typesDoNotUnify :: MonadError String m => Ty -> Ty -> m a 
typesDoNotUnify t1 t2 
  = throwError $ unwords [ "Types:"
                         , pretty t1
                         , "and"
                         , pretty t2
                         , "do not unify"
                         ]

rigidVarError :: MonadError String m => Tyvar -> Ty -> m a 
rigidVarError v t 
  = throwError $ unwords ["Cannot unify"
                         , pretty v 
                         , "with"
                         , pretty t 
                         , "since"
                         , pretty v 
                         , "is a rigid type variable."]
