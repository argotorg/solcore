module Solcore.Frontend.TypeInference.TcUnify where

import Control.Monad 
import Control.Monad.Except
import Control.Monad.Reader 

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
  | otherwise = do 
      return (v +-> t)

isTyCon :: Ty -> Bool 
isTyCon (TyCon _ _) = True 
isTyCon _ = False

-- type matching 

class Match a where 
  match :: MonadError String m => a -> a -> m Subst 

instance Match Ty where 
  match (TyCon n ts) (TyCon n' ts') 
    | n == n' = match ts ts' 
  match t1@(TyVar v) t 
    | t1 == t = pure mempty 
    | otherwise = do 
        pure (v +-> t)
  match t1 t2 = typesNotMatch t1 t2

instance (Pretty a, Match a) => Match [a] where 
  match [] [] = pure mempty 
  match (t : ts) (t' : ts') 
    = do 
        s1 <- match t t'
        s2 <- match ts ts' 
        merge s1 s2 
  match ts ts' = typesMatchListErr (map pretty ts) (map pretty ts')

instance Match Pred where 
  match (InCls n t ts) (InCls n' t' ts') 
    | n == n' = match (t : ts) (t' : ts')
    | otherwise = throwError "Classes differ!"

instance (HasType a, Match a) => Match (Qual a) where 
  match (ps :=> t) (ps' :=> t') 
    = do 
        s1 <- match t t'
        s2 <- match (apply s1 t) (apply s1 t')
        merge s1 s2 

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

typesMatchListErr :: MonadError String m => [String] -> [String] -> m a 
typesMatchListErr ts ts' 
  = throwError (errMsg (zip ts ts'))
    where 
      errMsg ps = unwords  ["Types do not match:"] ++ 
                           concatMap tyList ps  
      tyList (t1, t2) = t1 <> " and " <> t2

typesDoNotUnify :: MonadError String m => Ty -> Ty -> m a 
typesDoNotUnify t1 t2 
  = throwError $ unwords [ "Types:"
                         , pretty t1
                         , "and"
                         , pretty t2
                         , "do not unify"
                         ]


