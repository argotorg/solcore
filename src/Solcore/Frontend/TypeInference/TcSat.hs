module Solcore.Frontend.TypeInference.TcSat where

import Control.Monad
import Control.Monad.Except
import Control.Monad.Trans
import Data.List
import Data.Maybe
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax hiding (gen)
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Pipeline.Options

sat :: [Pred] -> TcM [Subst]
sat ps =
  do
    n <- askMaxRecursionDepth
    satI n ps

satI :: Int -> [Pred] -> TcM [Subst]
satI 0 p =
  throwError $
    unwords
      [ "Could not deduce:",
        pretty p,
        "because the solver exceeded the max number of iterations!"
      ]
satI n [] = pure [mempty] -- rule SEmpty
satI n [p] = satOne n p -- rule SInst
satI n (p : ps) =
  do
    -- rule SConj
    ss0 <- satOne n p
    ss1 <- mapM (satI (n - 1)) [apply s ps | s <- ss0]
    pure $ [s' <> s | s <- ss0, s1 <- ss1, s' <- s1]

-- rule SInst

satOne :: Int -> Pred -> TcM [Subst]
satOne n p = do
  -- rule Inst
  delta <- sats p
  when (null delta) $
    throwError $
      unwords ["There is no instance to satisfy:", pretty p]
  ss <- mapM (\(s, q) -> satI (n - 1) q) delta
  foldM (step n p) [mempty] delta

step :: Int -> Pred -> [Subst] -> (Subst, [Pred]) -> TcM [Subst]
step 0 p _ _ =
  throwError $
    unwords
      [ "Could not deduce:",
        pretty p,
        "because the solver exceeded the max number of iterations!"
      ]
step n p sacc (s, ps) =
  do
    ss <- liftM (map (s <>)) (satI (n - 1) ps)
    return [s' <> s1 | s' <- ss, s1 <- sacc]

-- function sats

sats :: Pred -> TcM [(Subst, [Pred])]
sats p@(InCls c t ts) =
  do
    insts <- askInstEnv c
    catMaybes <$> mapM (gen t) insts
sats p = tcmError $ "Invalid constraint:" ++ pretty p

gen :: Ty -> Inst -> TcM (Maybe (Subst, [Pred]))
gen t k@(ps :=> h@(InCls _ t' _)) =
  do
    r <- defaultM (mgu t t')
    case r of
      Just s -> pure $ Just (s, apply s ps)
      Nothing -> pure Nothing
gen _ _ = pure Nothing

-- closure

reach :: [Pred] -> [Tyvar] -> [Pred]
reach ps vs = [p | p@(InCls _ t _) <- ps, fv t `subset` vs]

closure :: [Pred] -> [Tyvar] -> [Pred]
closure ps vs
  | fv (reach ps vs) `subset` vs = reach ps vs
  | otherwise = closure ps (fv (reach ps vs))

-- utilities

subset :: (Eq a) => [a] -> [a] -> Bool
subset ps qs = all (\x -> x `elem` qs) ps
