module Solcore.Frontend.TypeInference.TcSat where 

import Data.Either (isRight)

import Solcore.Frontend.Syntax
import Solcore.Frontend.Parser.SolverInputParser
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify

import Solcore.Pipeline.Options

-- function sat 

satI :: Int -> Pred -> TcM [(Subst, [Pred], Pred)]
satI n p@(InCls c t ts)  
  | n <= 0 = tcmError $ unwords ["Cannot satisfy constraint:"
                                , pretty p
                                , "since we reached the maximum number of recursive calls"]
  | otherwise = do 
      insts <- askInstEnv c
      mapM (step t) insts 
satI _ p = tcmError $ "Invalid constraint:" ++ pretty p

step :: Ty -> Inst -> TcM (Subst, [Pred], Pred)
step t (ps :=> h@(InCls _ t' _)) 
  = do 
      s <- mgu t t' 
      pure (s, apply s ps, h)

-- constraint set satisfiability

sat :: Int -> [Pred] -> TcM [Subst]
sat 0 _ = pure [] -- rule SFail 
sat n [] = pure [mempty] -- rule SEmpty 
sat n [p] = do -- rule SInst 
  delta <- satI n p
  ss <- concat <$> mapM (\ (_,q,_) -> sat (n - 1) q) delta 
  pure $ [s' <> s | (s, _, _) <- delta, s' <- ss]


-- closure 

reach :: [Pred] -> [Tyvar] -> [Pred] 
reach ps vs = [p | p@(InCls _ t _) <- ps, fv t `subset` vs]

closure :: [Pred] -> [Tyvar] -> [Pred] 
closure ps vs 
  | fv (reach ps vs) `subset` vs = reach ps vs 
  | otherwise = closure ps (fv (reach ps vs))

-- utilities 

subset :: Eq a => [a] -> [a] -> Bool 
subset ps qs = all (\ x -> x `elem` qs) ps

-- running the satisfiability (for tests) 

runSat :: [Qual Pred] -> [Pred] -> IO Bool 
runSat insts ps 
  = isRight <$> runTcM (sat maxIter ps) senv
    where 
      senv = initTcEnv (emptyOption "") []
      maxIter = 20
