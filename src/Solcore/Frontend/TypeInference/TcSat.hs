module Solcore.Frontend.TypeInference.TcSat where 

import Control.Monad.Except
import Control.Monad.Trans
import Data.Maybe

import Solcore.Frontend.Syntax
import Solcore.Frontend.Parser.SolverInputParser
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify

import Solcore.Pipeline.Options

-- function sat 

satI :: Int -> Pred -> TcM [(Subst, [Pred])]
satI n p@(InCls c t ts)  
  | n <= 0 = tcmError $ unwords ["Cannot satisfy constraint:"
                                , pretty p
                                , "since we reached the maximum number of recursive calls"]
  | otherwise = do 
      insts <- askInstEnv c 
      catMaybes <$> mapM (\ i -> defaultM (step t i)) insts 
satI _ p = tcmError $ "Invalid constraint:" ++ pretty p

step :: Ty -> Inst -> TcM (Subst, [Pred])
step t k@(ps :=> h@(InCls _ t' _)) 
  = do {
      s <- mgu t t' ; 
      pure (s, apply s ps) 
    }

defaultM :: TcM a -> TcM (Maybe a)
defaultM m 
  = do {
      x <- m ;
      pure (Just x)
    } `catchError` (\ _ -> pure Nothing)

-- constraint set satisfiability

satPred :: Int -> Pred -> TcM [Subst]
satPred n p = do -- rule Inst
  -- liftIO $ putStrLn $ "constraint:" ++ pretty p
  delta <- satI n p 
  let f (s, ps) = pretty s ++ " - " ++ pretty ps 
  -- liftIO $ putStrLn $ unwords $ map f delta
  ss <- concat <$> mapM (\ (s,q) -> sat (n - 1) q) delta
  pure $ [s' <> s | (s, _) <- delta, s' <- ss]
 

sat :: Int -> [Pred] -> TcM [Subst]
sat 0 p = throwError $ unwords [ "Could not deduce:"
                               , pretty p 
                               , "using the current step limit"
                               ]
sat n [] = pure [mempty] -- rule SEmpty 
sat n (p : ps) 
  = do      --rule SConj
      ss0 <- satPred n p
      ss1 <- concat <$> mapM (\ s0 -> sat (n - 1) (apply s0 ps)) ss0
      pure $ [s' <> s | s <- ss0, s' <- ss1]


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

