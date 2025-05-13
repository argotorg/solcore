module Solcore.Pipeline.SolverPipeline where 

import Control.Monad
import Data.List
import qualified Data.Map as Map

import Solcore.Frontend.Parser.SolverInputParser
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.TypeInference.TcEnv 
import Solcore.Frontend.TypeInference.TcMonad 
import Solcore.Frontend.TypeInference.TcReduce
import Solcore.Frontend.TypeInference.TcSat 
import Solcore.Pipeline.Options 

-- running the satisfiability (for tests) 

runForFile :: FilePath -> IO Bool 
runForFile file 
  = do 
      content <- readFile file 
      let res = runParser content
      case res of 
        Left err -> do 
          putStrLn err 
          pure False 
        Right state -> do  
          runSolver state

runSolver :: SolverState -> IO Bool 
runSolver (SolverState (Theta cls insts) p)
  = solveProblem cls insts p 

solveProblem :: [Qual Pred] -> [Qual Pred] -> SolverProblem -> IO Bool 
solveProblem cls insts (Sat ps) 
  = runSat (correct cls) (correct insts) (correct ps) 
solveProblem cls insts (Reduce ps qs) 
  = runReduce (correct cls) (correct insts) (correct ps) (correct qs)

-- simple hack: type variables on tests 
-- should be strings of length 1

class Correct a where 
  correct :: a -> a 

instance Correct a => Correct [a] where
  correct = map correct 

instance Correct Ty where 
  correct t@(TyCon n ts) 
    | isVar n && null ts = Meta (MetaTv n)
    | otherwise = TyCon n (correct ts)  

instance Correct Pred where 
  correct (InCls n t ts) 
    = InCls n (correct t) (correct ts)
  correct (t1 :~: t2)
    = (correct t1) :~: (correct t2)

instance Correct (Qual Pred) where
  correct (ps :=> p) 
    = (correct ps) :=> (correct p)

isVar :: Name -> Bool 
isVar n = length (pretty n) == 1

runSat :: [Qual Pred] -> [Qual Pred] -> [Pred] -> IO Bool 
runSat cls insts ps 
  = do 
      let 
        senv = buildEnv cls insts 
      res <- runTcM (sat ps) senv
      case res of 
        Left err -> do 
          putStrLn err
          pure False 
        Right ([s], _) -> do
         putStrLn "Constraint is satisfiable!"
         pure True 
        Right (ss, _) -> do 
          when (null ss) (putStrLn $ "Could not satisfy " ++ unwords (map pretty ps))
          unless (null ss) (putStrLn $ "Not unique solution for " ++ unwords (map pretty ps))
          pure False

runReduce :: [Qual Pred] -> [Qual Pred] -> [Pred] -> [Pred] -> IO Bool 
runReduce cls insts ps qs 
  = do 
      let 
        senv = buildEnv cls insts 
      res <- runTcM (reduce ps) senv 
      case res of 
        Left err -> do 
          putStrLn err
          pure False 
        Right (ps', _) -> do
          putStrLn $ "Reduced constraints:" ++ pretty ps'
          putStrLn $ "Desired:" ++ pretty qs
          putStrLn $ "Final result:" ++ show (ps' == qs)
          pure (ps' == qs)

buildEnv :: [Qual Pred] -> [Qual Pred] -> TcEnv 
buildEnv cls insts = insertClasses cls (insertInsts insts)

insertClasses :: [Qual Pred] -> TcEnv -> TcEnv 
insertClasses cls env = foldr step env cls 
  where 
    step c@(ps :=> h@(InCls n t ts)) senv 
      = let 
          info = ClassInfo (length (t : ts)) [] h ps
        in senv {classTable = Map.insert n info (classTable senv) }

insertInsts :: [Qual Pred] -> TcEnv 
insertInsts
  = foldr step (initTcEnv (emptyOption ""))
    where 
      step i@(ps :=> h@(InCls c _ _)) senv 
        = senv{ instEnv = Map.insertWith (++) c [i] (instEnv senv) }
