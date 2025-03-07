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
        Right (ps, p) -> 
          runSat (correct ps) (correct p)

-- simple hack: type variables on tests 
-- should be strings of length 1

class Correct a where 
  correct :: a -> a 

instance Correct a => Correct [a] where
  correct = map correct 

instance Correct Ty where 
  correct t@(TyCon n ts) 
    | isVar n = TyVar (TVar n False)
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

runSat :: [Qual Pred] -> [Pred] -> IO Bool 
runSat insts ps 
  = do 
      let 
        senv = buildEnv insts 
        maxIter = 20
      res <- runTcM (sat maxIter ps) senv
      case res of 
        Left err -> do 
          putStrLn err
          pure False 
        Right ([s], _) -> do  
          -- putStrLn $ unlines [ "Instances:"
          --                    , render $ commaSep $ map ppr insts 
          --                    , "Constraints:"
          --                    , unwords $ map pretty ps
          --                    , "are satisfiable!"
          --                    , "Substitution:"
          --                    , pretty s 
          --                    ]
          pure True 
        Right (ss, _) -> do 
          when (null ss) (putStrLn $ "Could not satisfy " ++ unwords (map pretty ps))
          unless (null ss) (putStrLn $ "Not unique solution for " ++ unwords (map pretty ps))
          pure False
          
buildEnv :: [Qual Pred] -> TcEnv 
buildEnv 
  = foldr step (initTcEnv (emptyOption "") [])
    where 
      step i@(ps :=> h@(InCls c _ _)) senv 
        = senv{ instEnv = Map.insertWith (++) c [i] (instEnv senv) }
