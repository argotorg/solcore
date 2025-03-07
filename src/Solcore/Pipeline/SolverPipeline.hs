module Solcore.Pipeline.SolverPipeline where 

import Solcore.Frontend.Parser.SolverInputParser
import Solcore.Frontend.Pretty.SolcorePretty
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
          runSat ps p

runSat :: [Qual Pred] -> [Pred] -> IO Bool 
runSat insts ps 
  = do 
      let 
        senv = initTcEnv (emptyOption "") []
        maxIter = 20
      res <- runTcM (sat maxIter ps) senv
      case res of 
        Left err -> do 
          putStrLn err
          pure False 
        Right _ -> do  
          putStrLn $ unlines [ "Instances:"
                             , unwords $ map pretty insts 
                             , "Constraints:"
                             , unwords $ map pretty ps
                             , "are satisfiable"
                             ]
          pure True 
          

