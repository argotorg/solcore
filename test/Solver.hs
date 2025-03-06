module Solver where

import Data.Either

import Solcore.Frontend.Parser.SolverInputParser
import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcMonad
import Solcore.Frontend.TypeInference.TcSat
import Solcore.Pipeline.Options 

import Test.Tasty
import Test.Tasty.ExpectedFailure
import Test.Tasty.HUnit

-- constructing the test suit 

satTests :: TestTree 
satTests = testGroup "Tests for SAT"
                     [
                       expectFail $ testCase "test01" $ satFile solverDir "sat01.inp"
                     ]
            where 
              solverDir = "./test/solver"

satFile :: String -> String -> Assertion 
satFile dir file 
  = do 
      let path = concat [dir, "/", file]
      (runForFile path) @? "Sat failed" 


-- running the satisfiability (for tests) 

runForFile :: FilePath -> IO Bool 
runForFile file 
  = do 
      content <- readFile file 
      let res = runParser content 
      either (\ _ -> pure False)
             (uncurry runSat)
             res 

runSat :: [Qual Pred] -> [Pred] -> IO Bool 
runSat insts ps 
  = isRight <$> runTcM (sat maxIter ps) senv
    where 
      senv = initTcEnv (emptyOption "") []
      maxIter = 20
