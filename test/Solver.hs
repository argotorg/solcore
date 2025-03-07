module Solver where

import Solcore.Pipeline.SolverPipeline

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.ExpectedFailure 

-- constructing the test suit 

satTests :: TestTree 
satTests = testGroup "Tests for SAT"
                     [
                       testCase "testCase0" $ satFile solverDir "sat00.inp"
                     , testCase "testCase1" $ satFile solverDir "sat01.inp"
                     , testCase "testCase2" $ satFile solverDir "sat02.inp"
                     , testCase "testCase3" $ satFile solverDir "sat03.inp"
                     , expectFail $ testCase "testCase4" $ satFile solverDir "sat04.inp"
                     , expectFail $ testCase "testCase5" $ satFile solverDir "sat05.inp"
                     ]
            where 
              solverDir = "./test/solver"

satFile :: String -> String -> Assertion 
satFile dir file 
  = do 
      let path = concat [dir, "/", file]
      (runForFile path) @? "" 
