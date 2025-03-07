module Solver where

import Solcore.Pipeline.SolverPipeline

import Test.Tasty
import Test.Tasty.HUnit

-- constructing the test suit 

satTests :: TestTree 
satTests = testGroup "Tests for SAT"
                     [
                       testCase "testCase0" $ satFile solverDir "sat00.inp"
                     , testCase "testCase1" $ satFile solverDir "sat01.inp"
                     , testCase "testCase2" $ satFile solverDir "sat02.inp"
                     ]
            where 
              solverDir = "./test/solver"

satFile :: String -> String -> Assertion 
satFile dir file 
  = do 
      let path = concat [dir, "/", file]
      (runForFile path) @? "Sat failed" 
