module Solver where

import Test.Tasty
import Test.Tasty.Program
import Test.Tasty.ExpectedFailure 


-- constructing the test suit 

satTests :: TestTree 
satTests = testGroup "Tests for SAT"
                     [
                       testFile solverDir "sat00.inp"
                     , testFile solverDir "sat01.inp"
                     , testFile solverDir "sat02.inp"
                     , testFile solverDir "sat03.inp"
                     , expectFail $ testFile solverDir "sat04.inp"
                     , expectFail $ testFile solverDir "sat05.inp"
                     ]
            where 
              solverDir = "./test/solver"

testFile :: String -> String -> TestTree 
testFile folder file 
  = testProgram file "cabal" (basicOptions ++ [folder ++ "/" ++ file]) Nothing 
    where 
      basicOptions = ["run", "solver", "--"]
    

reduceTests :: TestTree 
reduceTests = testGroup "Tests for reduce"
                        [
                          testFile solverDir "red00.inp"
                        , testFile solverDir "red01.inp"
                        , testFile solverDir "red02.inp"
                        , testFile solverDir "red03.inp"
                        ]
            where 
              solverDir = "./test/solver"


