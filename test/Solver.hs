module Solver where

import Test.Tasty
import Test.Tasty.HUnit
import Solcore.Pipeline.SolverPipeline (runForFile)


-- tests

satTests :: TestTree
satTests = testGroup "Tests for SAT" $
  fmap shouldSolve
    [ "sat00.inp"
    , "sat01.inp"
    , "sat02.inp"
    , "sat03.inp"
    ]

unsatTests :: TestTree
unsatTests = testGroup "Tests for UNSAT" $
  fmap shouldFail
    [ "sat04.inp"
    , "sat05.inp"
    ]

reduceTests :: TestTree
reduceTests = testGroup "Tests for reduce" $
  fmap shouldSolve
    [ "red00.inp"
    , "red01.inp"
    , "red02.inp"
    , "red03.inp"
    , "red04.inp"
    , "red05.inp"
    , "red06.inp"
    ]

-- utils

shouldSolve :: FilePath -> TestTree
shouldSolve = testFile id

shouldFail :: FilePath -> TestTree
shouldFail = testFile not

testFile :: (Bool -> Bool) -> FilePath -> TestTree
testFile fn file = testCase file $ do
  res <- runForFile ("./test/solver/" ++ file)
  assertBool ("Solver test failed for " ++ file) (fn res)
