module Main where

import Cases
import MatchCompilerTests
import Test.Tasty
import YulEvalTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ cases,
      comptime,
      pragmas,
      spec,
      std,
      imports,
      dispatches,
      matchTests,
      yulEvalTests
    ]
