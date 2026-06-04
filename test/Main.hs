module Main where

import Cases
import HullCases
import MatchCompilerTests
import ModuleTypeCheckTests
import ParserTests
import Test.Tasty
import YulEvalTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ parserTests,
      cases,
      comptime,
      pragmas,
      spec,
      std,
      imports,
      moduleTypeCheckTests,
      dispatches,
      matchTests,
      yulEvalTests,
      hullTests
    ]
