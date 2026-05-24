module Main where

import Cases
import DiagnosticTests
import HullCases
import MatchCompilerTests
import ModuleTypeCheckTests
import Test.Tasty

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
      diagnosticTests,
      imports,
      moduleTypeCheckTests,
      dispatches,
      matchTests,
      hullTests
    ]
