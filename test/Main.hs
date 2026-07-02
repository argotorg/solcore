module Main where

import Cases
import ContractAbiTests
import DiagnosticCliTests
import DiagnosticTests
import HullCases
import KeccakTests
import LocationTests
import MatchCompilerTests
import ModuleTypeCheckTests
import ParserTests
import SpecialiseTests
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
      opcodes,
      pragmas,
      spec,
      std,
      diagnosticCliTests,
      diagnosticTests,
      locationTests,
      imports,
      moduleTypeCheckTests,
      dispatches,
      contractAbiTests,
      matchTests,
      yulEvalTests,
      hullTests,
      specialiseTests,
      keccakTests
    ]
