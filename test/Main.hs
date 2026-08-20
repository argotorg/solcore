module Main where

import Cases
import ComptimeCheckTests
import ContractAbiTests
import DiagnosticCliTests
import DiagnosticTests
import HullCases
import LocationTests
import MatchCompilerTests
import ModuleTypeCheckTests
import ParserTests
import SpecialiseTests
import Test.Tasty
import YulEvalTests
import YulParserTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ parserTests,
      yulParserTests,
      cases,
      tabledResolution,
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
      comptimeCheckTests,
      hullTests,
      specialiseTests
    ]
