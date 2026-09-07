module Main where

import Cases
import ContractAbiTests
import DiagnosticCliTests
import DiagnosticTests
import HullCases
import LocationTests
import MatchCompilerTests
import ModuleTypeCheckTests
import NewSyntaxDeclTests
import NewSyntaxDeriveTests
import NewSyntaxExprTests
import NewSyntaxTypeTests
import ParserTests
import SpecialiseTests
import UfcsTests
import YulEvalTests

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests =
  testGroup
    "Tests"
    [ newSyntaxTypeTests,
      newSyntaxExprTests,
      newSyntaxDeclTests,
      newSyntaxDeriveTests,
      parserTests,
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
      hullTests,
      specialiseTests,
      ufcsTests
    ]
