module Main where

import Cases
import ContractAbiTests
import HullCases
import InMemoryApiTests
import KeccakTests
import MatchCompilerTests
import ModuleTypeCheckTests
import ParserTests
import SpecialiseTests
import TcCacheTests
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
      imports,
      moduleTypeCheckTests,
      dispatches,
      contractAbiTests,
      matchTests,
      yulEvalTests,
      hullTests,
      specialiseTests,
      inMemoryApiTests,
      tcCacheTests,
      keccakTests
    ]
