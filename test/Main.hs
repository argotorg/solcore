module Main where

import Cases
-- import Solver
import Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests
  = testGroup "Tests"
               [
                 cases
               , pragmas
               , spec
               , std
               , imports
--             , reduceTests
               ]
