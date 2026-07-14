module InMemoryApiTests where

import Data.List (isInfixOf)
import Solcore.Api (CompileResult (..), compileSolcore, defaultOptions)
import Solcore.Pipeline.Options (Option (..))
import Test.Tasty
import Test.Tasty.HUnit

-- A simple contract that uses no std (compiled with dispatch generation off,
-- i.e. the CLI's -g). Mirrors test/examples/spec/00answer.solc.
simpleSource :: String
simpleSource =
  unlines
    [ "contract Answer {",
      "  public function main() -> word {",
      "    return 42;",
      "  }",
      "}"
    ]

noDispatch :: Option
noDispatch = defaultOptions {optNoGenDispatch = True}

inMemoryApiTests :: TestTree
inMemoryApiTests =
  testGroup
    "In-memory API"
    [ testCase "compiles a simple std-free contract with -g (hull + yul)" $ do
        result <- compileSolcore noDispatch simpleSource
        compileErrors result @?= []
        assertBool "expected non-empty hull output" (maybe False (not . null) (compileOutput result))
        assertBool "expected non-empty yul output" (maybe False (not . null) (compileYul result))
        assertBool
          "yul should be a Yul object"
          (maybe False (\y -> "object" `isInfixOf` y) (compileYul result)),
      testCase "emits a JSON ABI for the entry module's contract" $ do
        result <- compileSolcore noDispatch simpleSource
        case compileAbis result of
          [(contractName, abiJson)] -> do
            contractName @?= "Answer"
            assertBool "ABI should describe the main function" ("\"main\"" `isInfixOf` abiJson)
            assertBool "ABI should be a JSON array" ("[" `isInfixOf` abiJson)
          other -> assertFailure ("expected exactly one contract ABI, got " ++ show (map fst other)),
      -- The canonical dispatch example, fed in as source text (as the UI would)
      -- and compiled with the standard library resolved from the in-memory
      -- bundle. It imports std, std.dispatch and std.opcodes.
      testCase "compiles the dispatch/basic.solc example against bundled std" $ do
        source <- readFile "test/examples/dispatch/basic.solc"
        result <- compileSolcore defaultOptions source
        compileErrors result @?= []
        assertBool "expected non-empty hull output" (maybe False (not . null) (compileOutput result))
        assertBool "expected non-empty yul output" (maybe False (not . null) (compileYul result)),
      testCase "reports a parse error as a diagnostic" $ do
        result <- compileSolcore defaultOptions "this is not solcore"
        compileOutput result @?= Nothing
        assertBool "expected diagnostics" (not (null (compileErrors result))),
      testCase "reports every module as a cache hit on identical recompile" $ do
        source <- readFile "test/examples/dispatch/basic.solc"
        _ <- compileSolcore defaultOptions source -- warm the session cache
        again <- compileSolcore defaultOptions source
        let status = compileCacheStatus again
        assertBool "expected per-module cache status" (not (null status))
        assertBool "expected std among reported modules" (any (("std" ==) . fst) status)
        assertBool
          "expected all modules reused on identical recompile"
          (all snd status)
        -- The ABI is read from the entry module's prepared input; assert it is
        -- still produced when that module is served from the session cache.
        assertBool "expected an ABI even on a cache-hit recompile" (not (null (compileAbis again)))
    ]
