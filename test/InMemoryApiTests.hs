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
        assertBool "yul should be a Yul object"
          (maybe False (\y -> "object" `isInfixOf` y) (compileYul result)),
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
        assertBool "expected diagnostics" (not (null (compileErrors result)))
    ]
