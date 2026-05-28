module LocationTests
  ( locationTests,
  )
where

import Solcore.Diagnostics (SourceSpan (..))
import Solcore.Frontend.Parser.SolcoreParser (parseCompUnitWithPath)
import Solcore.Frontend.Syntax.Location
import Solcore.Frontend.Syntax.SyntaxTree ()
import Test.Tasty
import Test.Tasty.HUnit

locationTests :: TestTree
locationTests =
  testGroup
    "Syntax locations"
    [ testCase "parsed nodes carry source locations" test_parsedNodesCarrySourceLocations,
      testCase "generated nodes are explicit" test_generatedNodesAreExplicit
    ]

test_parsedNodesCarrySourceLocations :: Assertion
test_parsedNodesCarrySourceLocations = do
  parsed <- parseCompUnitWithPath "location-invariant.solc" locatedSource
  unit <-
    case parsed of
      Left err -> assertFailure err
      Right cunit -> pure cunit
  assertBool "compilation unit should have a source span" (hasSourceSpan unit)
  assertBool "parser sample should exercise located AST nodes" (length (nodeLocationsOf unit) > 8)
  assertEqual "generated node locations in parser output" [] (filter isGeneratedNodeLocation (nodeLocationsOf unit))

test_generatedNodesAreExplicit :: Assertion
test_generatedNodesAreExplicit = do
  assertBool "unlocatedNode is generated" (isGeneratedNodeLocation unlocatedNode)
  assertEqual "generated source span" Nothing (nodeLocationSpan unlocatedNode)
  assertEqual "source node span" (Just sampleSpan) (nodeLocationSpan (locatedNode sampleSpan))

hasSourceSpan :: (HasSourceSpan a) => a -> Bool
hasSourceSpan =
  maybe False (const True) . sourceSpanOf

sampleSpan :: SourceSpan
sampleSpan =
  SourceSpan
    { spanFile = "generated.solc",
      spanStartByte = 0,
      spanEndByte = 1,
      spanStartLine = 1,
      spanStartColumn = 1,
      spanEndLine = 1,
      spanEndColumn = 2
    }

locatedSource :: String
locatedSource =
  unlines
    [ "data Bool = True | False;",
      "function main(x : word) -> word {",
      "  let y : word = x + 1;",
      "  match Bool.True {",
      "  | Bool.True => return y;",
      "  | _ => return 0;",
      "  }",
      "}"
    ]
