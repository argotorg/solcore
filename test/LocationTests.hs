module LocationTests
  ( locationTests,
  )
where

import Data.Generics (Data, everything, mkQ)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Solcore.Diagnostics (CompilerError, SourceSpan (..), compilerErrorText)
import Solcore.Frontend.Parser.SolcoreParser (parseCompUnitWithPath)
import Solcore.Frontend.Syntax qualified as Typed
import Solcore.Frontend.Syntax.Location
import Solcore.Frontend.Syntax.NameResolution (nameResolution)
import Solcore.Frontend.Syntax.SyntaxTree qualified as Parsed
import Solcore.Frontend.TypeInference.SccAnalysis (sccAnalysis)
import Solcore.Frontend.TypeInference.TcModule
import Solcore.Pipeline.Options (stdOpt)
import Test.Tasty
import Test.Tasty.HUnit

locationTests :: TestTree
locationTests =
  testGroup
    "Syntax locations"
    [ testCase "parsed nodes carry source locations" test_parsedNodesCarrySourceLocations,
      testCase "generated nodes are explicit" test_generatedNodesAreExplicit,
      testCase "name resolution preserves source locations" test_nameResolutionPreservesSourceLocations,
      testCase "SCC analysis preserves source locations" test_sccAnalysisPreservesSourceLocations,
      testCase "type inference preserves source locations" test_typeInferencePreservesSourceLocations
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

test_nameResolutionPreservesSourceLocations :: Assertion
test_nameResolutionPreservesSourceLocations = do
  parsed <- parseUnit "location-name-resolution.solc" transformSource
  resolved <- assertCompilerRight "name resolution" (nameResolution parsed)
  assertSpansPreserved "name resolution" parsed resolved
  assertNoGeneratedNodeLocations "name resolution" resolved

test_sccAnalysisPreservesSourceLocations :: Assertion
test_sccAnalysisPreservesSourceLocations = do
  parsed <- parseUnit "location-scc.solc" mutualSource
  resolved <- assertCompilerRight "name resolution" (nameResolution parsed)
  grouped <- assertEitherRight "SCC analysis" =<< sccAnalysis resolved
  assertBool "SCC analysis should create a mutual group" (any isMutualDecl (Typed.contracts grouped))
  assertSpansPreserved "SCC analysis" resolved grouped
  assertNoGeneratedNodeLocations "SCC analysis" grouped

test_typeInferencePreservesSourceLocations :: Assertion
test_typeInferencePreservesSourceLocations = do
  parsed <- parseUnit "location-type-inference.solc" transformSource
  resolved <- assertCompilerRight "name resolution" (nameResolution parsed)
  (typedUnit, _) <-
    assertCompilerRight
      "type inference"
      (typeInferModuleLocals stdOpt (moduleInputFromUnit resolved))
  assertSpansPreserved "type inference" resolved typedUnit

hasSourceSpan :: (HasSourceSpan a) => a -> Bool
hasSourceSpan =
  maybe False (const True) . sourceSpanOf

parseUnit :: FilePath -> String -> IO Parsed.CompUnit
parseUnit path source = do
  parsed <- parseCompUnitWithPath path source
  case parsed of
    Left err -> assertFailure err
    Right cunit -> pure cunit

assertCompilerRight :: String -> IO (Either CompilerError a) -> IO a
assertCompilerRight label action = do
  result <- action
  case result of
    Left err -> assertFailure (label ++ " failed:\n" ++ compilerErrorText err)
    Right value -> pure value

assertEitherRight :: String -> Either String a -> IO a
assertEitherRight label result =
  case result of
    Left err -> assertFailure (label ++ " failed:\n" ++ err)
    Right value -> pure value

assertSpansPreserved :: (Data source, Data target) => String -> source -> target -> Assertion
assertSpansPreserved label source target = do
  let sourceSpans = Set.fromList (sourceSpansOf source)
      targetSpans = Set.fromList (sourceSpansOf target)
      introduced = Set.toList (targetSpans `Set.difference` sourceSpans)
  assertBool (label ++ " should keep source spans") (not (Set.null targetSpans))
  assertEqual (label ++ " introduced non-input source spans") [] introduced

assertNoGeneratedNodeLocations :: (Data a) => String -> a -> Assertion
assertNoGeneratedNodeLocations label value =
  assertEqual
    (label ++ " generated node locations")
    []
    (filter isGeneratedNodeLocation (nodeLocationsOf value))

sourceSpansOf :: (Data a) => a -> [SourceSpan]
sourceSpansOf value =
  mapMaybe nodeLocationSpan (nodeLocationsOf value)
    ++ everything (++) (mkQ [] nameSpan) value
  where
    nameSpan :: Typed.Name -> [SourceSpan]
    nameSpan name = maybe [] pure (sourceSpanOf name)

moduleInputFromUnit :: Typed.CompUnit Typed.Name -> ModuleTypeCheckInput
moduleInputFromUnit unit =
  withPreparedModuleInferenceDecls resolvedInput (moduleInitialInferenceDecls resolvedInput)
  where
    resolvedInput =
      ModuleResolvedTypeCheckInput
        { moduleResolvedInputImports = Typed.imports unit,
          moduleResolvedInputQualifiedDecls = [],
          moduleResolvedInputLocalDecls = Typed.contracts unit,
          moduleResolvedInputImportedDecls = [],
          moduleResolvedInputTrustedInstanceHeads = [],
          moduleResolvedInputPartialImportedTypes = []
        }

isMutualDecl :: Typed.TopDecl a -> Bool
isMutualDecl (Typed.TMutualDef _) = True
isMutualDecl _ = False

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

transformSource :: String
transformSource =
  unlines
    [ "function id(x : word) -> word {",
      "  return x;",
      "}",
      "function passthrough(y : word) -> word {",
      "  return id(y);",
      "}"
    ]

mutualSource :: String
mutualSource =
  unlines
    [ "function first(x : word) -> word {",
      "  return second(x);",
      "}",
      "function second(x : word) -> word {",
      "  return first(x);",
      "}"
    ]
