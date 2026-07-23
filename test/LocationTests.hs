module LocationTests
  ( locationTests,
  )
where

import Data.Generics (Data, everything, mkQ)
import Data.Maybe (mapMaybe)
import Data.Set qualified as Set
import Solcore.Diagnostics (CompilerError, SourceSpan (..), compilerErrorText)
import Solcore.Frontend.ComptimeCheck (checkComptimeEarly)
import Solcore.Frontend.Parser.SolcoreParser (parseCompUnitWithPath)
import Solcore.Frontend.Syntax qualified as Typed
import Solcore.Frontend.Syntax.Location
import Solcore.Frontend.Syntax.NameResolution (nameResolution)
import Solcore.Frontend.Syntax.SyntaxTree qualified as Parsed
import Solcore.Frontend.TypeInference.SccAnalysis (sccAnalysis)
import Solcore.Frontend.TypeInference.Id (Id)
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
      testCase "type inference preserves source locations" test_typeInferencePreservesSourceLocations,
      testCase "tuple destructuring binds typed and inferred recursive leaves" test_tupleDestructuringTypeChecks,
      testCase "comptime tuple destructuring propagates and checks binding ctness" test_comptimeTupleDestructuring
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

test_tupleDestructuringTypeChecks :: Assertion
test_tupleDestructuringTypeChecks = do
  parsed <- parseUnit "destructuring-let.solc" destructuringSource
  resolved <- assertCompilerRight "name resolution" (nameResolution parsed)
  _ <-
    assertCompilerRight
      "tuple destructuring type inference"
      (typeInferModuleLocals stdOpt (moduleInputFromUnit resolved))
  badParsed <- parseUnit "destructuring-let-mismatch.solc" badDestructuringSource
  badResolved <- assertCompilerRight "name resolution" (nameResolution badParsed)
  badResult <-
    typeInferModuleLocals stdOpt (moduleInputFromUnit badResolved)
  case badResult of
    Left _ -> pure ()
    Right _ ->
      assertFailure "a tuple binding annotation must describe the complete initializer type"

test_comptimeTupleDestructuring :: Assertion
test_comptimeTupleDestructuring = do
  goodUnit <- inferUnit "comptime-destructuring-good.solc" comptimeDestructuringSource
  assertEitherRight
    "comptime tuple bindings should remain comptime in their continuation"
    (checkComptimeEarly (sourceFunctionsOnly goodUnit))
  badUnit <- inferUnit "comptime-destructuring-bad.solc" runtimeDestructuringSource
  case checkComptimeEarly (sourceFunctionsOnly badUnit) of
    Left _ -> pure ()
    Right () ->
      assertFailure "a comptime tuple binding must reject a runtime initializer"
  propagatedUnit <-
    inferUnit
      "runtime-destructuring-propagation.solc"
      runtimeDestructuringPropagationSource
  case checkComptimeEarly (sourceFunctionsOnly propagatedUnit) of
    Left _ -> pure ()
    Right () ->
      assertFailure "runtime ctness must propagate through source tuple destructuring"

sourceFunctionsOnly :: Typed.CompUnit Id -> Typed.CompUnit Id
sourceFunctionsOnly (Typed.CompUnit imps decls) =
  Typed.CompUnit imps [decl | decl@(Typed.TFunDef _) <- decls]

inferUnit :: FilePath -> String -> IO (Typed.CompUnit Id)
inferUnit path source = do
  parsed <- parseUnit path source
  resolved <- assertCompilerRight "name resolution" (nameResolution parsed)
  fst
    <$> assertCompilerRight
      "type inference"
      (typeInferModuleLocals stdOpt (moduleInputFromUnit resolved))

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
    [ "enum Bool { True, False }",
      "function main(x: word) returns (word) {",
      "  let y: word = x + 1;",
      "  match (Bool.True) {",
      "    case Bool.True {",
      "      return y;",
      "    }",
      "    case Bool.False {",
      "      return 0;",
      "    }",
      "  }",
      "}"
    ]

transformSource :: String
transformSource =
  unlines
    [ "function id(x: word) returns (word) {",
      "  return x;",
      "}",
      "function passthrough(y: word) returns (word) {",
      "  return id(y);",
      "}"
    ]

mutualSource :: String
mutualSource =
  unlines
    [ "function first(x: word) returns (word) {",
      "  return second(x);",
      "}",
      "function second(x: word) returns (word) {",
      "  return first(x);",
      "}"
    ]

destructuringSource :: String
destructuringSource =
  unlines
    [ "function typed(value: (word, bool)) returns (word) {",
      "  let (amount, ok): (word, bool) = value;",
      "  if (ok) { return amount; } else { return amount; }",
      "}",
      "function nested(value: (word, (bool, word))) returns (word) {",
      "  let (amount, (ok, fallbackValue)) = value;",
      "  if (ok) { return amount; } else { return fallbackValue; }",
      "}"
    ]

badDestructuringSource :: String
badDestructuringSource =
  unlines
    [ "function bad(value: (word, word)) returns (word) {",
      "  let (amount, ok): (word, bool) = value;",
      "  if (ok) { return amount; } else { return amount; }",
      "}"
    ]

comptimeDestructuringSource :: String
comptimeDestructuringSource =
  unlines
    [ "function consume(comptime x: bool) returns (bool) {",
      "  return x;",
      "}",
      "function good() returns (bool) {",
      "  let comptime (left, right): (bool, bool) = (true, false);",
      "  return consume(left);",
      "}"
    ]

runtimeDestructuringSource :: String
runtimeDestructuringSource =
  unlines
    [ "function bad(value: (word, word)) returns (word) {",
      "  let comptime (left, right) = value;",
      "  return left;",
      "}"
    ]

runtimeDestructuringPropagationSource :: String
runtimeDestructuringPropagationSource =
  unlines
    [ "function consume(comptime value: word) returns (word) {",
      "  return value;",
      "}",
      "function bad(value: (word, word)) returns (word) {",
      "  let (left, right) = value;",
      "  return consume(left);",
      "}"
    ]
