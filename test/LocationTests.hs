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
import Solcore.Frontend.TypeInference.Id (Id)
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
      testCase "type inference preserves source locations" test_typeInferencePreservesSourceLocations,
      testCase "tuple match patterns bind typed and nested leaves" test_tuplePatternTypeChecks,
      testCase "comptime local bindings preserve and check evaluation mode" test_comptimeLocalBindings
    ]

test_parsedNodesCarrySourceLocations :: Assertion
test_parsedNodesCarrySourceLocations = do
  parsed <- parseCompUnitWithPath "location-invariant.sol" locatedSource
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
  parsed <- parseUnit "location-name-resolution.sol" transformSource
  resolved <- assertCompilerRight "name resolution" (nameResolution parsed)
  assertSpansPreserved "name resolution" parsed resolved
  assertNoGeneratedNodeLocations "name resolution" resolved

test_sccAnalysisPreservesSourceLocations :: Assertion
test_sccAnalysisPreservesSourceLocations = do
  parsed <- parseUnit "location-scc.sol" mutualSource
  resolved <- assertCompilerRight "name resolution" (nameResolution parsed)
  grouped <- assertEitherRight "SCC analysis" =<< sccAnalysis resolved
  assertBool "SCC analysis should create a mutual group" (any isMutualDecl (Typed.contracts grouped))
  assertSpansPreserved "SCC analysis" resolved grouped
  assertNoGeneratedNodeLocations "SCC analysis" grouped

test_typeInferencePreservesSourceLocations :: Assertion
test_typeInferencePreservesSourceLocations = do
  parsed <- parseUnit "location-type-inference.sol" transformSource
  resolved <- assertCompilerRight "name resolution" (nameResolution parsed)
  (typedUnit, _) <-
    assertCompilerRight
      "type inference"
      (typeInferModuleLocals stdOpt (moduleInputFromUnit resolved))
  assertSpansPreserved "type inference" resolved typedUnit

test_tuplePatternTypeChecks :: Assertion
test_tuplePatternTypeChecks = do
  parsed <- parseUnit "tuple-pattern.sol" tuplePatternSource
  resolved <- assertCompilerRight "name resolution" (nameResolution parsed)
  _ <-
    assertCompilerRight
      "tuple pattern type inference"
      (typeInferModuleLocals stdOpt (moduleInputFromUnit resolved))
  badParsed <- parseUnit "tuple-annotation-mismatch.sol" badTupleAnnotationSource
  badResolved <- assertCompilerRight "name resolution" (nameResolution badParsed)
  badResult <-
    typeInferModuleLocals stdOpt (moduleInputFromUnit badResolved)
  case badResult of
    Left _ -> pure ()
    Right _ ->
      assertFailure "a tuple variable annotation must describe the complete initializer type"

test_comptimeLocalBindings :: Assertion
test_comptimeLocalBindings = do
  goodUnit <- inferUnit "comptime-local-good.sol" comptimeLocalSource
  assertEitherRight
    "comptime local bindings should remain comptime in their continuation"
    (checkComptimeEarly (sourceFunctionsOnly goodUnit))
  badUnit <- inferUnit "comptime-local-bad.sol" runtimeComptimeInitializerSource
  case checkComptimeEarly (sourceFunctionsOnly badUnit) of
    Left _ -> pure ()
    Right () ->
      assertFailure "a comptime local binding must reject a runtime initializer"
  propagatedUnit <-
    inferUnit
      "runtime-pattern-propagation.sol"
      runtimePatternPropagationSource
  case checkComptimeEarly (sourceFunctionsOnly propagatedUnit) of
    Left _ -> pure ()
    Right () ->
      assertFailure "runtime evaluation mode must propagate through tuple patterns"

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
    { spanFile = "generated.sol",
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
      "  let zs: word = [x, y][0];",
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

tuplePatternSource :: String
tuplePatternSource =
  unlines
    [ "function typed(value: (word, bool)) returns (word) {",
      "  let copy: (word, bool) = value;",
      "  match (copy) { case (amount, ok) {",
      "    if (ok) { return amount; } else { return amount; }",
      "  } }",
      "}",
      "function nested(value: (word, (bool, word))) returns (word) {",
      "  match (value) { case (amount, (ok, fallbackValue)) {",
      "    if (ok) { return amount; } else { return fallbackValue; }",
      "  } }",
      "}"
    ]

badTupleAnnotationSource :: String
badTupleAnnotationSource =
  unlines
    [ "function bad(value: (word, word)) returns (word) {",
      "  let copy: (word, bool) = value;",
      "  match (copy) { case (amount, ok) {",
      "    if (ok) { return amount; } else { return amount; }",
      "  } }",
      "}"
    ]

comptimeLocalSource :: String
comptimeLocalSource =
  unlines
    [ "function consume(comptime x: bool) returns (bool) {",
      "  return x;",
      "}",
      "function good() returns (bool) {",
      "  let left: comptime<bool> = true;",
      "  return consume(left);",
      "}"
    ]

runtimeComptimeInitializerSource :: String
runtimeComptimeInitializerSource =
  unlines
    [ "function bad(value: (word, word)) returns ((word, word)) {",
      "  let copy: comptime<(word, word)> = value;",
      "  return value;",
      "}"
    ]

runtimePatternPropagationSource :: String
runtimePatternPropagationSource =
  unlines
    [ "function consume(comptime value: word) returns (word) {",
      "  return value;",
      "}",
      "function bad(value: (word, word)) returns (word) {",
      "  match (value) { case (left, right) { return consume(left); } }",
      "}"
    ]
