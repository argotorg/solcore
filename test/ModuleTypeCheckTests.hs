module ModuleTypeCheckTests
  ( moduleTypeCheckTests,
  )
where

import Data.List (isInfixOf, sort)
import Solcore.Backend.Mast (MastCompUnit (..), MastContract (..), MastTopDecl (..), deployerName)
import Solcore.Backend.Specialise (specialiseCompUnit)
import Solcore.Desugarer.ContractDispatch (contractDispatchTopDecls)
import Solcore.Desugarer.DecisionTreeCompiler (matchCompiler)
import Solcore.Desugarer.DeriveGeneric (deriveGenericTopDecls)
import Solcore.Desugarer.FieldAccess (fieldDesugarTopDecls)
import Solcore.Desugarer.IndirectCall (indirectCallTopDecls)
import Solcore.Diagnostics
  ( CompilerError,
    compilerErrorDiagnostics,
    compilerErrorText,
    diagnosticPrimarySpan,
  )
import Solcore.Frontend.ComptimeCheck (checkComptimeEarly)
import Solcore.Frontend.Module.Loader
  ( ModuleGraph (entryModule),
    ModuleTypeCheckSurface (moduleSurfaceImportedDecls),
    loadModuleGraph,
    moduleLocalTypeCheckSurface,
  )
import Solcore.Frontend.Parser.SolcoreParser (parseCompUnit)
import Solcore.Frontend.Pretty.SolcorePretty qualified as SolcorePretty
import Solcore.Frontend.Pretty.TreePretty qualified as TreePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.Syntax.NameResolution (nameResolution)
import Solcore.Frontend.Syntax.SyntaxTree qualified as Source
import Solcore.Frontend.TypeInference.Id (Id (..))
import Solcore.Frontend.TypeInference.SccAnalysis (sccAnalysisTopDecls)
import Solcore.Frontend.TypeInference.TcContract
  ( TopDeclCheck (..),
    TopDeclCheckMode (CheckTopDeclBody),
    typeInferTopDeclChecks,
  )
import Solcore.Frontend.TypeInference.TcEnv (TcEnv)
import Solcore.Frontend.TypeInference.TcModule
import Solcore.Pipeline.Options (stdOpt)
import Solcore.Pipeline.SolcorePipeline (localDataDefsForDeriving)
import Test.Tasty
import Test.Tasty.HUnit

moduleTypeCheckTests :: TestTree
moduleTypeCheckTests =
  testGroup
    "Module typecheck"
    [ testCase "retagged generated declarations default to local" $ do
        let generated = singleDecl (retagModuleInferenceDecls [] [funDecl "generated"])
        assertEqual
          "generated decl segment"
          ModuleLocalDecl
          (moduleInferenceDeclSegment generated),
      testCase "retagged mixed mutual declarations prefer local segment" $ do
        let inferenceDecls =
              [ ModuleInferenceDecl ModuleImportedDecl (funDecl "imported"),
                ModuleInferenceDecl ModuleLocalDecl (funDecl "local")
              ]
            retagged =
              singleDecl $
                retagModuleInferenceDecls
                  inferenceDecls
                  [TMutualDef [funDecl "imported", funDecl "local"]]
        assertEqual
          "mixed mutual segment"
          ModuleLocalDecl
          (moduleInferenceDeclSegment retagged),
      testCase "resolved input derives initial inference segments" $ do
        let inferenceDecls =
              [ ModuleInferenceDecl ModuleQualifiedDecl (funDecl "qualified"),
                ModuleInferenceDecl ModuleLocalDecl (funDecl "local"),
                ModuleInferenceDecl ModuleImportedDecl (funDecl "imported")
              ]
        assertEqual
          "initial inference segments"
          (map moduleInferenceDeclSegment inferenceDecls)
          (map moduleInferenceDeclSegment (moduleInitialInferenceDecls (resolvedModuleInput inferenceDecls))),
      testCase "type inference trusts imported bodies while checking local bodies" $ do
        result <-
          typeInferModuleLocals
            stdOpt
            (moduleInput [ModuleInferenceDecl ModuleImportedDecl badImportedFun, ModuleInferenceDecl ModuleLocalDecl usesImportedFun])
        assertRight "imported body should be trusted" result,
      testCase "type inference checks local bodies" $ do
        result <-
          typeInferModuleLocals
            stdOpt
            (moduleInput [ModuleInferenceDecl ModuleLocalDecl badImportedFun])
        assertLeft "local body should be checked" result,
      testCase "break and continue outside loops are rejected with locations" $ do
        breakResult <-
          typecheckSource
            "function badBreak() returns (()) { break; }"
        continueResult <-
          typecheckSource
            "function badContinue() returns (()) { continue; }"
        assertLocatedLoopControlError "break outside loop" "break" breakResult
        assertLocatedLoopControlError "continue outside loop" "continue" continueResult,
      testCase "break and continue remain valid in loop bodies" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function validLoopControl(flag: bool) returns (()) {",
                "  while (flag) {",
                "    break;",
                "  }",
                "  for (; flag; ) {",
                "    continue;",
                "  }",
                "  return;",
                "}"
              ]
        assertRight "loop-local control statements" checked,
      testCase "a lambda cannot control its enclosing loop" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function badLambdaBreak(flag: bool) returns (()) {",
                "  while (flag) {",
                "    let callback = lam() returns (()) { break; };",
                "  }",
                "  return;",
                "}"
              ]
        assertLocatedLoopControlError "lambda break boundary" "break" checked,
      testCase "numeric fixed-array size survives resolution and kind checking" $ do
        parsedResult <-
          parseCompUnit $
            unlines
              [ "enum array<element> { array }",
                "function accept(xs: word[4]) returns (()) {",
                "  return;",
                "}"
              ]
        parsed <-
          case parsedResult of
            Left err -> assertFailure ("unexpected parse failure:\n" ++ err)
            Right compUnit -> pure compUnit
        resolvedResult <- nameResolution parsed
        case resolvedResult of
          Left err ->
            assertFailure
              ("unexpected name-resolution failure:\n" ++ compilerErrorText err)
          Right (CompUnit resolvedImports resolvedDecls) -> do
            checked <-
              typeInferTopDeclChecks
                stdOpt
                resolvedImports
                []
                []
                [ TopDeclCheck CheckTopDeclBody decl
                | decl <- resolvedDecls
                ]
            assertRight
              "numeric fixed-array size should be kind-correct"
              checked,
      testCase "internal function parameters remain supported" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function applyCallback(f: function(word) internal returns (word), x: word) returns (word) {",
                "  return f(x);",
                "}"
              ]
        assertRight "internal function parameter" checked,
      testCase "internal function parameters preserve multiple returns" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function applyPair(f: function(word) internal returns (word, bool), x: word) returns (word, bool) {",
                "  return f(x);",
                "}"
              ]
        assertRight "multi-return internal function parameter" checked,
      testCase "external function types fail before internal arrow lowering" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function bad(f: function(word) external returns (word), x: word) returns (word) {",
                "  return x;",
                "}"
              ]
        assertLocatedFunctionTypeError
          "external function type"
          "external function types are not supported"
          checked,
      testCase "nullary function types do not collapse to their result" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function bad(f: function() internal returns (word)) returns (word) {",
                "  return 0;",
                "}"
              ]
        assertLocatedFunctionTypeError
          "nullary function type"
          "zero-parameter function types are not supported"
          checked,
      testCase "struct metadata survives name resolution and typechecking" $ do
        checked <-
          typecheckSource
            "struct Pair { left: word; right: bool; }"
        case checked of
          Left err ->
            assertFailure
              ("struct declaration should typecheck:\n" ++ compilerErrorText err)
          Right (CompUnit _ typedDecls, _) ->
            case [ dt
                 | TDataDef dt <- typedDecls,
                   dataName dt == Name "Pair"
                 ] of
              [ DataTyWithKind
                  (StructKind [Name "left", Name "right"])
                  _
                  []
                  [Constr _ [TyCon (Name "word") [], TyCon (Name "bool") []]]
                ] ->
                  pure ()
              got ->
                assertFailure
                  ("struct kind or ordered fields were lost: " ++ show got),
      testCase "qualified nested type paths beat same-named global declarations" $ do
        source <- readFile "./test/imports/qualified_nested_type_shadow.solc"
        CompUnit _ resolvedDecls <- resolvedSourceOrFail source
        let consumer = findSemanticContract "QualifiedNestedType" resolvedDecls
            nestedType = QualName (Name "C") "T"
            constructorName = QualName nestedType "Inner"
            resolvedBindings =
              [ (ty, value)
              | CFunDecl (FunDef _ sig body) <- decls consumer,
                sigName sig == Name "main",
                Let _ _ ty (Just value) <- body
              ]
        assertEqual
          "the complete C.T.Inner path remains canonical"
          [ ( Just (TyCon nestedType []),
              Con constructorName [Lit (IntLit 7)]
            )
          ]
          resolvedBindings,
      testCase "same-spelled contract-local data types keep distinct canonical identities" $ do
        resolved@(CompUnit _ resolvedDecls) <-
          resolvedSourceOrFail sameNamedLocalTypesSource
        let left = findSemanticContract "Left" resolvedDecls
            right = findSemanticContract "Right" resolvedDecls
            leftS = QualName (Name "Left") "S"
            leftE = QualName (Name "Left") "E"
            rightS = QualName (Name "Right") "S"
            rightE = QualName (Name "Right") "E"
        assertEqual
          "left local declarations retain kind, fields, and canonical constructors"
          [ DataTyWithKind
              (StructKind [Name "left"])
              leftS
              []
              [Constr (QualName leftS "S") [TyCon (Name "word") []]],
            DataTyWithKind
              EnumKind
              leftE
              []
              [Constr (QualName leftE "A") []]
          ]
          [dt | CDataDecl dt <- decls left]
        assertEqual
          "right local declarations are independent from left"
          [ DataTyWithKind
              (StructKind [Name "right", Name "extra"])
              rightS
              []
              [ Constr
                  (QualName rightS "S")
                  [TyCon (Name "bool") [], TyCon (Name "word") []]
              ],
            DataTyWithKind
              EnumKind
              rightE
              []
              [Constr (QualName rightE "B") []]
          ]
          [dt | CDataDecl dt <- decls right]
        assertContractFunctionTypes left leftS leftE
        assertContractFunctionTypes right rightS rightE

        checked <- typecheckSource sameNamedLocalTypesSource
        assertRight
          "same-spelled local types should coexist in the type table"
          checked

        let rendered = SolcorePretty.pretty resolved
        assertBool
          "semantic pretty printing keeps local declarations source-shaped"
          ( all (`isInfixOf` rendered) ["struct S", "enum E"]
              && not ("struct Left.S" `isInfixOf` rendered)
              && not ("struct Right.S" `isInfixOf` rendered)
          )
        reparsed <- parseCompUnit rendered
        case reparsed of
          Left err ->
            assertFailure
              ("pretty-printed local declarations did not parse:\n" ++ err ++ "\n" ++ rendered)
          Right reparsedUnit -> do
            reresolved <- nameResolution reparsedUnit
            assertRight
              "pretty-printed local declarations should resolve again"
              reresolved,
      testCase "local nested data types feed Generic, Storage, and ABI derivation independently" $ do
        CompUnit _ resolvedDecls <-
          resolvedSourceOrFail sameNamedLocalTypesSource
        let dispatched = contractDispatchTopDecls resolvedDecls
            inferenceDecls =
              map (ModuleInferenceDecl ModuleLocalDecl) dispatched
            localData = localDataDefsForDeriving inferenceDecls
            expectedNames =
              [ QualName (Name "Left") "S",
                QualName (Name "Left") "E",
                QualName (Name "Right") "S",
                QualName (Name "Right") "E"
              ]
            derivationSurface =
              map markerClass ["Generic", "StorageDeriving", "ABIDeriving"]
                ++ dispatched
        assertEqual
          "nested declarations are collected with canonical names"
          (sort expectedNames)
          (sort (map dataName localData))
        derived <-
          case deriveGenericTopDecls localData derivationSurface of
            Left err -> assertFailure ("unexpected derivation failure:\n" ++ err)
            Right decls' -> pure decls'
        let instances = [inst | TInstDef inst <- derived]
        mapM_
          (assertAllDerivedInstances instances)
          expectedNames,
      testCase "same-named parameter cannot capture a struct member read" $ do
        checked <-
          typecheckSourceAfterFieldDesugar $
            unlines
              [ "struct Pair { x: word; }",
                "function read(p: Pair, x: bool) returns (word) {",
                "  return p.x;",
                "}"
              ]
        case checked of
          Left err ->
            assertFailure
              ("same-named member read should typecheck:\n" ++ compilerErrorText err)
          Right (CompUnit _ typedDecls, _) ->
            case [ body
                 | TFunDef (FunDef _ sig body) <- typedDecls,
                   sigName sig == Name "read"
                 ] of
              [ [ Return
                    ( Call
                        Nothing
                        (Id selectorName selectorTy)
                        [Var (Id receiverName _)]
                      )
                  ]
                ] -> do
                  assertEqual
                    "member selector"
                    (QualName (Name "Pair") "$structField$x")
                    selectorName
                  assertEqual "receiver remains p" (Name "p") receiverName
                  assertEqual
                    "same-named bool parameter does not affect member type"
                    (funtype [TyCon (Name "Pair") []] (TyCon (Name "word") []))
                    selectorTy
              got ->
                assertFailure
                  ("member read was not lowered to one selector call: " ++ show got),
      testCase "generic nested struct reads instantiate ordered field types" $ do
        checked <-
          typecheckSourceAfterFieldDesugar $
            unlines
              [ "struct Box<a> { value: a; }",
                "struct Outer<a> { flag: bool; inner: Box<a>; }",
                "function read(o: Outer<word>) returns (word) {",
                "  return o.inner.value;",
                "}"
              ]
        case checked of
          Left err ->
            assertFailure
              ("nested generic member read should typecheck:\n" ++ compilerErrorText err)
          Right (CompUnit _ typedDecls, _) ->
            case [ body
                 | TFunDef (FunDef _ sig body) <- typedDecls,
                   sigName sig == Name "read"
                 ] of
              [ [ Return
                    ( Call
                        Nothing
                        (Id valueSelector valueSelectorTy)
                        [ Call
                            Nothing
                            (Id innerSelector innerSelectorTy)
                            [Var (Id receiverName _)]
                          ]
                      )
                  ]
                ] -> do
                  let word = TyCon (Name "word") []
                      boxWord = TyCon (Name "Box") [word]
                      outerWord = TyCon (Name "Outer") [word]
                  assertEqual
                    "outer field selector"
                    (QualName (Name "Outer") "$structField$inner")
                    innerSelector
                  assertEqual
                    "nested field selector"
                    (QualName (Name "Box") "$structField$value")
                    valueSelector
                  assertEqual "outer generic field type" (funtype [outerWord] boxWord) innerSelectorTy
                  assertEqual "nested generic field type" (funtype [boxWord] word) valueSelectorTy
                  assertEqual "receiver remains o" (Name "o") receiverName
              got ->
                assertFailure
                  ("nested member reads were not lowered correctly: " ++ show got),
      testCase "contract-local struct reads use the canonical local type" $ do
        checked <-
          typecheckSourceAfterFieldDesugar
            "contract C { struct Local { value: word; } function read(p: Local) returns (word) { return p.value; } }"
        case checked of
          Left err ->
            assertFailure
              ("contract-local member read should typecheck:\n" ++ compilerErrorText err)
          Right (CompUnit _ typedDecls, _) ->
            case [ selectorName
                 | TContr contractDef <- typedDecls,
                   CFunDecl (FunDef _ sig [Return (Call Nothing (Id selectorName _) [_])]) <- decls contractDef,
                   sigName sig == Name "read"
                 ] of
              [selectorName] ->
                assertEqual
                  "contract-local selector uses the canonical type name"
                  (QualName (QualName (Name "C") "Local") "$structField$value")
                  selectorName
              got ->
                assertFailure
                  ("contract-local member read was not lowered: " ++ show got),
      testCase "side-effecting struct receivers occur once in the selector call" $ do
        checked <-
          typecheckSourceAfterFieldDesugar $
            unlines
              [ "struct Pair { x: word; }",
                "function make(x: word) returns (Pair) { return Pair.Pair(x); }",
                "function read(x: word) returns (word) { return make(x).x; }"
              ]
        case checked of
          Left err ->
            assertFailure
              ("call receiver member read should typecheck:\n" ++ compilerErrorText err)
          Right (CompUnit _ typedDecls, _) ->
            case [ body
                 | TFunDef (FunDef _ sig body) <- typedDecls,
                   sigName sig == Name "read"
                 ] of
              [ [ Return
                    ( Call
                        Nothing
                        (Id _ _)
                        [Call Nothing (Id makeName _) [Var (Id argumentName _)]]
                      )
                  ]
                ] ->
                  do
                    assertEqual
                      "receiver call appears as the selector's single argument"
                      (Name "make")
                      makeName
                    assertEqual
                      "receiver call argument is not duplicated"
                      (Name "x")
                      argumentName
              got ->
                assertFailure
                  ("receiver was duplicated or not lowered: " ++ show got),
      testCase "unknown struct member reports an undefined-field diagnostic" $ do
        checked <-
          typecheckSourceAfterFieldDesugar $
            unlines
              [ "struct Pair { x: word; }",
                "function read(p: Pair) returns (word) {",
                "  return p.missing;",
                "}"
              ]
        assertLocatedFieldDiagnostic
          "unknown struct member"
          "SC0204"
          checked,
      testCase "struct member assignment fails safely before lowering" $ do
        checked <-
          typecheckSourceAfterFieldDesugar $
            unlines
              [ "struct Pair { x: word; }",
                "function write(p: Pair, value: word) {",
                "  p.x = value;",
                "  return;",
                "}"
              ]
        assertLocatedFieldDiagnostic
          "struct member assignment"
          "SC0231"
          checked,
      testCase "interface signature has no body to typecheck" $ do
        checked <-
          typecheckSource $
            unlines
              [ "interface Reader {",
                "  function read(key: word) external view returns (word);",
                "}"
              ]
        case checked of
          Left err ->
            assertFailure
              ("interface signature should typecheck:\n" ++ compilerErrorText err)
          Right (CompUnit _ [TContr (ContractWithKind InterfaceKind _ _ [CSignatureDecl isExternal sig])], _) -> do
            assertBool "external visibility is preserved" isExternal
            assertEqual "signature name" (Name "read") (sigName sig)
            assertEqual "signature return" (Just wordTy) (sigReturn sig)
            assertEqual
              "interface visibility and mutability survive typechecking"
              [ VisibilityModifier VisibilityExternal,
                MutabilityModifier MutabilityView
              ]
              (sigModifiers sig)
          Right other ->
            assertFailure ("unexpected typed interface shape: " ++ show (fst other)),
      testCase "contract visibility and mutability survive typechecking" $
        assertTypedFunctionModifiers
          ContractKind
          "read"
          [ VisibilityModifier VisibilityPublic,
            MutabilityModifier MutabilityView
          ]
          "contract Reader { function read(x: word) public view returns (word) { return x; } }",
      testCase "library visibility and mutability survive typechecking" $
        assertTypedFunctionModifiers
          LibraryKind
          "twice"
          [ VisibilityModifier VisibilityInternal,
            MutabilityModifier MutabilityPure
          ]
          "library Math { function twice(x: word) internal pure returns (word) { return x; } }",
      testCase "empty interface remains non-runtime through semantic passes" $
        assertContractKindLifecycle
          "Empty"
          InterfaceKind
          False
          "interface Empty {}",
      testCase "nonempty interface remains non-runtime through semantic passes" $
        assertContractKindLifecycle
          "Reader"
          InterfaceKind
          False
          "interface Reader { function read(key: word) external returns (word); }",
      testCase "library remains non-runtime through semantic passes" $
        assertContractKindLifecycle
          "Math"
          LibraryKind
          False
          "library Math { function twice(x: word) public returns (word) { return x; } }",
      testCase "ordinary contract remains the only runtime declaration kind" $
        assertContractKindLifecycle
          "Live"
          ContractKind
          True
          "contract Live { function ping() public { return; } }",
      testCase "ordinary contract function still checks its empty body" $ do
        checked <-
          typecheckSource $
            unlines
              [ "contract Reader {",
                "  function read(key: word) external view returns (word) {}",
                "}"
              ]
        assertLeft "non-unit contract function with an empty body" checked,
      testCase "bare revert terminates a non-unit function" $ do
        checked <-
          typecheckSource
            "function abort() returns (word) { revert; }"
        assertRight "bare revert in word-returning function" checked,
      testCase "bare revert satisfies a non-unit conditional branch" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function choose(flag: bool, value: word) returns (word) {",
                "  if (flag) { return value; } else { revert; }",
                "}"
              ]
        assertRight "bare revert in word-returning branch" checked,
      testCase "Yul loop control is rejected outside a loop body" $ do
        breakResult <-
          typecheckSource
            "function badBreak() { assembly { break } return; }"
        continueResult <-
          typecheckSource
            "function badContinue() { assembly { continue } return; }"
        assertLeftContaining
          "top-level Yul break"
          "only valid inside a for-loop body"
          breakResult
        assertLeftContaining
          "top-level Yul continue"
          "only valid inside a for-loop body"
          continueResult,
      testCase "Yul leave is rejected outside a Yul function" $ do
        checked <-
          typecheckSource
            "function badLeave() { assembly { leave } return; }"
        assertLeftContaining
          "top-level Yul leave"
          "only valid inside a Yul function"
          checked,
      testCase "Yul control transfer is accepted in its lexical context" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function validControl() {",
                "  assembly {",
                "    for {} true {} { continue break }",
                "    function stop() { leave }",
                "  }",
                "  return;",
                "}"
              ]
        assertRight "well-scoped Yul control transfer" checked,
      testCase "omitted returns clause is a fully annotated unit return" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function nop() {",
                "  return;",
                "}"
              ]
        assertRight "unit function with omitted returns clause" checked,
      testCase "fallback with omitted returns clause typechecks as unit" $ do
        checked <-
          typecheckSource $
            unlines
              [ "contract Receiver {",
                "  fallback() external {",
                "    return;",
                "  }",
                "}"
              ]
        assertRight "unit fallback" checked,
      testCase "named return is in scope and supports bare return" $ do
        checked <-
          typecheckSource $
            unlines
              [ "contract Reader {",
                "  function read(x: word) external returns (result: word) {",
                "    result = x;",
                "    return;",
                "  }",
                "}"
              ]
        case checked of
          Left err ->
            assertFailure
              ("named return should typecheck:\n" ++ compilerErrorText err)
          Right (CompUnit _ typedDecls, _) ->
            case [ sig
                 | TContr (ContractWithKind ContractKind contractName _ contractDecls) <- typedDecls,
                   contractName == Name "Reader",
                   CFunDecl (FunDef _ sig _) <- contractDecls,
                   sigName sig == Name "read"
                 ] of
              [sig] ->
                do
                  assertEqual
                    "legacy return-name view survives typechecking"
                    [Just (Name "result")]
                    (sigReturnNames sig)
                  case sigReturnItems sig of
                    [returnItem] -> do
                      assertEqual
                        "return item name survives typechecking"
                        (Just (Name "result"))
                        (signatureReturnItemName returnItem)
                      assertBool
                        "runtime return item remains runtime"
                        (not (signatureReturnItemComptime returnItem))
                      assertEqual
                        "return item type tracks the aggregate result"
                        (Just (signatureReturnItemType returnItem))
                        (sigReturn sig)
                    returnItems ->
                      assertFailure
                        ("unexpected typed return-item metadata: " ++ show returnItems)
              other ->
                assertFailure ("unexpected typed named-return signatures: " ++ show other),
      testCase "comptime named return supports assignment and bare return" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function staged(comptime x: word) returns (comptime result: word) {",
                "  result = x;",
                "  return;",
                "}"
              ]
        case checked of
          Left err ->
            assertFailure
              ("comptime named return should typecheck:\n" ++ compilerErrorText err)
          Right (typed@(CompUnit _ typedDecls), _) -> do
            case [ (sig, body)
                 | TFunDef (FunDef _ sig body) <- typedDecls,
                   sigName sig == Name "staged"
                 ] of
              [(sig, Let isComptime _ _ Nothing : _)] -> do
                assertBool "named return local remains comptime" isComptime
                assertBool "aggregate return remains comptime" (sigRetComptime sig)
                assertEqual
                  "per-item comptime metadata survives typechecking"
                  [True]
                  (map signatureReturnItemComptime (sigReturnItems sig))
              other ->
                assertFailure
                  ("unexpected comptime named-return function: " ++ show other)
            case checkComptimeEarly typed of
              Left err ->
                assertFailure
                  ("comptime named bare return failed early checking:\n" ++ err)
              Right () -> pure (),
      testCase "uninitialized comptime binding rejects a runtime assignment" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function bad(x: word) returns (word) {",
                "  let comptime result: word;",
                "  result = x;",
                "  return result;",
                "}"
              ]
        case checked of
          Left err ->
            assertFailure
              ("runtime-assignment fixture should typecheck first:\n" ++ compilerErrorText err)
          Right (typed, _) ->
            case checkComptimeEarly typed of
              Left err ->
                assertBool
                  ("unexpected comptime diagnostic: " ++ err)
                  ("comptime variable" `isInfixOf` err)
              Right () ->
                assertFailure
                  "runtime assignment to an uninitialized comptime binding was accepted",
      testCase "mixed comptime return items fail explicitly" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function mixed() returns (left: word, comptime right: bool) {",
                "  left = 1;",
                "  right = true;",
                "  return;",
                "}"
              ]
        case checked of
          Left err -> do
            assertLeftContaining
              "mixed comptime return mode"
              "SC0123"
              (Left err)
            assertLeftContaining
              "mixed comptime return mode"
              "mixed comptime and runtime return items are not supported"
              (Left err)
            assertBool
              "mixed return-mode diagnostic is source-located"
              (any ((/= Nothing) . diagnosticPrimarySpan) (compilerErrorDiagnostics err))
          Right _ ->
            assertFailure "mixed comptime return mode should fail explicitly",
      testCase "bare return remains invalid for unnamed non-unit result" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function bad() returns (word) {",
                "  return;",
                "}"
              ]
        assertLeft "unnamed word return requires a value" checked,
      testCase "explicit unit return is not lowered as a named bare return" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function bad(x: word) returns (result: word) {",
                "  result = x;",
                "  return ();",
                "}"
              ]
        assertLeft "explicit unit cannot satisfy a word return" checked,
      testCase "named return cannot reuse a parameter name" $ do
        checked <-
          typecheckSource
            "function bad(result: word) returns (result: word) { return; }"
        assertLeftContaining "parameter/return collision" "SC0108" checked,
      testCase "named return declarations must be unique" $ do
        checked <-
          typecheckSource
            "function bad() returns (result: word, result: word) { return; }"
        assertLeftContaining "duplicate named returns" "SC0108" checked,
      testCase "nested local cannot shadow a named return" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function bad(x: word) returns (result: word) {",
                "  if (true) {",
                "    let result: word = x;",
                "    return;",
                "  } else {",
                "    result = x;",
                "    return;",
                "  }",
                "}"
              ]
        assertLeftContaining "nested named-return shadow" "SC0108" checked,
      testCase "match binder cannot shadow a named return" $ do
        checked <-
          typecheckSource $
            unlines
              [ "function bad(x: word) returns (result: word) {",
                "  match (x) {",
                "    case result { return; }",
                "  }",
                "}"
              ]
        assertLeftContaining "match named-return shadow" "SC0108" checked,
      testCase "selective struct import preserves source metadata and pretty round-trips" $ do
        graphResult <-
          loadModuleGraph
            "test/imports"
            Nothing
            []
            "test/imports/struct_metadata_main.solc"
        graph <-
          case graphResult of
            Left err -> assertFailure ("unexpected module load failure:\n" ++ err)
            Right loadedGraph -> pure loadedGraph
        surface <-
          case moduleLocalTypeCheckSurface graph (entryModule graph) of
            Left err -> assertFailure ("unexpected module surface failure:\n" ++ err)
            Right loadedSurface -> pure loadedSurface
        importedStruct <-
          case [ dt
               | Source.TDataDef dt <- moduleSurfaceImportedDecls surface,
                 Source.dataName dt == "RenamedPair"
               ] of
            [dt] -> pure dt
            unexpectedDecls ->
              assertFailure
                ("expected one imported RenamedPair declaration, got " ++ show unexpectedDecls)
        assertEqual
          "renamed struct retains kind, field names, and field types"
          (Source.StructTy "RenamedPair" [] ["left", "right"] [Source.TyCon "word" [], Source.TyCon "bool" []])
          importedStruct
        let rendered = TreePretty.pretty (Source.TDataDef importedStruct)
        reparsed <- parseCompUnit rendered
        case reparsed of
          Left err -> assertFailure ("pretty-printed imported struct did not parse:\n" ++ err)
          Right unit ->
            assertEqual
              ("round trip: " ++ rendered)
              (Source.CompUnit [] [Source.TDataDef importedStruct])
              unit
    ]

sameNamedLocalTypesSource :: String
sameNamedLocalTypesSource =
  unlines
    [ "contract Left {",
      "  struct S { left: word; }",
      "  enum E { A }",
      "  function echoLeft(value: S) returns (S) { return value; }",
      "  function tagLeft() returns (E) { return E.A; }",
      "}",
      "contract Right {",
      "  struct S { right: bool; extra: word; }",
      "  enum E { B }",
      "  function echoRight(value: S) returns (S) { return value; }",
      "  function tagRight() returns (E) { return E.B; }",
      "}"
    ]

resolvedSourceOrFail :: String -> IO (CompUnit Name)
resolvedSourceOrFail source = do
  parsedResult <- parseCompUnit source
  parsed <-
    case parsedResult of
      Left err -> assertFailure ("unexpected parse failure:\n" ++ err)
      Right compUnit -> pure compUnit
  resolvedResult <- nameResolution parsed
  case resolvedResult of
    Left err ->
      assertFailure
        ("unexpected name-resolution failure:\n" ++ compilerErrorText err)
    Right compUnit -> pure compUnit

assertContractFunctionTypes :: Contract Name -> Name -> Name -> Assertion
assertContractFunctionTypes contractDef structName enumName =
  assertEqual
    ("local references in " ++ show (name contractDef))
    [ ([TyCon structName []], Just (TyCon structName [])),
      ([], Just (TyCon enumName []))
    ]
    [ ([ty | Typed _ _ ty <- sigParams sig], sigReturn sig)
    | CFunDecl (FunDef _ sig _) <- decls contractDef
    ]

markerClass :: String -> TopDecl Name
markerClass className' =
  TClassDef
    ( Class
        []
        []
        (Name className')
        []
        (TVar (Name "_self"))
        []
    )

assertAllDerivedInstances :: [Instance Name] -> Name -> Assertion
assertAllDerivedInstances instances typeName =
  mapM_ assertDerived ["Generic", "StorageSize", "CanStore", "ABIAttribs", "ABIDecode"]
  where
    nominalType = TyCon typeName []
    assertDerived className' =
      assertEqual
        (className' ++ " derivation for " ++ show typeName)
        1
        ( length
            [ ()
            | inst <- instances,
              instName inst == Name className',
              mainTy inst == nominalType || nominalType `elem` paramsTy inst
            ]
        )

assertRight :: String -> Either CompilerError a -> Assertion
assertRight _ (Right _) = pure ()
assertRight label (Left err) =
  assertFailure (label ++ ": unexpected failure:\n" ++ compilerErrorText err)

assertLeft :: String -> Either CompilerError a -> Assertion
assertLeft _ (Left _) = pure ()
assertLeft label (Right _) =
  assertFailure (label ++ ": expected failure")

assertLeftContaining :: String -> String -> Either CompilerError a -> Assertion
assertLeftContaining label needle (Left err) =
  assertBool
    (label ++ ": expected diagnostic containing " ++ show needle ++ "\n" ++ compilerErrorText err)
    (needle `isInfixOf` compilerErrorText err)
assertLeftContaining label _ (Right _) =
  assertFailure (label ++ ": expected failure")

assertLocatedFunctionTypeError ::
  String ->
  String ->
  Either CompilerError a ->
  Assertion
assertLocatedFunctionTypeError label expectedMessage (Left err) = do
  assertLeftContaining label "SC0122" (Left err)
  assertLeftContaining label expectedMessage (Left err)
  assertBool
    (label ++ ": expected a source-located diagnostic")
    (any ((/= Nothing) . diagnosticPrimarySpan) (compilerErrorDiagnostics err))
assertLocatedFunctionTypeError label _ (Right _) =
  assertFailure (label ++ ": expected failure")

assertLocatedLoopControlError ::
  String ->
  String ->
  Either CompilerError a ->
  Assertion
assertLocatedLoopControlError label keyword (Left err) = do
  assertLeftContaining label "SC0125" (Left err)
  assertLeftContaining label (keyword ++ " statement outside of a loop") (Left err)
  assertBool
    (label ++ ": expected a source-located diagnostic")
    (any ((/= Nothing) . diagnosticPrimarySpan) (compilerErrorDiagnostics err))
assertLocatedLoopControlError label _ (Right _) =
  assertFailure (label ++ ": expected failure")

assertContractKindLifecycle :: String -> ContractKind -> Bool -> String -> Assertion
assertContractKindLifecycle contractName expectedKind shouldGenerateRuntime source = do
  parsedResult <- parseCompUnit source
  parsed <-
    case parsedResult of
      Left err -> assertFailure ("unexpected parse failure:\n" ++ err)
      Right compUnit -> pure compUnit
  resolvedResult <- nameResolution parsed
  CompUnit resolvedImports resolvedDecls <-
    case resolvedResult of
      Left err ->
        assertFailure
          ("unexpected name-resolution failure:\n" ++ compilerErrorText err)
      Right compUnit -> pure compUnit
  assertSemanticKind "name resolution" contractName expectedKind resolvedDecls

  let fieldDesugared = fieldDesugarTopDecls resolvedDecls
  assertSemanticKind "field desugaring" contractName expectedKind fieldDesugared
  if shouldGenerateRuntime
    then
      assertBool
        "ordinary contract receives its storage-context declaration"
        (any isGeneratedDataDecl fieldDesugared)
    else
      assertEqual
        "non-runtime declaration kind is unchanged by field desugaring"
        resolvedDecls
        fieldDesugared

  let dispatched = contractDispatchTopDecls resolvedDecls
      dispatchedContract = findSemanticContract contractName dispatched
      dispatchedDecls = decls dispatchedContract
      hasMain =
        any
          isMainDecl
          dispatchedDecls
      hasDeployer =
        any
          isDeployerDecl
          dispatchedDecls
  assertSemanticKind "dispatch generation" contractName expectedKind dispatched
  assertEqual "runtime main generation" shouldGenerateRuntime hasMain
  assertEqual "default constructor deployer generation" shouldGenerateRuntime hasDeployer
  if shouldGenerateRuntime
    then
      assertBool
        "ordinary public method receives dispatch declarations"
        (any isGeneratedDataDecl dispatched)
    else
      assertEqual
        "non-runtime declaration kind is unchanged by dispatch generation"
        resolvedDecls
        dispatched

  sccResult <- sccAnalysisTopDecls resolvedDecls
  sccDecls <-
    case sccResult of
      Left err -> assertFailure ("unexpected SCC failure:\n" ++ err)
      Right topDecls -> pure topDecls
  assertSemanticKind "SCC reconstruction" contractName expectedKind sccDecls

  (directDecls, _) <- indirectCallTopDecls resolvedDecls
  assertSemanticKind "indirect-call reconstruction" contractName expectedKind directDecls

  checked <-
    typeInferTopDeclChecks
      stdOpt
      resolvedImports
      []
      []
      [ TopDeclCheck CheckTopDeclBody decl
      | decl <- resolvedDecls
      ]
  (typed, tcEnv) <-
    case checked of
      Left err ->
        assertFailure
          ("unexpected typecheck failure:\n" ++ compilerErrorText err)
      Right result -> pure result
  assertSemanticKind "type inference" contractName expectedKind (contracts typed)

  compiledResult <- matchCompiler typed
  compiled <-
    case compiledResult of
      Left err -> assertFailure ("unexpected match compilation failure:\n" ++ err)
      Right (compUnit, _) -> pure compUnit
  assertSemanticKind "match compilation" contractName expectedKind (contracts compiled)

  let specialisationInput =
        if shouldGenerateRuntime
          then addTestMain compiled
          else compiled
  specialised <- specialiseCompUnit specialisationInput False tcEnv
  let runtimeContractNames =
        [ runtimeName
        | MastTContr runtimeContract <- mastTopDecls specialised,
          let runtimeName = mastContrName runtimeContract
        ]
  assertEqual
    "specialisation runtime target"
    (if shouldGenerateRuntime then [Name contractName] else [])
    runtimeContractNames
  where
    isGeneratedDataDecl (TDataDef _) = True
    isGeneratedDataDecl _ = False
    isMainDecl (CFunDecl (FunDef _ sig _)) = sigName sig == Name "main"
    isMainDecl _ = False
    isDeployerDecl (CFunDecl (FunDef _ sig _)) = sigName sig == deployerName
    isDeployerDecl _ = False

addTestMain :: CompUnit Id -> CompUnit Id
addTestMain (CompUnit imps topDecls) =
  CompUnit imps (map addMain topDecls)
  where
    addMain (TContr contractDef)
      | contractKind contractDef == ContractKind =
          TContr
            contractDef
              { decls =
                  CFunDecl
                    ( FunDef
                        False
                        SignatureWithReturnNames
                          { sigVars = [],
                            sigContext = [],
                            sigName = Name "main",
                            sigParams = [],
                            sigRetComptime = False,
                            sigReturn = Just (TyCon (Name "()") []),
                            sigPayable = False,
                            sigReturnNames = [],
                            sigReturnItems = [],
                            sigModifiers = []
                          }
                        []
                    )
                    : decls contractDef
              }
    addMain topDecl = topDecl

assertSemanticKind :: String -> String -> ContractKind -> [TopDecl a] -> Assertion
assertSemanticKind phase contractName expectedKind topDecls =
  assertEqual
    (phase ++ " contract kind")
    expectedKind
    (contractKind (findSemanticContract contractName topDecls))

findSemanticContract :: String -> [TopDecl a] -> Contract a
findSemanticContract contractName topDecls =
  case [ contractDef
       | TContr contractDef <- topDecls,
         name contractDef == Name contractName
       ] of
    [contractDef] -> contractDef
    contractsFound ->
      error
        ( "expected exactly one contract named "
            ++ show contractName
            ++ ", got "
            ++ show (length contractsFound)
        )

typecheckSource :: String -> IO (Either CompilerError (CompUnit Id, TcEnv))
typecheckSource source = do
  parsedResult <- parseCompUnit source
  parsed <-
    case parsedResult of
      Left err -> assertFailure ("unexpected parse failure:\n" ++ err)
      Right compUnit -> pure compUnit
  resolvedResult <- nameResolution parsed
  case resolvedResult of
    Left err -> pure (Left err)
    Right (CompUnit resolvedImports resolvedDecls) ->
      typeInferTopDeclChecks
        stdOpt
        resolvedImports
        []
        []
        [ TopDeclCheck CheckTopDeclBody decl
        | decl <- resolvedDecls
        ]

typecheckSourceAfterFieldDesugar :: String -> IO (Either CompilerError (CompUnit Id, TcEnv))
typecheckSourceAfterFieldDesugar source = do
  parsedResult <- parseCompUnit source
  parsed <-
    case parsedResult of
      Left err -> assertFailure ("unexpected parse failure:\n" ++ err)
      Right compUnit -> pure compUnit
  resolvedResult <- nameResolution parsed
  case resolvedResult of
    Left err -> pure (Left err)
    Right (CompUnit resolvedImports resolvedDecls) ->
      typeInferTopDeclChecks
        stdOpt
        resolvedImports
        []
        []
        [ TopDeclCheck CheckTopDeclBody decl
        | decl <- fieldDesugarTopDecls resolvedDecls
        ]

assertLocatedFieldDiagnostic ::
  String ->
  String ->
  Either CompilerError a ->
  Assertion
assertLocatedFieldDiagnostic label expectedCode (Left err) = do
  assertLeftContaining label expectedCode (Left err)
  assertBool
    (label ++ ": expected a source-located diagnostic")
    (any ((/= Nothing) . diagnosticPrimarySpan) (compilerErrorDiagnostics err))
assertLocatedFieldDiagnostic label _ (Right _) =
  assertFailure (label ++ ": expected failure")

assertTypedFunctionModifiers ::
  ContractKind ->
  String ->
  [FunctionModifier] ->
  String ->
  Assertion
assertTypedFunctionModifiers expectedKind functionName expectedModifiers source = do
  checked <- typecheckSource source
  case checked of
    Left err ->
      assertFailure
        ("modifier lifecycle fixture failed typechecking:\n" ++ compilerErrorText err)
    Right (CompUnit _ typedDecls, _) -> do
      let contractDef =
            case [c | TContr c <- typedDecls, contractKind c == expectedKind] of
              [c] -> c
              other ->
                error
                  ( "expected one "
                      ++ show expectedKind
                      ++ ", got "
                      ++ show (length other)
                  )
          matchingModifiers =
            [ sigModifiers sig
            | sig <- contractSignatures (decls contractDef),
              sigName sig == Name functionName
            ]
      assertEqual
        ("typed modifiers for " ++ functionName)
        [expectedModifiers]
        matchingModifiers
  where
    contractSignatures = concatMap fromDecl
    fromDecl (CFunDecl (FunDef _ sig _)) = [sig]
    fromDecl (CSignatureDecl _ sig) = [sig]
    fromDecl (CMutualDecl nestedDecls) = contractSignatures nestedDecls
    fromDecl _ = []

moduleInput :: [ModuleInferenceDecl] -> ModuleTypeCheckInput
moduleInput inferenceDecls =
  withPreparedModuleInferenceDecls (resolvedModuleInput inferenceDecls) inferenceDecls

resolvedModuleInput :: [ModuleInferenceDecl] -> ModuleResolvedTypeCheckInput
resolvedModuleInput inferenceDecls =
  ModuleResolvedTypeCheckInput
    { moduleResolvedInputImports = [],
      moduleResolvedInputQualifiedDecls = declsInSegment ModuleQualifiedDecl inferenceDecls,
      moduleResolvedInputLocalDecls = declsInSegment ModuleLocalDecl inferenceDecls,
      moduleResolvedInputImportedDecls = declsInSegment ModuleImportedDecl inferenceDecls,
      moduleResolvedInputTrustedInstanceHeads = [],
      moduleResolvedInputPartialImportedTypes = []
    }

declsInSegment :: ModuleDeclSegment -> [ModuleInferenceDecl] -> [TopDecl Name]
declsInSegment segment =
  map moduleInferenceDeclTopDecl
    . filter ((== segment) . moduleInferenceDeclSegment)

singleDecl :: [ModuleInferenceDecl] -> ModuleInferenceDecl
singleDecl [decl] = decl
singleDecl inferenceDecls =
  error ("expected exactly one module inference declaration, got " ++ show (length inferenceDecls))

funDecl :: String -> TopDecl Name
funDecl funName =
  TFunDef
    FunDef
      { funIsPublic = False,
        funSignature =
          wordSignature funName,
        funDefBody = [Return (Lit (IntLit 0))]
      }

wordTy :: Ty
wordTy =
  TyCon (Name "word") []

badImportedFun :: TopDecl Name
badImportedFun =
  TFunDef
    FunDef
      { funIsPublic = False,
        funSignature = wordSignature "badImported",
        funDefBody = [Return (Var (Name "missing"))]
      }

usesImportedFun :: TopDecl Name
usesImportedFun =
  TFunDef
    FunDef
      { funIsPublic = False,
        funSignature = wordSignature "usesImported",
        funDefBody = [Return (Call Nothing (Name "badImported") [])]
      }

wordSignature :: String -> Signature Name
wordSignature funName =
  SignatureWithReturnNames
    { sigVars = [],
      sigContext = [],
      sigName = Name funName,
      sigParams = [],
      sigRetComptime = False,
      sigReturn = Just wordTy,
      sigPayable = False,
      sigReturnNames = [],
      sigReturnItems = [],
      sigModifiers = []
    }
