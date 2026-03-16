module Cases where

import Solcore.Pipeline.Options
import Solcore.Pipeline.SolcorePipeline
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

stdFolder :: FilePath
stdFolder = "./std"

std :: TestTree
std =
  testGroup
    "Standard library"
    [ runTestForFile "std.solc" stdFolder,
      runTestForFile "dispatch.solc" stdFolder
    ]

comptime :: TestTree
comptime =
  testGroup
    "Compile-time evaluation"
    [ runTestForFile "CondExpr.solc" comptimeFolder,
      runTestForFile "CondStmt.solc" comptimeFolder,
      runTestForFile "Plus.solc" comptimeFolder,
      runTestForFile "OneTwo.solc" comptimeFolder,
      runTestForFile "Size.solc" comptimeFolder,
      runTestForFile "StdSize.solc" comptimeFolder,
      runTestForFile "counter.solc" comptimeFolder,
      runTestForFile "fib.solc" comptimeFolder,
      runTestForFile "string-lit-ops.solc" comptimeFolder,
      runTestForFile "string-lit-len.solc" comptimeFolder,
      runTestForFile "string-lit-keccak.solc" comptimeFolder
    ]
  where
    comptimeFolder = "./test/examples/comptime"

spec :: TestTree
spec =
  testGroup
    "Files for spec cases"
    [ runTestForFile "00answer.solc" specFolder,
      runTestForFile "01id.solc" specFolder,
      runTestForFile "02nid.solc" specFolder,
      runTestForFile "021not.solc" specFolder,
      runTestForFile "022add.solc" specFolder,
      runTestForFile "024arith.solc" specFolder,
      runTestForFile "031maybe.solc" specFolder,
      runTestForFile "032simplejoin.solc" specFolder,
      runTestForFile "033join.solc" specFolder,
      runTestForFile "034cojoin.solc" specFolder,
      runTestForFile "035padding.solc" specFolder,
      runTestForFile "036wildcard.solc" specFolder,
      runTestForFile "037dwarves.solc" specFolder,
      runTestForFile "038food0.solc" specFolder,
      runTestForFile "039food.solc" specFolder,
      runTestForFile "041pair.solc" specFolder,
      runTestForFile "042triple.solc" specFolder,
      runTestForFile "043fstsnd.solc" specFolder,
      runTestForFile "047rgb.solc" specFolder,
      runTestForFile "048rgb2.solc" specFolder,
      runTestForFile "06comp.solc" specFolder,
      runTestForFile "09not.solc" specFolder,
      runTestForFile "10negBool.solc" specFolder,
      runTestForFile "11negPair.solc" specFolder,
      runTestForFile "903badassign.solc" specFolder,
      runTestForFile "939badfood.solc" specFolder,
      runTestForFile "SimpleField.solc" specFolder,
      runTestForFile "121counter.solc" specFolder,
      runTestForFile "126nanoerc20.solc" specFolder,
      runTestForFile "127microerc20.solc" specFolder,
      runTestForFile "128minierc20.solc" specFolder
    ]
  where
    specFolder = "./test/examples/spec"

dispatches :: TestTree
dispatches =
  testGroup
    "Files for dispatch cases"
    [ runDispatchTest "basic.solc",
      runDispatchTest "stringid.solc",
      runDispatchTest "miniERC20.solc",
      runDispatchTest "Revert.solc"
    ]
  where
    runDispatchTest file = runTestForFileWith (emptyOption mempty) file "./test/examples/dispatch"

imports :: TestTree
imports =
  testGroup
    "Files for imports cases"
    [ runImportSuccess "booldef.solc",
      runImportSuccess "boolmain.solc",
      runImportSuccess "boolalias.solc",
      runImportFailure "boolalias_open_fail.solc",
      runImportSuccess "boolqualified.solc",
      runImportSuccess "boolqualifiedtype.solc",
      runImportSuccess "boolaliastype.solc",
      runImportFailure "module_unqualified_fun_fail.solc",
      runImportFailure "alias_unqualified_fun_fail.solc",
      runImportFailure "module_unqualified_type_fail.solc",
      runImportFailure "alias_unqualified_type_fail.solc",
      runImportFailure "module_unqualified_constr_fail.solc",
      runImportFailure "alias_unqualified_constr_fail.solc",
      runImportSuccess "selective_unqualified_fun_ok.solc",
      runImportSuccess "transitive_dep_main_module.solc",
      runImportSuccess "transitive_dep_main_select.solc",
      runImportSuccess "module_name_shadow.solc",
      runImportSuccess "wrapper_shadow_success.solc",
      runImportSuccess "ns_cross_ok.solc",
      runImportSuccess "ns_constr_dup.solc",
      runImportFailure "strict_open_fail.solc",
      runImportSuccess "boolselect.solc",
      runImportSuccess "boolconselect_ok.solc",
      runImportFailure "boolconselect_fail.solc",
      runImportSuccess "nested_alias.solc",
      runImportSuccess "nested_select.solc",
      runImportSuccess "nested_foo_and_bar.solc",
      runImportSuccess "nested_direct_qualifier.solc",
      runImportSuccess "nested_deep_qualifier.solc",
      runImportSuccess "glob_import_ok.solc",
      runImportFailure "glob_import_mixed_fail.solc",
      runImportFailure "glob_import_dup_fail.solc",
      runImportFailure "glob_export_mixed_fail.solc",
      runImportFailure "glob_amb_main_fail.solc",
      runImportSuccess "select_ok.solc",
      runImportSuccess "select_shadow_local.solc",
      runImportFailure "select_fail.solc",
      runImportFailure "select_unknown.solc",
      runImportFailure "select_dup_item.solc",
      runImportFailure "alias_dup.solc",
      runImportFailure "amb_main.solc",
      runImportSuccess "amb_ok.solc",
      runImportSuccess "dupqual_main.solc",
      runImportSuccess "dupqual_module_main.solc",
      runImportSuccess "private_helper_main.solc",
      runImportSuccess "module_qualified_constructor.solc",
      runImportSuccess "module_qualified_constructor_alias.solc",
      runImportSuccess "type_collision_main.solc",
      runImportSuccess "dot_context_expr.solc",
      runImportSuccess "reexport_items_main.solc",
      runImportSuccess "reexport_module_main.solc",
      runImportSuccess "external_lib_main.solc",
      runImportSuccess "import_std_minimal.solc",
      runImportFailure "external_lib_missing_fail.solc",
      runImportFailure "pragma_scope_main.solc",
      runImportSuccess "selfcycle.solc",
      runImportSuccess "cycle_main.solc",
      runImportSuccess "wild_main.solc",
      runImportFailure "leak_main.solc"
    ]
  where
    importFolder = "./test/imports"
    importOpt =
      stdOpt
        { optNoGenDispatch = True,
          optExternalLibs = ["extlib=./test/imports/extlib"]
        }
    runImportSuccess file = runTestForFileWith importOpt file importFolder
    runImportFailure file = runTestExpectingFailureWith importOpt file importFolder

pragmas :: TestTree
pragmas =
  testGroup
    "Files for pragmas cases"
    [ runTestExpectingFailure "bound.solc" pragmaFolder,
      runTestForFile "coverage.solc" pragmaFolder,
      runTestForFile "patterson.solc" pragmaFolder
    ]
  where
    pragmaFolder = "./test/examples/pragmas"

cases :: TestTree
cases =
  testGroup
    "Files for folder cases"
    [ runTestForFile "Ackermann.solc" caseFolder,
      runTestForFile "Add1.solc" caseFolder,
      runTestExpectingFailure "add-moritz.solc" caseFolder,
      runTestForFile "another-subst.solc" caseFolder,
      runTestForFileWith noDesugarOpt "app.solc" caseFolder,
      runTestForFile "array.solc" caseFolder,
      runTestForFile "assembly.solc" caseFolder,
      runTestForFile "bal.solc" caseFolder,
      runTestExpectingFailure "BadInstance.solc" caseFolder,
      runTestForFile "BoolNot.solc" caseFolder,
      runTestExpectingFailure "bound-minimal.solc" caseFolder,
      runTestExpectingFailure "bound-only-test.solc" caseFolder,
      runTestForFile "bound-merge-case.solc" caseFolder,
      runTestForFile "bound-with-pragma.solc" caseFolder,
      runTestExpectingFailure "class-type-name-collision.solc" caseFolder,
      runTestForFile "class-context.solc" caseFolder,
      runTestForFile "closure.solc" caseFolder,
      runTestForFile "closure-capture-only.solc" caseFolder,
      runTestForFileWith noDesugarOpt "Compose.solc" caseFolder,
      runTestForFile "Compose2.solc" caseFolder,
      runTestForFile "Compose3.solc" caseFolder,
      -- The following test makes the test runner throw an exception
      -- , runTestForFile "comp.solc" caseFolder
      runTestForFile "compose0.solc" caseFolder,
      runTestForFileWith noDesugarOpt "compose_desugared.solc" caseFolder,
      runTestForFile "comparisons.solc" caseFolder,
      runTestForFile "CondExp.solc" caseFolder,
      runTestForFile "constrained-instance.solc" caseFolder,
      runTestForFile "constrained-instance-context.solc" caseFolder,
      runTestForFile "const.solc" caseFolder,
      runTestExpectingFailure "const-array.solc" caseFolder,
      runTestForFile "constructor-weak-args.solc" caseFolder,
      runTestExpectingFailure "complexproxy.solc" caseFolder,
      runTestForFile "cyclical-defs.solc" caseFolder,
      runTestForFile "cyclical-defs-inferred.solc" caseFolder,
      runTestExpectingFailure "default-inst.solc" caseFolder,
      runTestExpectingFailure "default-instance-missing.solc" caseFolder,
      runTestExpectingFailure "default-instance-weak.solc" caseFolder,
      runTestForFile "dot-expression-constructor.solc" caseFolder,
      runTestForFile "dot-expression-call-arg-context.solc" caseFolder,
      runTestForFile "dot-expression-match-return.solc" caseFolder,
      runTestForFile "dot-expression-nested-context.solc" caseFolder,
      runTestForFile "dot-expression-assignment-context.solc" caseFolder,
      runTestExpectingFailure "dot-expression-no-context-fail.solc" caseFolder,
      runTestExpectingFailure "dot-expression-unknown-fail.solc" caseFolder,
      runTestForFile "dot-pattern-constructor.solc" caseFolder,
      runTestForFile "dot-pattern-nested-constructor.solc" caseFolder,
      runTestForFile "dot-primitive-constructor.solc" caseFolder,
      runTestExpectingFailure "duplicated-contract-name.solc" caseFolder,
      runTestExpectingFailure "duplicated-type-name.solc" caseFolder,
      runTestForFile "DuplicateFun.solc" caseFolder,
      runTestExpectingFailure "DupFun.solc" caseFolder,
      runTestForFile "EitherModule.solc" caseFolder,
      runTestForFile "empty-asm.solc" caseFolder,
      runTestExpectingFailure "Enum.solc" caseFolder,
      runTestExpectingFailure "Eq.solc" caseFolder,
      runTestForFile "EqQual.solc" caseFolder,
      runTestForFile "EvenOdd.solc" caseFolder,
      runTestExpectingFailure "Filter.solc" caseFolder,
      runTestForFile "foo-class.solc" caseFolder,
      runTestForFile "Foo.solc" caseFolder,
      runTestExpectingFailure "GetSet.solc" caseFolder,
      runTestExpectingFailure "GoodInstance.solc" caseFolder,
      runTestForFile "Id.solc" caseFolder,
      runTestForFile "if-examples.solc" caseFolder,
      runTestExpectingFailure "index-example.solc" caseFolder,
      runTestExpectingFailure "IndexLib.solc" caseFolder,
      runTestForFile "import-std.solc" caseFolder,
      runTestForFile "inc-closure.solc" caseFolder,
      runTestExpectingFailure "IncompleteInstDef.solc" caseFolder,
      runTestExpectingFailure "instance-wrong-sig.solc" caseFolder,
      runTestExpectingFailure "Invokable.solc" caseFolder,
      runTestForFile "ixa.solc" caseFolder,
      runTestForFile "join.solc" caseFolder,
      runTestExpectingFailure "joinErr.solc" caseFolder,
      runTestExpectingFailure "KindTest.solc" caseFolder,
      runTestExpectingFailure "listeq.solc" caseFolder,
      runTestForFile "ListModule.solc" caseFolder,
      runTestForFile "listid.solc" caseFolder,
      runTestForFile "Logic.solc" caseFolder,
      runTestExpectingFailure "mainproxy.solc" caseFolder,
      runTestForFile "MatchCall.solc" caseFolder,
      runTestForFile "match-yul.solc" caseFolder,
      runTestForFile "memory.solc" caseFolder,
      runTestForFile "Memory1.solc" caseFolder,
      runTestForFile "Memory2.solc" caseFolder,
      runTestExpectingFailure "missing-instance.solc" caseFolder,
      runTestForFile "modifier.solc" caseFolder,
      runTestForFile "morefun.solc" caseFolder,
      runTestForFile "Mutuals.solc" caseFolder,
      runTestExpectingFailure "nano-desugared.solc" caseFolder,
      runTestForFile "NegPair.solc" caseFolder,
      runTestForFile "nid.solc" caseFolder,
      runTestForFile "noclosure.solc" caseFolder,
      runTestExpectingFailure "noconstr.solc" caseFolder,
      runTestForFile "notif.solc" caseFolder,
      runTestForFile "Option.solc" caseFolder,
      runTestForFile "option2.solc" caseFolder,
      runTestExpectingFailure "overlapping-heads.solc" caseFolder,
      runTestForFile "Pair.solc" caseFolder,
      runTestExpectingFailure "PairMatch1.solc" caseFolder,
      runTestExpectingFailure "PairMatch2.solc" caseFolder,
      -- failing due to missing assign constraint
      runTestExpectingFailure "patterson-bug.solc" caseFolder,
      runTestForFile "Peano.solc" caseFolder,
      runTestForFile "PeanoMatch.solc" caseFolder,
      runTestForFile "polymatch-error.solc" caseFolder,
      runTestExpectingFailure "pragma_merge_fail_coverage.solc" caseFolder,
      runTestExpectingFailure "pragma_merge_fail_patterson.solc" caseFolder,
      runTestForFile "pragma_merge_base.solc" caseFolder,
      runTestExpectingFailure "pragma_merge_import.solc" caseFolder,
      runTestExpectingFailure "pragma_merge_verify.solc" caseFolder,
      runTestForFile "pragma_test_patterson.solc" caseFolder,
      runTestForFile "proxy.solc" caseFolder,
      runTestExpectingFailure "proxy1.solc" caseFolder,
      runTestForFile "rec.solc" caseFolder,
      runTestExpectingFailure "Ref.solc" caseFolder,
      runTestForFile "RefDeref.solc" caseFolder,
      runTestExpectingFailure "reference.solc" caseFolder,
      runTestForFile "reference-encoding-good.solc" caseFolder,
      runTestForFile "reference-encoding-good1.solc" caseFolder,
      runTestExpectingFailure "reference-encoding.solc" caseFolder,
      runTestExpectingFailure "reference-test.solc" caseFolder,
      runTestExpectingFailure "references-daniel.solc" caseFolder,
      runTestForFile "simpleid.solc" caseFolder,
      runTestForFile "SimpleLambda.solc" caseFolder,
      runTestForFile "single-lambda.solc" caseFolder,
      runTestExpectingFailure "duplicated-type-name.solc" caseFolder,
      runTestExpectingFailure "overlapping-heads.solc" caseFolder,
      runTestExpectingFailure "instance-wrong-sig.solc" caseFolder,
      runTestForFile "match-yul.solc" caseFolder,
      runTestForFile "yul-for.solc" caseFolder,
      runTestForFile "SingleFun.solc" caseFolder,
      runTestForFile "synonym-basic.solc" caseFolder,
      runTestForFile "synonym-param.solc" caseFolder,
      runTestForFile "synonym-nested.solc" caseFolder,
      runTestForFile "synonym-in-function.solc" caseFolder,
      runTestExpectingFailure "synonym-recursive.solc" caseFolder,
      runTestExpectingFailure "synonym-self-recursive.solc" caseFolder,
      runTestExpectingFailure "synonym-long-cycle.solc" caseFolder,
      runTestExpectingFailure "signature.solc" caseFolder,
      runTestExpectingFailure "SillyReturn.solc" caseFolder,
      runTestExpectingFailure "SimpleInvoke.solc" caseFolder,
      runTestExpectingFailure "string-const.solc" caseFolder,
      runTestExpectingFailure "StructMembers.solc" caseFolder,
      runTestExpectingFailure "subject-index.solc" caseFolder,
      runTestExpectingFailure "subject-reduction.solc" caseFolder,
      runTestExpectingFailure "subsumption-test.solc" caseFolder,
      runTestForFile "super-class.solc" caseFolder,
      runTestForFile "super-class-num.solc" caseFolder,
      runTestForFile "tiamat.solc" caseFolder,
      runTestForFile "tuple-trick.solc" caseFolder,
      runTestForFile "tuva.solc" caseFolder,
      runTestForFile "tyexp.solc" caseFolder,
      runTestForFile "typedef.solc" caseFolder,
      runTestForFile "Uncurry.solc" caseFolder,
      runTestExpectingFailure "unconstrained-instance.solc" caseFolder,
      runTestForFile "undefined.solc" caseFolder,
      runTestForFile "uintdesugared.solc" caseFolder,
      runTestForFile "unit.solc" caseFolder,
      runTestExpectingFailure "vartyped.solc" caseFolder,
      runTestExpectingFailure "weirdfoo.solc" caseFolder,
      runTestExpectingFailure "withdraw.solc" caseFolder,
      runTestForFile "word-match.solc" caseFolder,
      runTestExpectingFailure "xref.solc" caseFolder,
      runTestForFile "yul-function-typing.solc" caseFolder,
      runTestForFile "yul-return.solc" caseFolder,
      runTestExpectingFailure "pragma_merge_fail_patterson.solc" caseFolder,
      runTestExpectingFailure "pragma_merge_fail_coverage.solc" caseFolder,
      runTestForFile "single-lambda.solc" caseFolder,
      runTestExpectingFailure "duplicated-type-name.solc" caseFolder,
      runTestExpectingFailure "overlapping-heads.solc" caseFolder,
      runTestExpectingFailure "instance-wrong-sig.solc" caseFolder,
      runTestForFile "match-yul.solc" caseFolder,
      runTestForFile "yul-for.solc" caseFolder,
      runTestForFile "yul-function-typing.solc" caseFolder,
      runTestExpectingFailure "unbound-instance-var.solc" caseFolder,
      runTestExpectingFailure "subsumption-constraint.solc" caseFolder,
      runTestForFile "closure-free-var.solc" caseFolder,
      runTestForFile "closure-free-var-std.solc" caseFolder,
      runTestForFile "closure-free-var-local.solc" caseFolder,
      runTestForFile "closure-free-bound-test.solc" caseFolder,
      runTestExpectingFailure "instance-context-wrong-kind.solc" caseFolder,
      runTestForFile "instance-closure-error.solc" caseFolder,
      runTestExpectingFailure "instance-closure-error-invalid-member.solc" caseFolder,
      runTestForFile "field-name-error.solc" caseFolder,
      runTestForFile "field-helper-cxt-collision.solc" caseFolder,
      runTestExpectingFailure "field-access.solc" caseFolder,
      runTestForFile "mod-example.solc" caseFolder,
      runTestForFile "proxy-desugar.solc" caseFolder
    ]
  where
    caseFolder = "./test/examples/cases"

-- basic infrastructure for tests

type FileName = String

type BaseFolder = String

runTestForFile :: FileName -> BaseFolder -> TestTree
runTestForFile file folder = runTestForFileWith option file folder
  where
    option = stdOpt {optNoGenDispatch = True}

runTestForFileWith :: Option -> FileName -> BaseFolder -> TestTree
runTestForFileWith opts file folder =
  testCase file $ do
    let filePath = folder </> file
    result <- compile (opts {fileName = filePath})
    case result of
      Left err -> assertFailure err
      Right _ -> return ()

runTestExpectingFailure :: FileName -> BaseFolder -> TestTree
runTestExpectingFailure file folder = runTestExpectingFailureWith option file folder
  where
    option = stdOpt {optNoGenDispatch = True}

runTestExpectingFailureWith :: Option -> FileName -> BaseFolder -> TestTree
runTestExpectingFailureWith opts file folder =
  testCase file $ do
    let filePath = folder </> file
    result <- compile opts {fileName = filePath}
    case result of
      Left _ -> return () -- Expected failure
      Right _ -> assertFailure "Expected compilation to fail, but it succeeded"
