module Cases where

import Control.Exception (try)
import Data.List (isInfixOf)
import Language.Hull qualified as Hull
import Solcore.Pipeline.Options
import Solcore.Pipeline.SolcorePipeline
import System.Exit (ExitCode (..))
import System.FilePath
import Test.Tasty
import Test.Tasty.HUnit

stdFolder :: FilePath
stdFolder = "./std"

std :: TestTree
std =
  testGroup
    "Standard library"
    [ runTestForFile "std.sol" stdFolder,
      runTestForFile "dispatch.sol" stdFolder
    ]

comptime :: TestTree
comptime =
  testGroup
    "Compile-time evaluation"
    [ runTestForFile "CondExpr.sol" comptimeFolder,
      runTestForFile "CondStmt.sol" comptimeFolder,
      runTestForFile "Plus.sol" comptimeFolder,
      runTestForFile "OneTwo.sol" comptimeFolder,
      runTestForFile "Size.sol" comptimeFolder,
      runTestForFile "StdSize.sol" comptimeFolder,
      runTestForFile "counter.sol" comptimeFolder,
      runTestForFile "fib.sol" comptimeFolder,
      runTestForFile "string-lit-ops.sol" comptimeFolder,
      runTestForFile "string-lit-len.sol" comptimeFolder,
      runTestForFile "string-lit-keccak.sol" comptimeFolder,
      runTestForFile "comptime_syntax.sol" comptimeFolder,
      -- comptime verification: positive cases (must compile)
      runTestForFile "ct_param_ok.sol" comptimeFolder,
      runTestForFile "ct_chain_ok.sol" comptimeFolder,
      runTestForFile "ct_let_ok.sol" comptimeFolder,
      runTestForFile "ct_overloaded_ok.sol" comptimeFolder,
      runTestForFile "fib.sol" comptimeFolder,
      runTestForFile "fib2.sol" comptimeFolder,
      runTestForFile "fib3.sol" comptimeFolder,
      runTestForFile "ct_asm_mem.sol" comptimeFolder,
      runTestForFile "integer-basic.sol" comptimeFolder,
      runTestForFile "integer-fib.sol" comptimeFolder,
      runTestForFile "integer-from-integer.sol" comptimeFolder,
      runTestForFile "integer-lit.sol" comptimeFolder,
      runTestForFile "integer-lit-safe.sol" comptimeFolder,
      runTestForFile "integer-lit-class.sol" comptimeFolder,
      -- integer literal coercion: unit tests for each case
      runTestForFile "integer-lit-word-site.sol" comptimeFolder,
      runTestForFile "integer-lit-poly.sol" comptimeFolder,
      runTestForFile "integer-lit-cond.sol" comptimeFolder,
      runTestForFile "integer-lit-pat.sol" comptimeFolder,
      runTestForFile "uint256-lit.sol" comptimeFolder,
      runTestForFile "match_labels.sol" comptimeFolder,
      -- comptime string materialization into memory(string)
      runTestForFile "string-lit-mem.sol" comptimeFolder,
      runTestForFile "string-concat-mem.sol" comptimeFolder,
      runTestForFile "string-lit-dedup.sol" comptimeFolder,
      runTestForFile "string-user-instance.sol" comptimeFolder,
      runTestForFile "string-param-erasure.sol" comptimeFolder,
      -- comptime verification: negative cases (must be rejected)
      runTestExpectingFailure "ct_param_runtime.sol" comptimeFolder,
      runTestExpectingFailure "ct_param_poly_runtime.sol" comptimeFolder,
      runTestExpectingFailure "ct_runtime_arg.sol" comptimeFolder,
      runTestExpectingFailure "ct_let_runtime.sol" comptimeFolder,
      runTestExpectingFailure "ct_asm_ret.sol" comptimeFolder,
      runTestExpectingFailure "ct_overloaded_bad.sol" comptimeFolder,
      runTestExpectingFailure "string-mem-runtime-fail.sol" comptimeFolder
    ]
  where
    comptimeFolder = "./test/examples/comptime"

spec :: TestTree
spec =
  testGroup
    "Files for spec cases"
    [ runTestForFile "00answer.sol" specFolder,
      runTestForFile "01id.sol" specFolder,
      runTestForFile "02nid.sol" specFolder,
      runTestForFile "021not.sol" specFolder,
      runTestForFile "022add.sol" specFolder,
      runTestForFile "024arith.sol" specFolder,
      runTestForFile "031maybe.sol" specFolder,
      runTestForFile "032simplejoin.sol" specFolder,
      runTestForFile "033join.sol" specFolder,
      runTestForFile "034cojoin.sol" specFolder,
      runTestForFile "035padding.sol" specFolder,
      runTestForFile "036wildcard.sol" specFolder,
      runTestForFile "037dwarves.sol" specFolder,
      runTestForFile "038food0.sol" specFolder,
      runTestForFile "039food.sol" specFolder,
      runTestForFile "041pair.sol" specFolder,
      runTestForFile "042triple.sol" specFolder,
      runTestForFile "043fstsnd.sol" specFolder,
      runTestForFile "047rgb.sol" specFolder,
      runTestForFile "048rgb2.sol" specFolder,
      runTestForFile "049rgb3.sol" specFolder,
      runTestForFile "06comp.sol" specFolder,
      runTestForFile "09not.sol" specFolder,
      runTestForFile "10negBool.sol" specFolder,
      runTestForFile "11negPair.sol" specFolder,
      runTestForFile "903badassign.sol" specFolder,
      runTestForFile "939badfood.sol" specFolder,
      runTestForFile "SimpleField.sol" specFolder,
      runTestForFile "121counter.sol" specFolder,
      runTestForFile "126nanoerc20.sol" specFolder,
      runTestForFile "127microerc20.sol" specFolder,
      runTestForFile "128minierc20.sol" specFolder,
      runTestForFile "129arraystorage.sol" specFolder,
      runTestForFile "130arrayfield.sol" specFolder,
      runTestForFile "131localindex.sol" specFolder,
      runTestForFile "132nestedarray.sol" specFolder,
      runTestForFile "133arraystring.sol" specFolder,
      runTestForFile "135aliaspush.sol" specFolder
    ]
  where
    specFolder = "./test/examples/spec"

dispatches :: TestTree
dispatches =
  testGroup
    "Files for dispatch cases"
    [ runDispatchTest "basic.sol",
      runDispatchTest "assembly.sol",
      runDispatchTest "stringid.sol",
      runDispatchTest "storage.sol",
      runDispatchTest "miniERC20.sol",
      runDispatchTest "Revert.sol",
      runDispatchTest "hashes.sol",
      runDispatchTest "empty.sol",
      runDispatchTest "empty_no_constructor.sol",
      runDispatchTest "generic_product.sol",
      runDispatchTest "generic_sum.sol",
      runDispatchTest "specialise_sum_of_product.sol",
      runDispatchTest "storage_adt_field.sol",
      runDispatchTest "storage_adt_enum.sol",
      runDispatchTest "storage_adt_bool.sol",
      runDispatchTest "storage_adt_mapping.sol",
      runDispatchTest "storage_adt_abi.sol",
      runDispatchTest "storage_dynamic_field.sol",
      runDispatchTest "storage_array.sol",
      runDispatchTest "ufcs_array.sol",
      runDispatchTest "array_ops.sol",
      runDispatchTest "array_copy.sol",
      runDispatchTest "array_string.sol",
      runDispatchTest "array_nested.sol"
    ]
  where
    runDispatchTest file = runTestForFileWith (emptyOption mempty) file "./test/examples/dispatch"

imports :: TestTree
imports =
  testGroup
    "Files for imports cases"
    [ runImportSuccess "booldef.sol",
      runImportSuccess "boolmain.sol",
      runImportSuccess "unordered_imports_main.sol",
      runImportSuccess "boolalias.sol",
      runImportFailure "alias_hides_original_fail.sol",
      runImportFailure "boolalias_open_fail.sol",
      runImportSuccess "boolqualified.sol",
      runImportSuccess "boolqualifiedtype.sol",
      runImportSuccess "boolaliastype.sol",
      runImportFailure "module_unqualified_fun_fail.sol",
      runImportFailure "alias_unqualified_fun_fail.sol",
      runImportFailure "module_unqualified_type_fail.sol",
      runImportFailure "alias_unqualified_type_fail.sol",
      runImportFailure "module_unqualified_constr_fail.sol",
      runImportFailure "alias_unqualified_constr_fail.sol",
      runImportSuccess "selective_unqualified_fun_ok.sol",
      runImportSuccess "transitive_dep_main_module.sol",
      runImportSuccess "transitive_dep_main_select.sol",
      runImportSuccess "opaque_alias_main.sol",
      runImportSuccess "opaque_select_alias_main.sol",
      runImportFailure "opaque_alias_leak_fail.sol",
      runImportFailure "opaque_alias_qualifier_leak_fail.sol",
      runImportFailure "opaque_select_direct_leak_fail.sol",
      runImportFailure "module_name_shadow.sol",
      runImportSuccess "wrapper_shadow_success.sol",
      runImportSuccess "ns_cross_ok.sol",
      runImportSuccess "ns_constr_dup.sol",
      runImportFailure "strict_open_fail.sol",
      runImportSuccess "boolselect.sol",
      runImportSuccess "boolconselect_ok.sol",
      runImportFailure "boolconselect_fail.sol",
      runImportSuccess "nested_alias.sol",
      runImportSuccess "nested_select.sol",
      runImportSuccess "nested_foo_and_bar.sol",
      runImportSuccess "nested_direct_qualifier.sol",
      runImportSuccess "nested_deep_qualifier.sol",
      runImportSuccess "glob_import_ok.sol",
      runImportSuccess "glob_import_mixed.sol",
      runImportSuccess "glob_import_hiding.sol",
      runImportSuccess "glob_hiding_amb_ok.sol",
      runImportSuccess "glob_import_dup.sol",
      runImportSuccess "glob_export_mixed.sol",
      runImportFailure "glob_amb_main_fail.sol",
      runImportFailure "glob_import_hiding_unknown_fail.sol",
      runImportSuccess "select_hiding_ok.sol",
      runImportFailure "select_hiding_fail.sol",
      runImportFailure "export_item_dup_fail.sol",
      runImportFailure "export_module_dup_fail.sol",
      runImportSuccess "select_ok.sol",
      runImportFailure "select_shadow_local.sol",
      runImportSuccess "select_shadow_param_ok.sol",
      runImportFailure "select_fail.sol",
      runImportFailure "select_unknown.sol",
      runImportFailure "select_dup_item.sol",
      runImportFailure "alias_dup.sol",
      runImportFailure "amb_main.sol",
      runImportSuccess "amb_ok.sol",
      runImportSuccess "dupqual_main.sol",
      runImportSuccess "dupqual_module_main.sol",
      runImportSuccess "private_helper_main.sol",
      runImportSuccess "module_qualified_constructor.sol",
      runImportSuccess "module_qualified_constructor_pattern.sol",
      runImportSuccess "module_qualified_constructor_alias.sol",
      runImportSuccess "type_collision_main.sol",
      runImportSuccess "dot_context_expr.sol",
      runImportSuccess "reexport_items_main.sol",
      runImportSuccess "reexport_select_main.sol",
      runImportSuccess "reexport_select_alias_main.sol",
      runImportSuccess "reexport_module_main.sol",
      runImportSuccess "reexport_module_alias_main.sol",
      runImportSuccess "reexport_ctor_pattern.sol",
      runImportSuccess "reexport_ctor_expr_ok.sol",
      runImportFailure "reexport_ctor_expr_hidden_fail.sol",
      runImportFailure "reexport_ctor_hidden_fail.sol",
      runImportFailure "hidden_ctor_expr_fail.sol",
      runImportFailure "hidden_ctor_dot_fail.sol",
      runImportFailure "hidden_ctor_pattern_fail.sol",
      runImportFailure "hidden_ctor_nonexhaustive_fail.sol",
      runImportSuccess "hidden_ctor_wildcard_ok.sol",
      runImportSuccess "rootcheck/nested/main.sol",
      runImportSuccess "rootcheck/nested/relative_and_lib_main.sol",
      runImportSuccess "external_lib_main.sol",
      runImportSuccess "external_lib_alias_main.sol",
      runImportSuccess "import_std_minimal.sol",
      runImportSuccess "select_alias_item_ok.sol",
      runImportSuccess "select_alias_multi_ok.sol",
      runImportFailure "select_alias_tail_fail.sol",
      runImportFailure "external_lib_missing_fail.sol",
      runImportFailure "symlink_identity_fail.sol",
      runImportFailure "private_bad_main.sol",
      runImportFailure "pragma_scope_main.sol",
      runImportSuccess "selfcycle.sol",
      runImportSuccess "cycle_main.sol",
      runImportSuccess "wild_main.sol",
      runImportFailure "leak_main.sol"
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
    [ runTestExpectingFailure "bound.sol" pragmaFolder,
      runTestForFile "coverage.sol" pragmaFolder,
      runTestForFile "patterson.sol" pragmaFolder
    ]
  where
    pragmaFolder = "./test/examples/pragmas"

opcodes :: TestTree
opcodes =
  testGroup
    "Files for opcodes wrappers"
    [ runTestForFile "all-shapes.sol" opcodesFolder
    ]
  where
    opcodesFolder = "./test/examples/opcodes"

cases :: TestTree
cases =
  testGroup
    "Files for folder cases"
    [ runTestForFile "abigeneric.sol" caseFolder,
      runTestForFile "Ackermann.sol" caseFolder,
      runTestForFile "Add1.sol" caseFolder,
      runTestExpectingFailure "add-moritz.sol" caseFolder,
      runTestForFile "another-subst.sol" caseFolder,
      runTestForFile "app.sol" caseFolder,
      runTestForFile "array.sol" caseFolder,
      runTestForFile "assembly.sol" caseFolder,
      runTestExpectingFailure "asm-assign-no-return.sol" caseFolder,
      runTestExpectingFailure "asm-assign-non-word.sol" caseFolder,
      runTestExpectingFailure "asm-let-no-return.sol" caseFolder,
      runTestForFile "asm-let-uninit.sol" caseFolder,
      runTestForFile "asm-let-bool-lit.sol" caseFolder,
      runTestForFile "asm-match-tuple-read.sol" caseFolder,
      runTestForFile "asm-match-tuple-write-read.sol" caseFolder,
      runTestForFile "bal.sol" caseFolder,
      runTestExpectingFailure "BadInstance.sol" caseFolder,
      runTestForFile "BoolNot.sol" caseFolder,
      runTestExpectingFailure "bound-minimal.sol" caseFolder,
      runTestExpectingFailure "bound-only-test.sol" caseFolder,
      runTestForFile "bound-merge-case.sol" caseFolder,
      runTestForFile "bound-with-pragma.sol" caseFolder,
      runTestExpectingFailure "class-type-name-collision.sol" caseFolder,
      runTestForFile "class-context.sol" caseFolder,
      runTestForFile "closure.sol" caseFolder,
      runTestForFile "closure-capture-only.sol" caseFolder,
      runTestForFile "Compose.sol" caseFolder,
      runTestForFile "Compose3.sol" caseFolder,
      -- The following test makes the test runner throw an exception
      -- , runTestForFile "comp.sol" caseFolder
      runTestForFile "compose0.sol" caseFolder,
      -- compose_desugared.sol exercises the desugared closure/invoke encoding.
      -- The generated `invoke` instance requires rank-N polymorphism that the
      -- type checker cannot infer, so the full pipeline fails typechecking with
      -- SC0209 ("type is not polymorphic enough"). It previously passed only by
      -- disabling the desugaring phases via noDesugarOpt; now that that helper is
      -- gone it is expected to fail.
      runTestExpectingFailure "compose_desugared.sol" caseFolder,
      runTestForFile "compound-assignment-single-eval.sol" caseFolder,
      runTestForFile "comparisons.sol" caseFolder,
      runTestForFile "bitwise.sol" caseFolder,
      runTestForFile "match-bitwise.sol" caseFolder,
      runTestForFile "modulo.sol" caseFolder,
      runTestForFile "CondExp.sol" caseFolder,
      runTestForFile "constrained-instance.sol" caseFolder,
      runTestForFile "constrained-instance-context.sol" caseFolder,
      runTestForFile "const.sol" caseFolder,
      runTestExpectingFailure "const-array.sol" caseFolder,
      runTestForFile "constructor-weak-args.sol" caseFolder,
      runTestExpectingFailure "complexproxy.sol" caseFolder,
      runTestForFile "cyclical-defs.sol" caseFolder,
      runTestForFile "cyclical-defs-inferred.sol" caseFolder,
      runTestExpectingFailure "default-inst.sol" caseFolder,
      runTestExpectingFailure "default-instance-missing.sol" caseFolder,
      runTestExpectingFailure "default-instance-weak.sol" caseFolder,
      runTestForFile "derive-generic-sum.sol" caseFolder,
      runTestForFile "derive-generic-excluded.sol" caseFolder,
      runTestExpectingFailure "generic-manual-no-pragma.sol" caseFolder,
      runTestExpectingFailure "generic-sum-no-pragma.sol" caseFolder,
      runTestExpectingFailure "generic-product-no-pragma.sol" caseFolder,
      runTestForFile "dot-expression-constructor.sol" caseFolder,
      runTestForFile "dot-expression-call-arg-context.sol" caseFolder,
      runTestForFile "dot-expression-match-return.sol" caseFolder,
      runTestForFile "dot-expression-nested-context.sol" caseFolder,
      runTestForFile "dot-expression-assignment-context.sol" caseFolder,
      runTestExpectingFailure "dot-expression-no-context-fail.sol" caseFolder,
      runTestExpectingFailure "dot-expression-unknown-fail.sol" caseFolder,
      runTestForFile "dot-pattern-constructor.sol" caseFolder,
      runTestForFile "dot-pattern-nested-constructor.sol" caseFolder,
      runTestForFile "dot-primitive-constructor.sol" caseFolder,
      runTestForFile "same-name-constructor-qualifier.sol" caseFolder,
      runTestExpectingFailure "duplicated-contract-name.sol" caseFolder,
      runTestExpectingFailure "duplicated-type-name.sol" caseFolder,
      runTestForFile "DuplicateFun.sol" caseFolder,
      runTestExpectingFailure "DupFun.sol" caseFolder,
      runTestForFile "EitherModule.sol" caseFolder,
      runTestForFile "empty-asm.sol" caseFolder,
      runTestForFile "encoder.sol" caseFolder,
      runTestForFile "encoder1.sol" caseFolder,
      runTestExpectingFailure "Enum.sol" caseFolder,
      runTestExpectingFailure "Eq.sol" caseFolder,
      runTestForFile "EqQual.sol" caseFolder,
      runTestForFile "EvenOdd.sol" caseFolder,
      runTestExpectingFailure "fallback-with-args.sol" caseFolder,
      runTestExpectingFailure "fallback-with-return.sol" caseFolder,
      runTestExpectingFailure "public-fallback.sol" caseFolder,
      runTestExpectingFailure "public-constructor.sol" caseFolder,
      runTestExpectingFailure "public-top-level-function.sol" caseFolder,
      runTestExpectingFailure "toplevel-fallback.sol" caseFolder,
      runTestExpectingFailure "toplevel-constructor.sol" caseFolder,
      runTestExpectingFailure "payable-toplevel-function.sol" caseFolder,
      runTestExpectingFailure "Filter.sol" caseFolder,
      runTestForFile "foo-class.sol" caseFolder,
      runTestForFile "Foo.sol" caseFolder,
      runTestForFile "for-body-shadow.sol" caseFolder,
      runTestForFile "for-break.sol" caseFolder,
      runTestForFile "for-continue.sol" caseFolder,
      runTestForFile "for-empty-init.sol" caseFolder,
      runTestForFile "for-inner-block.sol" caseFolder,
      runTestForFile "for-init-shadow.sol" caseFolder,
      runTestForFile "for-let.sol" caseFolder,
      runTestExpectingFailure "for-let-post.sol" caseFolder,
      runTestForFile "for-loop.sol" caseFolder,
      runTestForFile "for-multi-init.sol" caseFolder,
      runTestForFile "for-multi-post.sol" caseFolder,
      runTestExpectingFailure "GetSet.sol" caseFolder,
      runTestExpectingFailure "GoodInstance.sol" caseFolder,
      runTestForFile "Id.sol" caseFolder,
      runTestForFile "if-examples.sol" caseFolder,
      runTestExpectingFailure "index-example.sol" caseFolder,
      runTestForFile "import-std.sol" caseFolder,
      runTestForFile "inc-closure.sol" caseFolder,
      runTestExpectingFailure "IncompleteInstDef.sol" caseFolder,
      runTestExpectingFailure "instance-wrong-sig.sol" caseFolder,
      runTestExpectingFailure "Invokable.sol" caseFolder,
      runTestForFile "ixa.sol" caseFolder,
      runTestForFile "join.sol" caseFolder,
      runTestExpectingFailure "joinErr.sol" caseFolder,
      runTestExpectingFailure "KindTest.sol" caseFolder,
      runTestExpectingFailure "listeq.sol" caseFolder,
      runTestForFile "ListModule.sol" caseFolder,
      runTestForFile "listid.sol" caseFolder,
      runTestForFile "Logic.sol" caseFolder,
      runTestExpectingFailure "mainproxy.sol" caseFolder,
      runTestForFile "MatchCall.sol" caseFolder,
      runTestExpectingFailure "match-compiler-undef-asm.sol" caseFolder,
      runTestExpectingFailure "phantom-type-return-con.sol" caseFolder,
      runTestForFile "match-yul.sol" caseFolder,
      runTestForFile "memory.sol" caseFolder,
      runTestForFile "Memory1.sol" caseFolder,
      runTestForFile "Memory2.sol" caseFolder,
      runTestExpectingFailure "missing-instance.sol" caseFolder,
      runTestForFile "modifier.sol" caseFolder,
      runTestForFile "mptc-both-templates.sol" caseFolder,
      runTestForFile "mptc-chain-phantom.sol" caseFolder,
      runTestForFile "mptc-guard-extras-concrete.sol" caseFolder,
      runTestForFile "mptc-multi-instance.sol" caseFolder,
      runTestForFile "mptc-nop-mainty-free.sol" caseFolder,
      runTestForFile "mptc-partial-instance.sol" caseFolder,
      runTestForFile "mptc-template-a-only.sol" caseFolder,
      runTestForFile "mptc-template-b-only.sol" caseFolder,
      runTestForFile "monomorphic-require.sol" caseFolder,
      runTestForFile "morefun.sol" caseFolder,
      runTestForFile "Mutuals.sol" caseFolder,
      runTestExpectingFailure "nano-desugared.sol" caseFolder,
      runTestForFile "NegPair.sol" caseFolder,
      runTestForFile "nid.sol" caseFolder,
      runTestForFile "noclosure.sol" caseFolder,
      runTestExpectingFailure "noconstr.sol" caseFolder,
      runTestForFile "notif.sol" caseFolder,
      runTestForFile "Option.sol" caseFolder,
      runTestForFile "option2.sol" caseFolder,
      runTestExpectingFailure "overlapping-heads.sol" caseFolder,
      runTestForFile "Pair.sol" caseFolder,
      runTestExpectingFailure "PairMatch1.sol" caseFolder,
      runTestExpectingFailure "PairMatch2.sol" caseFolder,
      -- failing due to missing assign constraint
      runTestExpectingFailure "patterson-bug.sol" caseFolder,
      runTestForFile "Peano.sol" caseFolder,
      runTestForFile "PeanoMatch.sol" caseFolder,
      runTestForFile "pair-bug.sol" caseFolder,
      runTestForFile "polymatch-error.sol" caseFolder,
      runTestForFile "polymorphic-require.sol" caseFolder,
      runTestExpectingFailure "pragma_merge_fail_coverage.sol" caseFolder,
      runTestExpectingFailure "pragma_merge_fail_patterson.sol" caseFolder,
      runTestForFile "pragma_merge_base.sol" caseFolder,
      runTestExpectingFailure "pragma_merge_import.sol" caseFolder,
      runTestExpectingFailure "pragma_merge_verify.sol" caseFolder,
      runTestForFile "pragma_test_patterson.sol" caseFolder,
      runTestForFile "proxy.sol" caseFolder,
      runTestExpectingFailure "proxy1.sol" caseFolder,
      runTestForFile "rec.sol" caseFolder,
      runTestExpectingFailure "require-annotation-missing-param.sol" caseFolder,
      runTestExpectingFailure "require-annotation-missing-return.sol" caseFolder,
      runTestExpectingFailure "require-annotation-missing-both.sol" caseFolder,
      runTestExpectingFailure "require-annotation-contract-method.sol" caseFolder,
      runTestExpectingFailure "require-annotation-mutual.sol" caseFolder,
      runTestExpectingFailure "Ref.sol" caseFolder,
      runTestForFile "RefDeref.sol" caseFolder,
      runTestExpectingFailure "reference.sol" caseFolder,
      runTestForFile "reference-encoding-good.sol" caseFolder,
      runTestForFile "reference-encoding-good1.sol" caseFolder,
      runTestExpectingFailure "reference-encoding.sol" caseFolder,
      runTestExpectingFailure "reference-test.sol" caseFolder,
      runTestExpectingFailure "references-daniel.sol" caseFolder,
      runTestExpectingFailure "skolem-let.sol" caseFolder,
      runTestForFile "simpleid.sol" caseFolder,
      runTestForFile "SimpleLambda.sol" caseFolder,
      runTestForFile "single-lambda.sol" caseFolder,
      runTestExpectingFailure "duplicated-type-name.sol" caseFolder,
      runTestExpectingFailure "overlapping-heads.sol" caseFolder,
      runTestExpectingFailure "instance-wrong-sig.sol" caseFolder,
      runTestForFile "match-yul.sol" caseFolder,
      runTestForFile "yul-for.sol" caseFolder,
      runTestForFile "SingleFun.sol" caseFolder,
      runTestForFile "synonym-basic.sol" caseFolder,
      runTestForFile "synonym-param.sol" caseFolder,
      runTestForFile "synonym-nested.sol" caseFolder,
      runTestForFile "synonym-in-function.sol" caseFolder,
      runTestExpectingFailure "synonym-recursive.sol" caseFolder,
      runTestExpectingFailure "synonym-self-recursive.sol" caseFolder,
      runTestExpectingFailure "synonym-long-cycle.sol" caseFolder,
      runTestExpectingFailure "synonym-arity-mismatch.sol" caseFolder,
      runTestExpectingFailure "signature.sol" caseFolder,
      runTestExpectingFailure "spec-fail-ungrounded.sol" caseFolder,
      runTestExpectingFailure "SillyReturn.sol" caseFolder,
      runTestExpectingFailure "SimpleInvoke.sol" caseFolder,
      runTestExpectingFailure "string-const.sol" caseFolder,
      runTestExpectingFailure "StructMembers.sol" caseFolder,
      runTestExpectingFailure "subject-index.sol" caseFolder,
      runTestExpectingFailure "subject-reduction.sol" caseFolder,
      runTestExpectingFailure "subsumption-test.sol" caseFolder,
      runTestForFile "super-class.sol" caseFolder,
      runTestForFile "super-class-cycle.sol" caseFolder,
      runTestExpectingFailure "super-class-cycle-fail.sol" caseFolder,
      runTestForFile "super-class-num.sol" caseFolder,
      runTestForFile "tiamat.sol" caseFolder,
      runTestForFile "tuple-trick.sol" caseFolder,
      runTestForFile "tuva.sol" caseFolder,
      runTestForFile "tyexp.sol" caseFolder,
      runAsConversionTest,
      runAsConversionFailureTest
        "as-conversion-fail.sol"
        "no explicit conversion from bool to uint256",
      runAsConversionFailureTest
        "as-conversion-ambiguous-fail.sol"
        "ambiguous explicit conversion from Left to Right",
      runAsConversionFailureTest
        "as-conversion-identity-instance-fail.sol"
        "no explicit conversion from word to Wrapped",
      runTestForFile "typedef.sol" caseFolder,
      runTestForFile "Uncurry.sol" caseFolder,
      runTestExpectingFailure "unconstrained-instance.sol" caseFolder,
      runTestForFile "undefined.sol" caseFolder,
      runTestForFile "uintdesugared.sol" caseFolder,
      runTestForFile "ufcs-no-conflict.sol" caseFolder,
      runTestForFile "unit.sol" caseFolder,
      runTestExpectingFailure "vartyped.sol" caseFolder,
      runTestExpectingFailure "weirdfoo.sol" caseFolder,
      runTestForFile "word-match-default.sol" caseFolder,
      runTestForFile "sum-match-default.sol" caseFolder,
      runTestForFile "word-match.sol" caseFolder,
      runTestExpectingFailure "xref.sol" caseFolder,
      runTestForFile "yul-function-typing.sol" caseFolder,
      runTestForFile "yul-return.sol" caseFolder,
      runTestForFile "yul-multi-return.sol" caseFolder,
      runTestExpectingFailure "yul-multi-return-arity-fail.sol" caseFolder,
      runTestExpectingFailure "pragma_merge_fail_patterson.sol" caseFolder,
      runTestExpectingFailure "pragma_merge_fail_coverage.sol" caseFolder,
      runTestForFile "single-lambda.sol" caseFolder,
      runTestExpectingFailure "duplicated-type-name.sol" caseFolder,
      runTestExpectingFailure "overlapping-heads.sol" caseFolder,
      runTestExpectingFailure "instance-wrong-sig.sol" caseFolder,
      runTestForFile "match-yul.sol" caseFolder,
      runTestForFile "yul-for.sol" caseFolder,
      runTestForFile "yul-function-typing.sol" caseFolder,
      runTestExpectingFailure "unbound-instance-var.sol" caseFolder,
      runTestExpectingFailure "subsumption-constraint.sol" caseFolder,
      runTestForFile "closure-free-var.sol" caseFolder,
      runTestForFile "closure-free-var-std.sol" caseFolder,
      runTestForFile "closure-free-var-local.sol" caseFolder,
      runTestForFile "closure-free-bound-test.sol" caseFolder,
      runTestExpectingFailure "instance-context-wrong-kind.sol" caseFolder,
      runTestForFile "instance-closure-error.sol" caseFolder,
      runTestExpectingFailure "instance-closure-error-invalid-member.sol" caseFolder,
      -- functions returning functions: validate that the single type-inference
      -- pass still catches lambda type errors that closure conversion could mask.
      runTestForFile "return-fun-adder.sol" caseFolder,
      runTestForFile "return-fun-const.sol" caseFolder,
      runTestForFile "return-fun-instance.sol" caseFolder,
      runTestForFile "return-fun-eq.sol" caseFolder,
      runTestExpectingFailure "return-fun-bad-param.sol" caseFolder,
      runTestExpectingFailure "return-fun-bad-return.sol" caseFolder,
      runTestExpectingFailure "return-fun-bad-arity.sol" caseFolder,
      runTestExpectingFailure "return-fun-bad-sig.sol" caseFolder,
      runTestExpectingFailure "return-fun-not-fun.sol" caseFolder,
      runTestForFile "field-name-error.sol" caseFolder,
      runTestForFile "field-helper-cxt-collision.sol" caseFolder,
      runTestExpectingFailure "field-access.sol" caseFolder,
      runTestForFile "mod-example.sol" caseFolder,
      runTestForFile "snds.sol" caseFolder,
      runTestForFile "bool-elim.sol" caseFolder,
      runTestForFile "catch-all.sol" caseFolder,
      runTestForFile "redundant-match.sol" caseFolder,
      runTestForFile "false-redundant-warning.sol" caseFolder,
      runTestForFile "proxy-desugar.sol" caseFolder,
      runTestForFile "invokable-issue.sol" caseFolder,
      runTestForFile "td.sol" caseFolder,
      runTestForFile "bar.sol" caseFolder,
      runTestForFile "fresh-pat-arg.sol" caseFolder,
      runTestForFile "fresh-pat-arg-synonym.sol" caseFolder,
      runTestExpectingFailure "weird-error-foo.sol" caseFolder,
      runTestForFile "strange-unbound.sol" caseFolder,
      runTestForFile "type-synonym-arg.sol" caseFolder,
      runTestForFile "instance-synonym.sol" caseFolder,
      runTestForFile "instance-synonym-int.sol" caseFolder,
      runTestExpectingFailure "overlap-synonym-detected.sol" caseFolder,
      runTestExpectingFailure "overlap-synonym-missed-order.sol" caseFolder,
      runTestExpectingFailure "overlap-synonym-missed-two-synonyms.sol" caseFolder,
      runTestForFile "copytomem.sol" caseFolder,
      runTestForFile "fresh-variable-shadowing.sol" caseFolder,
      runTestForFile "simpleDiscount.sol" caseFolder,
      runTestForFile "yul-deposit-example.sol" caseFolder,
      runTestForFile "yul-asm-for-body.sol" caseFolder,
      runTestForFile
        "yul-asm-switch-body.sol"
        caseFolder,
      runTestForFile
        "multi-stmt-var-leaf.sol"
        caseFolder,
      runTestForFile "ltimp.sol" caseFolder,
      runTestForFile "class-return-type-miss.sol" caseFolder,
      runTestExpectingFailure "catenable-err.sol" caseFolder,
      runTestForFile "pars.sol" caseFolder,
      runTestForFile "bug-rep-name-capture.sol" caseFolder,
      runTestForFile "bug-import-default-inst-shadow.sol" caseFolder,
      runTestForFile "bug-call-expected-nontail-return.sol" caseFolder,
      runTestExpectingFailure "array-elem-no-storagecopy.sol" caseFolder,
      runTestExpectingFailure "array-push-no-canstore.sol" caseFolder,
      -- Storage derivation for ADTs. These need dispatch generation: without a
      -- generated `main` the contract constructor is dead code, so the field's
      -- CanStore obligation is never forced and the negative cases would pass.
      runTestForFileWith dispatchOpt "storage-adt-recursive-ok.sol" caseFolder,
      runTestExpectingFailureWith dispatchOpt "storage-adt-recursive-fail.sol" caseFolder,
      runTestExpectingFailureWith dispatchOpt "storage-adt-mapping-field-fail.sol" caseFolder
    ]
  where
    caseFolder = "./test/examples/cases"
    dispatchOpt = emptyOption mempty

runAsConversionTest :: TestTree
runAsConversionTest =
  testCase "as-conversion.sol" $ do
    let folder = "./test/examples/cases"
        filePath = folder </> "as-conversion.sol"
        opts =
          stdOpt
            { fileName = filePath,
              optRootDir = folder,
              optNoGenDispatch = True
            }
    result <- compile opts
    case result of
      Left err -> assertFailure err
      Right [object] -> do
        assertReturnsCall object (== "wrap") "concrete abstraction conversion"
        assertReturnsCall object (== "unwrap") "concrete representation conversion"
        assertReturnsCall object ("genericWrap" `isInfixOf`) "generic abstraction conversion"
        assertReturnsCall object ("genericUnwrap" `isInfixOf`) "generic representation conversion"
        assertReturnsVariable object (== "identity") "same-type identity conversion"
      Right objects ->
        assertFailure ("expected one Hull object, got " ++ show (length objects))
  where
    assertReturnsCall object matches label =
      assertBool
        (label ++ " was erased instead of calling its Typedef method")
        (any (functionReturnsCall matches) (Hull.objCode object))

    assertReturnsVariable object matches label =
      assertBool
        (label ++ " unexpectedly introduced a conversion call")
        (any (functionReturnsVariable matches) (Hull.objCode object))

    functionReturnsCall matches (Hull.SFunction name _ _ body) =
      matches name && any isCallReturn body
    functionReturnsCall _ _ = False

    functionReturnsVariable matches (Hull.SFunction name _ _ body) =
      matches name && any isVariableReturn body
    functionReturnsVariable _ _ = False

    isCallReturn (Hull.SReturn (Hull.ECall _ [_])) = True
    isCallReturn _ = False

    isVariableReturn (Hull.SReturn (Hull.EVar _)) = True
    isVariableReturn _ = False

runAsConversionFailureTest :: FilePath -> String -> TestTree
runAsConversionFailureTest file expectedMessage =
  testCase file $ do
    let folder = "./test/examples/cases"
        filePath = folder </> file
        opts =
          stdOpt
            { fileName = filePath,
              optRootDir = folder,
              optNoGenDispatch = True
            }
    result <- compile opts
    case result of
      Left err ->
        assertBool
          ("expected explicit-conversion diagnostic SC0230, got:\n" ++ err)
          ( "error[SC0230]" `isInfixOf` err
              && expectedMessage `isInfixOf` err
          )
      Right _ ->
        assertFailure (file ++ " unexpectedly compiled")

tabledResolution :: TestTree
tabledResolution =
  testGroup
    "Tabled type class resolution"
    [ testGroup
        "Cases"
        [ runTabledTestForFile "constrained-instance.sol" caseFolder,
          runTabledTestForFile "constrained-instance-context.sol" caseFolder,
          runTabledTestForFile "constructor-weak-args.sol" caseFolder,
          runTabledTestForFile "EqQual.sol" caseFolder,
          runTabledTestForFile "super-class.sol" caseFolder,
          runTabledTestForFile "super-class-num.sol" caseFolder,
          runTabledTestForFile "field-helper-cxt-collision.sol" caseFolder,
          runTabledTestForFile "instance-synonym.sol" caseFolder,
          runTabledTestForFile "invokable-issue.sol" caseFolder,
          runTabledTestForFile "td.sol" caseFolder,
          runTabledTestForFile "tuple-trick.sol" caseFolder,
          runTabledTestForFile "typedef.sol" caseFolder,
          runTabledTestForFile "word-match.sol" caseFolder,
          runTabledTestExpectingFailure "tabled-cycle-fail.sol" caseFolder
        ],
      testGroup
        "Conformance"
        [ runTabledTestForFile "tabled-answer-reuse.sol" caseFolder,
          runTabledTestForFile "tabled-default-instance.sol" caseFolder,
          runTabledTestForFile "tabled-given-order.sol" caseFolder,
          runTabledTestForFile "tabled-mutual-chain.sol" caseFolder,
          runTabledTestForFile "tabled-residual-given.sol" caseFolder,
          runTabledTestForFile "super-class-cycle.sol" caseFolder,
          runTabledTestExpectingFailure "super-class-cycle-fail.sol" caseFolder,
          runTabledTestForFile "super-class-recursive-arg.sol" caseFolder,
          runTabledTestExpectingFailure "tabled-left-recursive-fail.sol" caseFolder
        ],
      testGroup
        "Spec"
        [ runTabledTestForFile "024arith.sol" specFolder,
          runTabledTestForFile "031maybe.sol" specFolder,
          runTabledTestForFile "041pair.sol" specFolder,
          runTabledTestForFile "047rgb.sol" specFolder
        ],
      testGroup
        "Standard library"
        [ runTabledTestForFile "std.sol" stdFolder
        ]
    ]
  where
    caseFolder = "./test/examples/cases"
    specFolder = "./test/examples/spec"

-- basic infrastructure for tests

type FileName = String

type BaseFolder = String

runTestForFile :: FileName -> BaseFolder -> TestTree
runTestForFile file folder = runTestForFileWith option file folder
  where
    option = stdOpt {optNoGenDispatch = True}

runTestForFileWith :: Option -> FileName -> BaseFolder -> TestTree
runTestForFileWith opts file folder =
  runNamedTestForFile file opts file folder

runTabledTestForFile :: FileName -> BaseFolder -> TestTree
runTabledTestForFile file folder =
  runNamedTestForFile (file ++ " (tabled)") option file folder
  where
    option =
      stdOpt
        { optNoGenDispatch = True,
          optTypeClassResolution = TabledResolution
        }

runTabledTestExpectingFailure :: FileName -> BaseFolder -> TestTree
runTabledTestExpectingFailure file folder =
  runNamedTestExpectingFailure (file ++ " (tabled)") option file folder
  where
    option =
      stdOpt
        { optNoGenDispatch = True,
          optTypeClassResolution = TabledResolution
        }

runNamedTestForFile :: String -> Option -> FileName -> BaseFolder -> TestTree
runNamedTestForFile testName opts file folder =
  testCase testName $ do
    let filePath = folder </> file
    result <- compile (opts {fileName = filePath, optRootDir = folder})
    case result of
      Left err -> assertFailure err
      Right _ -> return ()

runTestExpectingFailure :: FileName -> BaseFolder -> TestTree
runTestExpectingFailure file folder = runTestExpectingFailureWith option file folder
  where
    option = stdOpt {optNoGenDispatch = True}

runTestExpectingFailureWith :: Option -> FileName -> BaseFolder -> TestTree
runTestExpectingFailureWith opts file folder =
  runNamedTestExpectingFailure file opts file folder

runNamedTestExpectingFailure :: String -> Option -> FileName -> BaseFolder -> TestTree
runNamedTestExpectingFailure testName opts file folder =
  testCase testName $ do
    let filePath = folder </> file
    outcome <- try (compile opts {fileName = filePath, optRootDir = folder})
    case outcome of
      Left (ExitFailure _) -> return () -- Expected failure via exitFailure
      Left ExitSuccess -> assertFailure "Expected compilation to fail, but it exited successfully"
      Right (Left _) -> return () -- Expected failure via Either
      Right (Right _) -> assertFailure "Expected compilation to fail, but it succeeded"
