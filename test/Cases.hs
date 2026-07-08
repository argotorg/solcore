module Cases where

import Control.Exception (try)
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
      runTestForFile "string-lit-keccak.solc" comptimeFolder,
      runTestForFile "comptime_syntax.solc" comptimeFolder,
      -- comptime verification: positive cases (must compile)
      runTestForFile "ct_param_ok.solc" comptimeFolder,
      runTestForFile "ct_chain_ok.solc" comptimeFolder,
      runTestForFile "ct_let_ok.solc" comptimeFolder,
      runTestForFile "ct_overloaded_ok.solc" comptimeFolder,
      runTestForFile "fib.solc" comptimeFolder,
      runTestForFile "fib2.solc" comptimeFolder,
      runTestForFile "fib3.solc" comptimeFolder,
      runTestForFile "ct_asm_mem.solc" comptimeFolder,
      runTestForFile "integer-basic.solc" comptimeFolder,
      runTestForFile "integer-fib.solc" comptimeFolder,
      runTestForFile "integer-from-integer.solc" comptimeFolder,
      runTestForFile "integer-lit.solc" comptimeFolder,
      runTestForFile "integer-lit-safe.solc" comptimeFolder,
      runTestForFile "integer-lit-class.solc" comptimeFolder,
      -- integer literal coercion: unit tests for each case
      runTestForFile "integer-lit-word-site.solc" comptimeFolder,
      runTestForFile "integer-lit-poly.solc" comptimeFolder,
      runTestForFile "integer-lit-cond.solc" comptimeFolder,
      runTestForFile "integer-lit-pat.solc" comptimeFolder,
      runTestForFile "uint256-lit.solc" comptimeFolder,
      runTestForFile "match_labels.solc" comptimeFolder,
      -- comptime verification: negative cases (must be rejected)
      runTestExpectingFailure "ct_param_runtime.solc" comptimeFolder,
      runTestExpectingFailure "ct_param_poly_runtime.solc" comptimeFolder,
      runTestExpectingFailure "ct_runtime_arg.solc" comptimeFolder,
      runTestExpectingFailure "ct_let_runtime.solc" comptimeFolder,
      runTestExpectingFailure "ct_asm_ret.solc" comptimeFolder,
      runTestExpectingFailure "ct_overloaded_bad.solc" comptimeFolder
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
      runTestForFile "049rgb3.solc" specFolder,
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
      runDispatchTest "assembly.solc",
      runDispatchTest "stringid.solc",
      runDispatchTest "storage.solc",
      runDispatchTest "transient.solc",
      runDispatchTest "transient_field.solc",
      runDispatchTest "miniERC20.solc",
      runDispatchTest "Revert.solc",
      runDispatchTest "hashes.solc",
      runDispatchTest "empty.solc",
      runDispatchTest "empty_no_constructor.solc",
      runDispatchTest "generic_product.solc",
      runDispatchTest "generic_sum.solc",
      runDispatchTest "specialise_sum_of_product.solc",
      runDispatchTest "storage_adt_field.solc",
      runDispatchTest "storage_adt_enum.solc",
      runDispatchTest "storage_adt_bool.solc",
      runDispatchTest "storage_adt_mapping.solc",
      runDispatchTest "storage_adt_abi.solc",
      runDispatchTest "storage_dynamic_field.solc"
    ]
  where
    runDispatchTest file = runTestForFileWith (emptyOption mempty) file "./test/examples/dispatch"

imports :: TestTree
imports =
  testGroup
    "Files for imports cases"
    [ runImportSuccess "booldef.solc",
      runImportSuccess "boolmain.solc",
      runImportSuccess "unordered_imports_main.solc",
      runImportSuccess "boolalias.solc",
      runImportFailure "alias_hides_original_fail.solc",
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
      runImportSuccess "opaque_alias_main.solc",
      runImportSuccess "opaque_select_alias_main.solc",
      runImportFailure "opaque_alias_leak_fail.solc",
      runImportFailure "opaque_alias_qualifier_leak_fail.solc",
      runImportFailure "opaque_select_direct_leak_fail.solc",
      runImportFailure "module_name_shadow.solc",
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
      runImportSuccess "glob_import_mixed.solc",
      runImportSuccess "glob_import_hiding.solc",
      runImportSuccess "glob_hiding_amb_ok.solc",
      runImportSuccess "glob_import_dup.solc",
      runImportSuccess "glob_export_mixed.solc",
      runImportFailure "glob_amb_main_fail.solc",
      runImportFailure "glob_import_hiding_unknown_fail.solc",
      runImportSuccess "select_hiding_ok.solc",
      runImportFailure "select_hiding_fail.solc",
      runImportFailure "export_item_dup_fail.solc",
      runImportFailure "export_module_dup_fail.solc",
      runImportSuccess "select_ok.solc",
      runImportFailure "select_shadow_local.solc",
      runImportSuccess "select_shadow_param_ok.solc",
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
      runImportSuccess "module_qualified_constructor_pattern.solc",
      runImportSuccess "module_qualified_constructor_alias.solc",
      runImportSuccess "type_collision_main.solc",
      runImportSuccess "dot_context_expr.solc",
      runImportSuccess "reexport_items_main.solc",
      runImportSuccess "reexport_select_main.solc",
      runImportSuccess "reexport_select_alias_main.solc",
      runImportSuccess "reexport_module_main.solc",
      runImportSuccess "reexport_module_alias_main.solc",
      runImportSuccess "reexport_ctor_pattern.solc",
      runImportSuccess "reexport_ctor_expr_ok.solc",
      runImportFailure "reexport_ctor_expr_hidden_fail.solc",
      runImportFailure "reexport_ctor_hidden_fail.solc",
      runImportFailure "hidden_ctor_expr_fail.solc",
      runImportFailure "hidden_ctor_dot_fail.solc",
      runImportFailure "hidden_ctor_pattern_fail.solc",
      runImportFailure "hidden_ctor_nonexhaustive_fail.solc",
      runImportSuccess "hidden_ctor_wildcard_ok.solc",
      runImportSuccess "rootcheck/nested/main.solc",
      runImportSuccess "rootcheck/nested/relative_and_lib_main.solc",
      runImportSuccess "external_lib_main.solc",
      runImportSuccess "external_lib_alias_main.solc",
      runImportSuccess "import_std_minimal.solc",
      runImportSuccess "select_alias_item_ok.solc",
      runImportSuccess "select_alias_multi_ok.solc",
      runImportFailure "select_alias_tail_fail.solc",
      runImportFailure "external_lib_missing_fail.solc",
      runImportFailure "symlink_identity_fail.solc",
      runImportFailure "private_bad_main.solc",
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

opcodes :: TestTree
opcodes =
  testGroup
    "Files for opcodes wrappers"
    [ runTestForFile "all-shapes.solc" opcodesFolder
    ]
  where
    opcodesFolder = "./test/examples/opcodes"

cases :: TestTree
cases =
  testGroup
    "Files for folder cases"
    [ runTestForFile "abigeneric.solc" caseFolder,
      runTestForFile "Ackermann.solc" caseFolder,
      runTestForFile "Add1.solc" caseFolder,
      runTestExpectingFailure "add-moritz.solc" caseFolder,
      runTestForFile "another-subst.solc" caseFolder,
      runTestForFile "app.solc" caseFolder,
      runTestForFile "array.solc" caseFolder,
      runTestForFile "assembly.solc" caseFolder,
      runTestExpectingFailure "asm-assign-no-return.solc" caseFolder,
      runTestExpectingFailure "asm-assign-non-word.solc" caseFolder,
      runTestExpectingFailure "asm-let-no-return.solc" caseFolder,
      runTestForFile "asm-let-uninit.solc" caseFolder,
      runTestForFile "asm-let-bool-lit.solc" caseFolder,
      runTestForFile "asm-match-tuple-read.solc" caseFolder,
      runTestForFile "asm-match-tuple-write-read.solc" caseFolder,
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
      runTestForFile "Compose.solc" caseFolder,
      runTestForFile "Compose3.solc" caseFolder,
      -- The following test makes the test runner throw an exception
      -- , runTestForFile "comp.solc" caseFolder
      runTestForFile "compose0.solc" caseFolder,
      -- compose_desugared.solc exercises the desugared closure/invoke encoding.
      -- The generated `invoke` instance requires rank-N polymorphism that the
      -- type checker cannot infer, so the full pipeline fails typechecking with
      -- SC0209 ("type is not polymorphic enough"). It previously passed only by
      -- disabling the desugaring phases via noDesugarOpt; now that that helper is
      -- gone it is expected to fail.
      runTestExpectingFailure "compose_desugared.solc" caseFolder,
      runTestForFile "comparisons.solc" caseFolder,
      runTestForFile "bitwise.solc" caseFolder,
      runTestForFile "match-bitwise.solc" caseFolder,
      runTestForFile "modulo.solc" caseFolder,
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
      runTestForFile "derive-generic-sum.solc" caseFolder,
      runTestForFile "derive-generic-excluded.solc" caseFolder,
      runTestExpectingFailure "generic-manual-no-pragma.solc" caseFolder,
      runTestExpectingFailure "generic-sum-no-pragma.solc" caseFolder,
      runTestExpectingFailure "generic-product-no-pragma.solc" caseFolder,
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
      runTestForFile "same-name-constructor-qualifier.solc" caseFolder,
      runTestExpectingFailure "duplicated-contract-name.solc" caseFolder,
      runTestExpectingFailure "duplicated-type-name.solc" caseFolder,
      runTestForFile "DuplicateFun.solc" caseFolder,
      runTestExpectingFailure "DupFun.solc" caseFolder,
      runTestForFile "EitherModule.solc" caseFolder,
      runTestForFile "empty-asm.solc" caseFolder,
      runTestForFile "encoder.solc" caseFolder,
      runTestForFile "encoder1.solc" caseFolder,
      runTestExpectingFailure "Enum.solc" caseFolder,
      runTestExpectingFailure "Eq.solc" caseFolder,
      runTestForFile "EqQual.solc" caseFolder,
      runTestForFile "EvenOdd.solc" caseFolder,
      runTestExpectingFailure "fallback-with-args.solc" caseFolder,
      runTestExpectingFailure "fallback-with-return.solc" caseFolder,
      runTestExpectingFailure "public-fallback.solc" caseFolder,
      runTestExpectingFailure "public-constructor.solc" caseFolder,
      runTestExpectingFailure "public-top-level-function.solc" caseFolder,
      runTestExpectingFailure "toplevel-fallback.solc" caseFolder,
      runTestExpectingFailure "toplevel-constructor.solc" caseFolder,
      runTestExpectingFailure "payable-toplevel-function.solc" caseFolder,
      runTestExpectingFailure "Filter.solc" caseFolder,
      runTestForFile "foo-class.solc" caseFolder,
      runTestForFile "Foo.solc" caseFolder,
      runTestForFile "for-body-shadow.solc" caseFolder,
      runTestForFile "for-break.solc" caseFolder,
      runTestForFile "for-continue.solc" caseFolder,
      runTestForFile "for-empty-init.solc" caseFolder,
      runTestForFile "for-inner-block.solc" caseFolder,
      runTestForFile "for-init-shadow.solc" caseFolder,
      runTestForFile "for-let.solc" caseFolder,
      runTestExpectingFailure "for-let-post.solc" caseFolder,
      runTestForFile "for-loop.solc" caseFolder,
      runTestForFile "for-multi-init.solc" caseFolder,
      runTestForFile "for-multi-post.solc" caseFolder,
      runTestExpectingFailure "GetSet.solc" caseFolder,
      runTestExpectingFailure "GoodInstance.solc" caseFolder,
      runTestForFile "Id.solc" caseFolder,
      runTestForFile "if-examples.solc" caseFolder,
      runTestExpectingFailure "index-example.solc" caseFolder,
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
      runTestExpectingFailure "match-compiler-undef-asm.solc" caseFolder,
      runTestExpectingFailure "phantom-type-return-con.solc" caseFolder,
      runTestForFile "match-yul.solc" caseFolder,
      runTestForFile "memory.solc" caseFolder,
      runTestForFile "Memory1.solc" caseFolder,
      runTestForFile "Memory2.solc" caseFolder,
      runTestExpectingFailure "missing-instance.solc" caseFolder,
      runTestForFile "modifier.solc" caseFolder,
      runTestForFile "mptc-both-templates.solc" caseFolder,
      runTestForFile "mptc-chain-phantom.solc" caseFolder,
      runTestForFile "mptc-guard-extras-concrete.solc" caseFolder,
      runTestForFile "mptc-multi-instance.solc" caseFolder,
      runTestForFile "mptc-nop-mainty-free.solc" caseFolder,
      runTestForFile "mptc-partial-instance.solc" caseFolder,
      runTestForFile "mptc-template-a-only.solc" caseFolder,
      runTestForFile "mptc-template-b-only.solc" caseFolder,
      runTestForFile "monomorphic-require.solc" caseFolder,
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
      runTestForFile "pair-bug.solc" caseFolder,
      runTestForFile "polymatch-error.solc" caseFolder,
      runTestForFile "polymorphic-require.solc" caseFolder,
      runTestExpectingFailure "pragma_merge_fail_coverage.solc" caseFolder,
      runTestExpectingFailure "pragma_merge_fail_patterson.solc" caseFolder,
      runTestForFile "pragma_merge_base.solc" caseFolder,
      runTestExpectingFailure "pragma_merge_import.solc" caseFolder,
      runTestExpectingFailure "pragma_merge_verify.solc" caseFolder,
      runTestForFile "pragma_test_patterson.solc" caseFolder,
      runTestForFile "proxy.solc" caseFolder,
      runTestExpectingFailure "proxy1.solc" caseFolder,
      runTestForFile "rec.solc" caseFolder,
      runTestExpectingFailure "require-annotation-missing-param.solc" caseFolder,
      runTestExpectingFailure "require-annotation-missing-return.solc" caseFolder,
      runTestExpectingFailure "require-annotation-missing-both.solc" caseFolder,
      runTestExpectingFailure "require-annotation-contract-method.solc" caseFolder,
      runTestExpectingFailure "require-annotation-mutual.solc" caseFolder,
      runTestExpectingFailure "Ref.solc" caseFolder,
      runTestForFile "RefDeref.solc" caseFolder,
      runTestExpectingFailure "reference.solc" caseFolder,
      runTestForFile "reference-encoding-good.solc" caseFolder,
      runTestForFile "reference-encoding-good1.solc" caseFolder,
      runTestExpectingFailure "reference-encoding.solc" caseFolder,
      runTestExpectingFailure "reference-test.solc" caseFolder,
      runTestExpectingFailure "references-daniel.solc" caseFolder,
      runTestExpectingFailure "skolem-let.solc" caseFolder,
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
      runTestExpectingFailure "synonym-arity-mismatch.solc" caseFolder,
      runTestExpectingFailure "signature.solc" caseFolder,
      runTestExpectingFailure "spec-fail-ungrounded.solc" caseFolder,
      runTestExpectingFailure "SillyReturn.solc" caseFolder,
      runTestExpectingFailure "SimpleInvoke.solc" caseFolder,
      runTestExpectingFailure "string-const.solc" caseFolder,
      runTestExpectingFailure "StructMembers.solc" caseFolder,
      runTestExpectingFailure "subject-index.solc" caseFolder,
      runTestExpectingFailure "subject-reduction.solc" caseFolder,
      runTestExpectingFailure "subsumption-test.solc" caseFolder,
      runTestForFile "super-class.solc" caseFolder,
      runTestForFile "super-class-cycle.solc" caseFolder,
      runTestExpectingFailure "super-class-cycle-fail.solc" caseFolder,
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
      runTestForFile "word-match-default.solc" caseFolder,
      runTestForFile "sum-match-default.solc" caseFolder,
      runTestForFile "word-match.solc" caseFolder,
      runTestExpectingFailure "xref.solc" caseFolder,
      runTestForFile "yul-function-typing.solc" caseFolder,
      runTestForFile "yul-return.solc" caseFolder,
      runTestForFile "yul-multi-return.solc" caseFolder,
      runTestExpectingFailure "yul-multi-return-arity-fail.solc" caseFolder,
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
      -- functions returning functions: validate that the single type-inference
      -- pass still catches lambda type errors that closure conversion could mask.
      runTestForFile "return-fun-adder.solc" caseFolder,
      runTestForFile "return-fun-const.solc" caseFolder,
      runTestForFile "return-fun-instance.solc" caseFolder,
      runTestForFile "return-fun-eq.solc" caseFolder,
      runTestExpectingFailure "return-fun-bad-param.solc" caseFolder,
      runTestExpectingFailure "return-fun-bad-return.solc" caseFolder,
      runTestExpectingFailure "return-fun-bad-arity.solc" caseFolder,
      runTestExpectingFailure "return-fun-bad-sig.solc" caseFolder,
      runTestExpectingFailure "return-fun-not-fun.solc" caseFolder,
      runTestForFile "field-name-error.solc" caseFolder,
      runTestForFile "field-helper-cxt-collision.solc" caseFolder,
      runTestExpectingFailure "field-access.solc" caseFolder,
      runTestForFile "mod-example.solc" caseFolder,
      runTestForFile "snds.solc" caseFolder,
      runTestForFile "bool-elim.solc" caseFolder,
      runTestForFile "catch-all.solc" caseFolder,
      runTestForFile "redundant-match.solc" caseFolder,
      runTestForFile "false-redundant-warning.solc" caseFolder,
      runTestForFile "proxy-desugar.solc" caseFolder,
      runTestForFile "invokable-issue.solc" caseFolder,
      runTestForFile "td.solc" caseFolder,
      runTestForFile "bar.solc" caseFolder,
      runTestForFile "fresh-pat-arg.solc" caseFolder,
      runTestForFile "fresh-pat-arg-synonym.solc" caseFolder,
      runTestExpectingFailure "weird-error-foo.solc" caseFolder,
      runTestForFile "strange-unbound.solc" caseFolder,
      runTestForFile "type-synonym-arg.solc" caseFolder,
      runTestForFile "instance-synonym.solc" caseFolder,
      runTestForFile "instance-synonym-int.solc" caseFolder,
      runTestExpectingFailure "overlap-synonym-detected.solc" caseFolder,
      runTestExpectingFailure "overlap-synonym-missed-order.solc" caseFolder,
      runTestExpectingFailure "overlap-synonym-missed-two-synonyms.solc" caseFolder,
      runTestForFile "copytomem.solc" caseFolder,
      runTestForFile "fresh-variable-shadowing.solc" caseFolder,
      runTestForFile "simpleDiscount.solc" caseFolder,
      runTestForFile "yul-deposit-example.solc" caseFolder,
      runTestForFile "yul-asm-for-body.solc" caseFolder,
      runTestForFile
        "yul-asm-switch-body.solc"
        caseFolder,
      runTestForFile
        "multi-stmt-var-leaf.solc"
        caseFolder,
      runTestForFile "ltimp.solc" caseFolder,
      runTestExpectingFailure "class-return-type-miss.solc" caseFolder,
      runTestExpectingFailure "catenable-err.solc" caseFolder,
      runTestForFile "pars.solc" caseFolder,
      runTestForFile "bug-rep-name-capture.solc" caseFolder,
      runTestForFile "bug-import-default-inst-shadow.solc" caseFolder,
      -- Storage derivation for ADTs. These need dispatch generation: without a
      -- generated `main` the contract constructor is dead code, so the field's
      -- CanStore obligation is never forced and the negative cases would pass.
      runTestForFileWith dispatchOpt "storage-adt-recursive-ok.solc" caseFolder,
      runTestExpectingFailureWith dispatchOpt "storage-adt-recursive-fail.solc" caseFolder,
      runTestExpectingFailureWith dispatchOpt "storage-adt-mapping-field-fail.solc" caseFolder
    ]
  where
    caseFolder = "./test/examples/cases"
    dispatchOpt = emptyOption mempty

tabledResolution :: TestTree
tabledResolution =
  testGroup
    "Tabled type class resolution"
    [ testGroup
        "Cases"
        [ runTabledTestForFile "constrained-instance.solc" caseFolder,
          runTabledTestForFile "constrained-instance-context.solc" caseFolder,
          runTabledTestForFile "constructor-weak-args.solc" caseFolder,
          runTabledTestForFile "EqQual.solc" caseFolder,
          runTabledTestForFile "super-class.solc" caseFolder,
          runTabledTestForFile "super-class-num.solc" caseFolder,
          runTabledTestForFile "field-helper-cxt-collision.solc" caseFolder,
          runTabledTestForFile "instance-synonym.solc" caseFolder,
          runTabledTestForFile "invokable-issue.solc" caseFolder,
          runTabledTestForFile "td.solc" caseFolder,
          runTabledTestForFile "tuple-trick.solc" caseFolder,
          runTabledTestForFile "typedef.solc" caseFolder,
          runTabledTestForFile "word-match.solc" caseFolder,
          runTabledTestExpectingFailure "tabled-cycle-fail.solc" caseFolder
        ],
      testGroup
        "Conformance"
        [ runTabledTestForFile "tabled-answer-reuse.solc" caseFolder,
          runTabledTestForFile "tabled-default-instance.solc" caseFolder,
          runTabledTestForFile "tabled-given-order.solc" caseFolder,
          runTabledTestForFile "tabled-mutual-chain.solc" caseFolder,
          runTabledTestForFile "tabled-residual-given.solc" caseFolder,
          runTabledTestForFile "super-class-cycle.solc" caseFolder,
          runTabledTestExpectingFailure "super-class-cycle-fail.solc" caseFolder,
          runTabledTestForFile "super-class-recursive-arg.solc" caseFolder,
          runTabledTestExpectingFailure "tabled-left-recursive-fail.solc" caseFolder
        ],
      testGroup
        "Spec"
        [ runTabledTestForFile "024arith.solc" specFolder,
          runTabledTestForFile "031maybe.solc" specFolder,
          runTabledTestForFile "041pair.solc" specFolder,
          runTabledTestForFile "047rgb.solc" specFolder
        ],
      testGroup
        "Standard library"
        [ runTabledTestForFile "std.solc" stdFolder
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
