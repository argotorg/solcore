module Cases where

import Solcore.Pipeline.SolcorePipeline
import Solcore.Pipeline.Options
import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath

stdFolder :: FilePath
stdFolder = "./std"

std :: TestTree
std =
  testGroup
    "Standard library"
    [ runTestForFile "std.solc" stdFolder
    , runTestForFile "dispatch.solc" stdFolder
    ]

spec :: TestTree
spec =
  testGroup
    "Files for spec cases"
    [ runTestForFile "00answer.solc" specFolder
    , runTestForFile "01id.solc" specFolder
    , runTestForFile "02nid.solc" specFolder
    , runTestForFile "021not.solc" specFolder
    , runTestForFile "022add.solc" specFolder
    , runTestForFile "024arith.solc" specFolder
    , runTestForFile "031maybe.solc" specFolder
    , runTestForFile "032simplejoin.solc" specFolder
    , runTestForFile "033join.solc" specFolder
    , runTestForFile "034cojoin.solc" specFolder
    , runTestForFile "035padding.solc" specFolder
    , runTestForFile "036wildcard.solc" specFolder
    , runTestForFile "037dwarves.solc" specFolder
    , runTestForFile "038food0.solc" specFolder
    , runTestForFile "039food.solc" specFolder
    , runTestForFile "041pair.solc" specFolder
    , runTestForFile "042triple.solc" specFolder
    , runTestForFile "043fstsnd.solc" specFolder
    , runTestForFile "047rgb.solc" specFolder
    , runTestForFile "048rgb2.solc" specFolder
    , runTestForFile "06comp.solc" specFolder
    , runTestForFile "09not.solc" specFolder
    , runTestForFile "10negBool.solc" specFolder
    , runTestForFile "11negPair.solc" specFolder
    , runTestForFile "903badassign.solc" specFolder
    , runTestForFile "939badfood.solc" specFolder
    , runTestForFile "SimpleField.solc" specFolder
    , runTestForFile "121counter.solc" specFolder
    , runTestForFile "126nanoerc20.solc" specFolder
    , runTestForFile "127microerc20.solc" specFolder
    , runTestForFile "128minierc20.solc" specFolder
    ]
 where
  specFolder = "./test/examples/spec"

dispatches :: TestTree
dispatches =
  testGroup
    "Files for dispatch cases"
    [ runDispatchTest "basic.solc"
    , runDispatchTest "stringid.solc"
    , runDispatchTest "miniERC20.solc"
    ]
 where
  runDispatchTest file = runTestForFileWith (emptyOption mempty) file "./test/examples/dispatch"

imports :: TestTree
imports =
  testGroup
    "Files for imports cases"
    [ runTestForFile "booldef.solc" importFolder
    , runTestForFile "boolmain.solc" importFolder
    ]
 where
  importFolder = "./test/imports"

pragmas :: TestTree
pragmas =
  testGroup
    "Files for pragmas cases"
    [ runTestExpectingFailure "bound.solc" pragmaFolder
    , runTestForFile "coverage.solc" pragmaFolder
    , runTestForFile "patterson.solc" pragmaFolder
    ]
 where
  pragmaFolder = "./test/examples/pragmas"

cases :: TestTree
cases =
  testGroup
    "Files for folder cases"
    [ runTestForFile "Ackermann.solc" caseFolder
    , runTestForFile "app.solc" caseFolder
    , runTestExpectingFailure "BadInstance.solc" caseFolder
    , runTestForFile "BoolNot.solc" caseFolder
    , runTestForFile "Compose.solc" caseFolder
    , runTestForFile "Compose2.solc" caseFolder
    , runTestForFile "Compose3.solc" caseFolder
    , runTestForFile "CondExp.solc" caseFolder
    , runTestExpectingFailure "DupFun.solc" caseFolder
    , runTestForFile "DuplicateFun.solc" caseFolder
    , runTestForFile "EitherModule.solc" caseFolder
    , runTestForFile "Id.solc" caseFolder
    , runTestForFile "IncompleteInstDef.solc" caseFolder
    , runTestExpectingFailure "Invokable.solc" caseFolder
    , runTestForFile "ListModule.solc" caseFolder
    , runTestForFile "Logic.solc" caseFolder
    , runTestForFile "Memory1.solc" caseFolder
    , runTestForFile "Memory2.solc" caseFolder
    , runTestForFile "Mutuals.solc" caseFolder
    , runTestForFile "NegPair.solc" caseFolder
    , runTestForFile "Option.solc" caseFolder
    , runTestForFile "Pair.solc" caseFolder
    , runTestExpectingFailure "PairMatch1.solc" caseFolder
    , runTestExpectingFailure "PairMatch2.solc" caseFolder
    , runTestForFile "Peano.solc" caseFolder
    , runTestForFile "PeanoMatch.solc" caseFolder
    , runTestForFile "RefDeref.solc" caseFolder
    , runTestExpectingFailure "SillyReturn.solc" caseFolder
    , runTestExpectingFailure "SimpleInvoke.solc" caseFolder
    , runTestForFile "closure-capture-only.solc" caseFolder
    , runTestForFile "SimpleLambda.solc" caseFolder
    , runTestForFile "SingleFun.solc" caseFolder
    , runTestForFile "assembly.solc" caseFolder
    , runTestForFile "join.solc" caseFolder
    , runTestForFile "EqQual.solc" caseFolder
    , runTestExpectingFailure "joinErr.solc" caseFolder
    , runTestForFile "tyexp.solc" caseFolder
    , runTestForFile "Uncurry.solc" caseFolder
    , runTestForFile "unit.solc" caseFolder
    , runTestForFile "memory.solc" caseFolder
    , runTestForFile "cyclical-defs.solc" caseFolder
    , runTestForFile "cyclical-defs-inferred.solc" caseFolder
    , runTestForFile "closure.solc" caseFolder
    , runTestForFile "noclosure.solc" caseFolder
    , runTestForFile "constructor-weak-args.solc" caseFolder
    , runTestExpectingFailure "unconstrained-instance.solc" caseFolder
    , runTestForFile "constrained-instance.solc" caseFolder
    , runTestForFile "constrained-instance-context.solc" caseFolder
    , runTestExpectingFailure "reference.solc" caseFolder
    , runTestForFile "super-class.solc" caseFolder
    , runTestForFile "proxy.solc" caseFolder
    , runTestForFile "another-subst.solc" caseFolder
    , runTestForFile "morefun.solc" caseFolder
    , runTestForFile "typedef.solc" caseFolder
    , runTestExpectingFailure "mainproxy.solc" caseFolder
    , runTestExpectingFailure "complexproxy.solc" caseFolder
    , runTestExpectingFailure "reference-test.solc" caseFolder
    , runTestForFile "reference-encoding-good.solc" caseFolder
    , runTestForFile "reference-encoding-good1.solc" caseFolder
    , runTestExpectingFailure "default-inst.solc" caseFolder
    , runTestExpectingFailure "default-instance-missing.solc" caseFolder
    , runTestExpectingFailure "default-instance-weak.solc" caseFolder
    , runTestForFile "tuple-trick.solc" caseFolder
    , runTestExpectingFailure "const-array.solc" caseFolder
    , runTestForFile "array.solc" caseFolder
    , runTestForFile "class-context.solc" caseFolder
    , runTestExpectingFailure "missing-instance.solc" caseFolder
    , runTestForFile "rec.solc" caseFolder
    , runTestForFile "undefined.solc" caseFolder
    , runTestForFile "foo-class.solc" caseFolder
    , runTestExpectingFailure "subsumption-test.solc" caseFolder
    -- failing due to missing assign constraint
    , runTestExpectingFailure "patterson-bug.solc" caseFolder
    , runTestExpectingFailure "listeq.solc" caseFolder
    , runTestExpectingFailure "nano-desugared.solc" caseFolder
    , runTestForFile "uintdesugared.solc" caseFolder
    , runTestForFile "word-match.solc" caseFolder
    , runTestForFile "if-examples.solc" caseFolder
    , runTestForFile "import-std.solc" caseFolder
    , runTestExpectingFailure "bound-minimal.solc" caseFolder
    , runTestExpectingFailure "bound-only-test.solc" caseFolder
    , runTestForFile "bound-with-pragma.solc" caseFolder
    , runTestExpectingFailure "withdraw.solc" caseFolder
    , runTestForFile "bal.solc" caseFolder
    , runTestForFile "ixa.solc" caseFolder
    , runTestForFile "tuva.solc" caseFolder
    , runTestForFile "yul-return.solc" caseFolder
    , runTestForFile "pragma_merge_base.solc" caseFolder
    , runTestForFile "pragma_merge_import.solc" caseFolder
    , runTestForFile "pragma_merge_verify.solc" caseFolder
    , runTestExpectingFailure "pragma_merge_fail_patterson.solc" caseFolder
    , runTestExpectingFailure "pragma_merge_fail_coverage.solc" caseFolder
    , runTestForFile "single-lambda.solc" caseFolder
    , runTestExpectingFailure "duplicated-type-name.solc" caseFolder
    , runTestExpectingFailure "overlapping-heads.solc" caseFolder
    , runTestExpectingFailure "instance-wrong-sig.solc" caseFolder
    , runTestForFile "match-yul.solc" caseFolder
    , runTestForFile "yul-for.solc" caseFolder
    , runTestForFile "yul-function-typing.solc" caseFolder
    ]
 where
  caseFolder = "./test/examples/cases"

-- basic infrastructure for tests

type FileName = String
type BaseFolder = String

runTestForFile :: FileName -> BaseFolder -> TestTree
runTestForFile file folder = runTestForFileWith option file folder
  where
    option = stdOpt { optNoGenDispatch = True }

runTestForFileWith :: Option -> FileName -> BaseFolder -> TestTree
runTestForFileWith opts file folder =
  testCase file $ do
    let filePath = folder </> file
    result <- compile (opts { fileName = filePath })
    case result of
      Left err -> assertFailure err
      Right _ -> return ()

runTestExpectingFailure :: FileName -> BaseFolder -> TestTree
runTestExpectingFailure file folder = runTestExpectingFailureWith option file folder
  where
    option = stdOpt { optNoGenDispatch = True }

runTestExpectingFailureWith :: Option -> FileName -> BaseFolder -> TestTree
runTestExpectingFailureWith opts file folder =
  testCase file $ do
    let filePath = folder </> file
    result <- compile opts {fileName = filePath}
    case result of
      Left _ -> return () -- Expected failure
      Right _ -> assertFailure "Expected compilation to fail, but it succeeded"
