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
    [ runSimpleTestForFile "std.solc" stdFolder
    , runSimpleTestForFile "dispatch.solc" stdFolder
    ]

spec :: TestTree
spec =
  testGroup
    "Files for spec cases"
    [ runSimpleTestForFile "00answer.solc" specFolder
    , runSimpleTestForFile "01id.solc" specFolder
    , runSimpleTestForFile "02nid.solc" specFolder
    , runSimpleTestForFile "021not.solc" specFolder
    , runSimpleTestForFile "022add.solc" specFolder
    , runSimpleTestForFile "024arith.solc" specFolder
    , runSimpleTestForFile "031maybe.solc" specFolder
    , runSimpleTestForFile "032simplejoin.solc" specFolder
    , runSimpleTestForFile "033join.solc" specFolder
    , runSimpleTestForFile "034cojoin.solc" specFolder
    , runSimpleTestForFile "035padding.solc" specFolder
    , runSimpleTestForFile "036wildcard.solc" specFolder
    , runSimpleTestForFile "037dwarves.solc" specFolder
    , runSimpleTestForFile "038food0.solc" specFolder
    , runSimpleTestForFile "039food.solc" specFolder
    , runSimpleTestForFile "041pair.solc" specFolder
    , runSimpleTestForFile "042triple.solc" specFolder
    , runSimpleTestForFile "043fstsnd.solc" specFolder
    , runSimpleTestForFile "047rgb.solc" specFolder
    , runSimpleTestForFile "048rgb2.solc" specFolder
    , runSimpleTestForFile "06comp.solc" specFolder
    , runSimpleTestForFile "09not.solc" specFolder
    , runSimpleTestForFile "10negBool.solc" specFolder
    , runSimpleTestForFile "11negPair.solc" specFolder
    , runSimpleTestForFile "903badassign.solc" specFolder
    , runSimpleTestForFile "939badfood.solc" specFolder
    , runSimpleTestForFile "SimpleField.solc" specFolder
    , runSimpleTestForFile "121counter.solc" specFolder
    , runSimpleTestForFile "126nanoerc20.solc" specFolder
    , runSimpleTestForFile "127microerc20.solc" specFolder
    , runSimpleTestForFile "128minierc20.solc" specFolder
    ]
 where
  specFolder = "./test/examples/spec"

imports :: TestTree
imports =
  testGroup
    "Files for imports cases"
    [ runSimpleTestForFile "booldef.solc" importFolder
    , runSimpleTestForFile "boolmain.solc" importFolder
    ]
 where
  importFolder = "./test/imports"

pragmas :: TestTree
pragmas =
  testGroup
    "Files for pragmas cases"
    [ runTestExpectingFailure "bound.solc" pragmaFolder
    , runSimpleTestForFile "coverage.solc" pragmaFolder
    , runSimpleTestForFile "patterson.solc" pragmaFolder
    ]
 where
  pragmaFolder = "./test/examples/pragmas"

cases :: TestTree
cases =
  testGroup
    "Files for folder cases"
    [ runSimpleTestForFile "Ackermann.solc" caseFolder
    , runSimpleTestForFile "app.solc" caseFolder
    , runTestExpectingFailure "BadInstance.solc" caseFolder
    , runSimpleTestForFile "BoolNot.solc" caseFolder
    , runSimpleTestForFile "Compose.solc" caseFolder
    , runSimpleTestForFile "Compose2.solc" caseFolder
    , runSimpleTestForFile "Compose3.solc" caseFolder
    , runSimpleTestForFile "CondExp.solc" caseFolder
    , runTestExpectingFailure "DupFun.solc" caseFolder
    , runSimpleTestForFile "DuplicateFun.solc" caseFolder
    , runSimpleTestForFile "EitherModule.solc" caseFolder
    , runSimpleTestForFile "Id.solc" caseFolder
    , runSimpleTestForFile "IncompleteInstDef.solc" caseFolder
    , runTestExpectingFailure "Invokable.solc" caseFolder
    , runSimpleTestForFile "ListModule.solc" caseFolder
    , runSimpleTestForFile "Logic.solc" caseFolder
    , runSimpleTestForFile "Memory1.solc" caseFolder
    , runSimpleTestForFile "Memory2.solc" caseFolder
    , runSimpleTestForFile "Mutuals.solc" caseFolder
    , runSimpleTestForFile "NegPair.solc" caseFolder
    , runSimpleTestForFile "Option.solc" caseFolder
    , runSimpleTestForFile "Pair.solc" caseFolder
    , runTestExpectingFailure "PairMatch1.solc" caseFolder
    , runTestExpectingFailure "PairMatch2.solc" caseFolder
    , runSimpleTestForFile "Peano.solc" caseFolder
    , runSimpleTestForFile "PeanoMatch.solc" caseFolder
    , runSimpleTestForFile "RefDeref.solc" caseFolder
    , runTestExpectingFailure "SillyReturn.solc" caseFolder
    , runTestExpectingFailure "SimpleInvoke.solc" caseFolder
    , runSimpleTestForFile "closure-capture-only.solc" caseFolder
    , runSimpleTestForFile "SimpleLambda.solc" caseFolder
    , runSimpleTestForFile "SingleFun.solc" caseFolder
    , runSimpleTestForFile "assembly.solc" caseFolder
    , runSimpleTestForFile "join.solc" caseFolder
    , runSimpleTestForFile "EqQual.solc" caseFolder
    , runTestExpectingFailure "joinErr.solc" caseFolder
    , runSimpleTestForFile "tyexp.solc" caseFolder
    , runSimpleTestForFile "Uncurry.solc" caseFolder
    , runSimpleTestForFile "unit.solc" caseFolder
    , runSimpleTestForFile "memory.solc" caseFolder
    , runSimpleTestForFile "cyclical-defs.solc" caseFolder
    , runSimpleTestForFile "cyclical-defs-inferred.solc" caseFolder
    , runSimpleTestForFile "closure.solc" caseFolder
    , runSimpleTestForFile "noclosure.solc" caseFolder
    , runSimpleTestForFile "constructor-weak-args.solc" caseFolder
    , runTestExpectingFailure "unconstrained-instance.solc" caseFolder
    , runSimpleTestForFile "constrained-instance.solc" caseFolder
    , runSimpleTestForFile "constrained-instance-context.solc" caseFolder
    , runTestExpectingFailure "reference.solc" caseFolder
    , runSimpleTestForFile "super-class.solc" caseFolder
    , runSimpleTestForFile "proxy.solc" caseFolder
    , runSimpleTestForFile "another-subst.solc" caseFolder
    , runSimpleTestForFile "morefun.solc" caseFolder
    , runSimpleTestForFile "typedef.solc" caseFolder
    , runTestExpectingFailure "mainproxy.solc" caseFolder
    , runTestExpectingFailure "complexproxy.solc" caseFolder
    , runTestExpectingFailure "reference-test.solc" caseFolder
    , runSimpleTestForFile "reference-encoding-good.solc" caseFolder
    , runSimpleTestForFile "reference-encoding-good1.solc" caseFolder
    , runTestExpectingFailure "default-inst.solc" caseFolder
    , runTestExpectingFailure "default-instance-missing.solc" caseFolder
    , runTestExpectingFailure "default-instance-weak.solc" caseFolder
    , runSimpleTestForFile "tuple-trick.solc" caseFolder
    , runTestExpectingFailure "const-array.solc" caseFolder
    , runSimpleTestForFile "array.solc" caseFolder
    , runSimpleTestForFile "class-context.solc" caseFolder
    , runTestExpectingFailure "missing-instance.solc" caseFolder
    , runSimpleTestForFile "rec.solc" caseFolder
    , runSimpleTestForFile "undefined.solc" caseFolder
    , runSimpleTestForFile "foo-class.solc" caseFolder
    , runTestExpectingFailure "subsumption-test.solc" caseFolder
    -- failing due to missing assign constraint
    , runTestExpectingFailure "patterson-bug.solc" caseFolder
    , runTestExpectingFailure "listeq.solc" caseFolder
    , runTestExpectingFailure "nano-desugared.solc" caseFolder
    , runSimpleTestForFile "uintdesugared.solc" caseFolder
    , runSimpleTestForFile "word-match.solc" caseFolder
    , runSimpleTestForFile "if-examples.solc" caseFolder
    , runSimpleTestForFile "import-std.solc" caseFolder
    , runTestExpectingFailure "bound-minimal.solc" caseFolder
    , runTestExpectingFailure "bound-only-test.solc" caseFolder
    , runSimpleTestForFile "bound-with-pragma.solc" caseFolder
    , runTestExpectingFailure "withdraw.solc" caseFolder
    , runSimpleTestForFile "bal.solc" caseFolder
    , runSimpleTestForFile "ixa.solc" caseFolder
    , runSimpleTestForFile "tuva.solc" caseFolder
    , runSimpleTestForFile "yul-return.solc" caseFolder
    , runSimpleTestForFile "pragma_merge_base.solc" caseFolder
    , runSimpleTestForFile "pragma_merge_import.solc" caseFolder
    , runSimpleTestForFile "pragma_merge_verify.solc" caseFolder
    , runTestExpectingFailure "pragma_merge_fail_patterson.solc" caseFolder
    , runTestExpectingFailure "pragma_merge_fail_coverage.solc" caseFolder
    , runSimpleTestForFile "single-lambda.solc" caseFolder
    , runSimpleTestForFile "match-yul.solc" caseFolder
    , runSimpleTestForFile "dispatch.solc" stdFolder
    ]
 where
  caseFolder = "./test/examples/cases"

-- basic infrastructure for tests

type FileName = String
type BaseFolder = String

runSimpleTestForFile :: FileName -> BaseFolder -> TestTree
runSimpleTestForFile file folder = runTestForFileWith option file folder where
  option = stdOpt { optNoGenDispatch = True }

runTestForFile :: FileName -> BaseFolder -> TestTree
runTestForFile file folder = runTestForFileWith (emptyOption mempty) file folder

runTestForFileWith :: Option -> FileName -> BaseFolder -> TestTree
runTestForFileWith opts file folder =
  testCase file $ do
    let filePath = folder </> file
    result <- compile (opts { fileName = filePath })
    case result of
      Left err -> assertFailure err
      Right _ -> return ()

runTestExpectingFailure :: FileName -> BaseFolder -> TestTree
runTestExpectingFailure file folder
  = runTestExpectingFailureWith (emptyOption mempty) file folder

runTestExpectingFailureWith :: Option -> FileName -> BaseFolder -> TestTree
runTestExpectingFailureWith opts file folder =
  testCase file $ do
    let filePath = folder </> file
    result <- compile opts {fileName = filePath}
    case result of
      Left _ -> return () -- Expected failure
      Right _ -> assertFailure "Expected compilation to fail, but it succeeded"
