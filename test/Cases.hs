module Cases where

import Solcore.Pipeline.SolcorePipeline
import Solcore.Pipeline.Options
import Test.Tasty
import Test.Tasty.HUnit
import System.FilePath

std :: TestTree
std =
  testGroup
    "Standard library"
    [ runTestForFile "std.sol" stdFolder
    ]
 where
  stdFolder = "./std"

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
    ]
 where
  specFolder = "./test/examples/spec"

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
    , runTestForFile "SimpleField.solc" caseFolder
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
    , runTestForFile "const-array.solc" caseFolder
    , runTestForFile "array.solc" caseFolder
    , runTestForFile "class-context.solc" caseFolder
    ]
 where
  caseFolder = "./test/examples/cases"

-- basic infrastructure for tests

type FileName = String
type BaseFolder = String

runTestForFile :: FileName -> BaseFolder -> TestTree
runTestForFile file folder =
  testCase file $ do
    let filePath = folder </> file
        opts = emptyOption filePath
    result <- compile opts
    case result of
      Left err -> assertFailure err
      Right _ -> return ()

runTestExpectingFailure :: FileName -> BaseFolder -> TestTree
runTestExpectingFailure file folder =
  testCase file $ do
    let filePath = folder </> file
        opts = emptyOption filePath
    result <- compile opts
    case result of
      Left _ -> return () -- Expected failure
      Right _ -> assertFailure "Expected compilation to fail, but it succeeded"
