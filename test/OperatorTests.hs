module OperatorTests (operatorTests) where

import Cases (runTestExpectingFailureWith, runTestForFileWith)
import Solcore.Pipeline.Options (Option (..), stdOpt)
import Test.Tasty

operatorTests :: TestTree
operatorTests =
  testGroup
    "User-defined operators"
    [ basicTests,
      precedenceTests,
      associativityTests,
      prefixTests,
      compoundAssignTests,
      lambdaTests,
      importTests,
      errorTests
    ]

opFolder :: FilePath
opFolder = "./test/operators"

-- Run a test that is expected to succeed.
runOpSuccess :: FilePath -> TestTree
runOpSuccess file = runTestForFileWith opt file opFolder
  where
    opt = stdOpt {optNoGenDispatch = True}

-- Run a test that is expected to fail (parse or compilation error).
runOpFailure :: FilePath -> TestTree
runOpFailure file = runTestExpectingFailureWith opt file opFolder
  where
    opt = stdOpt {optNoGenDispatch = True}

basicTests :: TestTree
basicTests =
  testGroup
    "Basic declaration and use"
    [ runOpSuccess "basic.solc"
    ]

precedenceTests :: TestTree
precedenceTests =
  testGroup
    "Operator precedence"
    [ runOpSuccess "precedence.solc",
      runOpSuccess "multi-op.solc"
    ]

associativityTests :: TestTree
associativityTests =
  testGroup
    "Associativity"
    [ runOpSuccess "infixl.solc",
      runOpSuccess "infixr.solc"
    ]

prefixTests :: TestTree
prefixTests =
  testGroup
    "Prefix operators"
    [ runOpSuccess "prefix.solc"
    ]

compoundAssignTests :: TestTree
compoundAssignTests =
  testGroup
    "Compound assignment sugar (*=, /=, ~=)"
    [ runOpSuccess "compound-assign.solc"
    ]

lambdaTests :: TestTree
lambdaTests =
  testGroup
    "Operators inside lambda bodies"
    [ runOpSuccess "in-lambda.solc"
    ]

importTests :: TestTree
importTests =
  testGroup
    "Import and export of operators"
    [ runOpSuccess "import-op.solc"
    ]

errorTests :: TestTree
errorTests =
  testGroup
    "Error cases"
    [ runOpFailure "undeclared-fail.solc"
    ]
