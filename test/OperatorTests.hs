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
      unitTests,
      lambdaTests,
      importTests,
      commentTests,
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

unitTests :: TestTree
unitTests =
  testGroup
    "Ether and time unit suffixes (2 ether, 5 minutes)"
    [ runOpSuccess "units.solc"
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
    [ -- import-op.solc selects only the operator (^^), not its target `pow`: a
      -- user-defined operator is self-contained.
      runOpSuccess "import-op.solc",
      -- (&&)'s target `and` is hidden, yet the operator still works because its
      -- target is surfaced independently of the item selector.
      runOpSuccess "hidden-target-ok.solc",
      -- Importing only a name (std.{addWord}) does not bring std's own (+)
      -- operator into scope, so a module may declare its own (+) bound to that
      -- imported function without an operator conflict.
      runOpSuccess "local-op-import.solc"
    ]

commentTests :: TestTree
commentTests =
  testGroup
    "Operators in comments and strings are ignored"
    [ runOpSuccess "comments-and-strings-ok.solc"
    ]

errorTests :: TestTree
errorTests =
  testGroup
    "Error cases"
    [ runOpFailure "undeclared-fail.solc",
      -- A compound assignment (`+=`) whose base operator is not in scope is a
      -- parse error, not a silent no-op.
      runOpFailure "compound-undeclared-fail.solc",
      -- Declaring an operator symbol twice is a parse error (SC0122): a plain
      -- redefinition, and a second fixity for the same symbol.
      runOpFailure "redefined-fail.solc",
      runOpFailure "infix-postfix-fail.solc",
      -- A compound assignment whose base operator has the wrong fixity is a
      -- parse error: `~=` needs a unary operator, but here `~` is declared infix.
      runOpFailure "compound-fixity-fail.solc",
      -- Two operators at the same precedence with different associativities
      -- (infixl vs infixr) is a load-time error (SC0124).
      runOpFailure "assoc-conflict-fail.solc",
      -- Importing two modules that declare the same operator incompatibly is a
      -- compile error (SC0123). oplibx.solc and opliby.solc are the two
      -- conflicting library modules.
      runOpFailure "import-conflict-fail.solc",
      -- An operator declared but not exported by its module is not importable;
      -- privoplib.solc declares (%%) but exports only its function.
      runOpFailure "unexported-op-fail.solc",
      -- A missing module in an operator import surfaces the import diagnostic
      -- instead of being swallowed into unknown-operator parse errors.
      runOpFailure "bad-import-op-fail.solc"
    ]
