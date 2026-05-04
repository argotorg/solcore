module HullCases (hullTests) where

import Language.Hull.Parser (parseObject)
import Language.Hull.TcEnv (emptyHullTcEnv)
import Language.Hull.TcMonad (runHullTcM)
import Language.Hull.TypeCheck (checkObject)
import System.FilePath ((</>))
import Test.Tasty
import Test.Tasty.HUnit

hullFolder :: FilePath
hullFolder = "./test/examples/hull"

hullTests :: TestTree
hullTests =
  testGroup
    "Hull type checker"
    [ testGroup
        "Valid programs"
        [ runHullTest "01-word-ops.hull",
          runHullTest "02-pair.hull",
          runHullTest "03-sum.hull",
          runHullTest "04-cond.hull",
          runHullTest "05-forward-ref.hull"
        ],
      testGroup
        "Programs with type errors"
        [ runHullTestExpectingFailure "06-err-return-type.hull",
          runHullTestExpectingFailure "07-err-undef-var.hull",
          runHullTestExpectingFailure "08-err-arity.hull",
          runHullTestExpectingFailure "09-err-sum-payload.hull",
          runHullTestExpectingFailure "10-err-fst-non-pair.hull"
        ]
    ]

runHullTest :: FilePath -> TestTree
runHullTest file = testCase file $ do
  result <- checkHullFile (hullFolder </> file)
  case result of
    Left err -> assertFailure err
    Right () -> return ()

runHullTestExpectingFailure :: FilePath -> TestTree
runHullTestExpectingFailure file = testCase file $ do
  result <- checkHullFile (hullFolder </> file)
  case result of
    Left _ -> return ()
    Right () -> assertFailure "Expected type error, but type check succeeded"

checkHullFile :: FilePath -> IO (Either String ())
checkHullFile path = do
  src <- readFile path
  let obj = parseObject path src
  runHullTcM (checkObject obj) emptyHullTcEnv
