module ReferenceSyntaxTests (referenceSyntaxTests) where

import Control.Monad (forM_)
import Data.List (sort)
import Solcore.Frontend.Parser.SolcoreParser (parseCompUnitWithPath)
import Solcore.Frontend.Pretty.TreePretty qualified as TreePretty
import Solcore.Pipeline.Options (Option (..), emptyOption)
import Solcore.Pipeline.SolcorePipeline (compile)
import System.Directory (listDirectory)
import System.FilePath (takeExtension, (</>))
import Test.Tasty
import Test.Tasty.HUnit

referenceSyntaxTests :: TestTree
referenceSyntaxTests =
  testGroup
    "Reference syntax compatibility"
    [ testCase "reference parser fixtures parse and round trip" $ do
        let folder = "test/new-syntax/reference"
        files <- sort . filter ((== ".sol") . takeExtension) <$> listDirectory folder
        length files @?= 23
        forM_ files $ \file -> do
          let path = folder </> file
          source <- readFile path
          parsed <- parseCompUnitWithPath path source
          case parsed of
            Left err -> assertFailure (path ++ "\n" ++ err)
            Right unit -> do
              let rendered = TreePretty.pretty unit
              reparsed <- parseCompUnitWithPath path rendered
              case reparsed of
                Left err -> assertFailure (path ++ " pretty output:\n" ++ rendered ++ "\n" ++ err)
                Right actual -> assertEqual (path ++ " round trip") unit actual,
      testCase "canonical syntax compiles through specialization" $ do
        let folder = "test/new-syntax/integration"
            options =
              (emptyOption (folder </> "main.sol"))
                { optRootDir = folder
                }
        result <- compile options
        case result of
          Left err -> assertFailure err
          Right _ -> pure ()
    ]
