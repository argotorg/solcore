module Common.LightYear
  ( module Text.Megaparsec,
    module Text.Megaparsec.Char,
    Parser,
    runMyParser,
    runParserE,
    runParserM,
  )
where

import Control.Monad.Error.Class
-- import Text.Megaparsec.Char.Lexer qualified as Lexer
import Data.Void
import GHC.Stack
import Text.Megaparsec
import Text.Megaparsec.Char

type Parser = Parsec Void String

runMyParser :: (HasCallStack) => String -> Parser a -> String -> a
runMyParser name p = runMyParser' p name

runMyParser' :: (HasCallStack) => Parser a -> String -> String -> a
runMyParser' p filename input =
  case parse p filename input of
    Left e -> error (errorBundlePretty e)
    Right x -> x

runParserE :: Parser a -> String -> String -> Either String a
runParserE = runParserM

runParserM :: (MonadError String m) => Parser a -> String -> String -> m a
runParserM p filename input =
  case parse p filename input of
    Left e -> throwError (errorBundlePretty e)
    Right x -> return x
