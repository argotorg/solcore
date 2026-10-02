module Solcore.Frontend.Parser.SolcoreParser
  ( parseCompUnit,
    parseCompUnitWithPath,
    moduleParser,
  )
where

import Data.Foldable (toList)
import Data.List (find, isPrefixOf)
import Data.List.NonEmpty qualified as NonEmpty
import Data.Void (Void)
import Solcore.Diagnostics
  ( Diagnostic (..),
    DiagnosticCode (..),
    Label (..),
    LabelStyle (..),
    Severity (..),
    SourceSpan (..),
    encodeDiagnostic,
  )
import Solcore.Frontend.Parser.Decl (compUnitP)
import Solcore.Frontend.Syntax.SyntaxTree (CompUnit)
import Text.Megaparsec (ParseErrorBundle, bundleErrors, errorBundlePretty, errorOffset, parse)
import Text.Megaparsec.Error (ErrorFancy (..), ParseError (..))

parseCompUnit :: String -> IO (Either String CompUnit)
parseCompUnit = parseCompUnitWithPath "<input>"

moduleParser :: [String] -> String -> IO (Either String CompUnit)
moduleParser _dirs =
  parseCompUnitWithPath "<input>"

parseCompUnitWithPath :: FilePath -> String -> IO (Either String CompUnit)
parseCompUnitWithPath sourcePath src =
  pure $
    case parse compUnitP sourcePath src of
      Left err -> Left (parseDiagnostic sourcePath src err)
      Right compUnit -> Right compUnit

parseDiagnostic :: FilePath -> String -> ParseErrorBundle String Void -> String
parseDiagnostic sourcePath src err =
  encodeDiagnostic
    Diagnostic
      { diagnosticSeverity = Error,
        diagnosticCode = Just (DiagnosticCode "SC0001"),
        -- A `fail "..."` in the grammar carries a specific explanation (e.g.
        -- newP rejecting `new T[][](n)`). Megaparsec keeps it as an ErrorFail,
        -- which neither the "unexpected"/"expecting" lines capture, so surface it
        -- directly as the headline when present and point the label at it.
        diagnosticMessage =
          case parseFailMessages err of
            (msg : _) -> msg
            [] -> parseDiagnosticMessage err,
        diagnosticLabels =
          [ Label
              { labelSpan = parseErrorSpan sourcePath src err,
                labelStyle = Primary,
                labelMessage =
                  case parseFailMessages err of
                    (_ : _) -> Just "not allowed here"
                    [] -> Just "unexpected token"
              }
          ],
        diagnosticNotes = parseDiagnosticNotes err,
        diagnosticHelp = []
      }

-- Explanations attached by `fail` in the grammar (megaparsec ErrorFail), pulled
-- straight out of the error bundle rather than string-matching the pretty output.
parseFailMessages :: ParseErrorBundle String Void -> [String]
parseFailMessages err =
  [ msg
  | FancyError _ fancies <- NonEmpty.toList (bundleErrors err),
    ErrorFail msg <- toList fancies
  ]

parseDiagnosticMessage :: ParseErrorBundle String Void -> String
parseDiagnosticMessage err =
  case find (isPrefixOf "unexpected ") (parseDiagnosticDetailLines err) of
    Just unexpected -> "parse error: " ++ unexpected
    Nothing -> "parse error"

parseDiagnosticNotes :: ParseErrorBundle String Void -> [String]
parseDiagnosticNotes err =
  [line | line <- parseDiagnosticDetailLines err, "expecting " `isPrefixOf` line]

parseDiagnosticDetailLines :: ParseErrorBundle String Void -> [String]
parseDiagnosticDetailLines =
  filter (not . null) . map trim . lines . errorBundlePretty

parseErrorSpan :: FilePath -> String -> ParseErrorBundle String Void -> SourceSpan
parseErrorSpan sourcePath src err =
  SourceSpan
    { spanFile = sourcePath,
      spanStartByte = offset,
      spanEndByte = offset + tokenLength,
      spanStartLine = startLine,
      spanStartColumn = startColumn,
      spanEndLine = endLine,
      spanEndColumn = endColumn
    }
  where
    offset = firstParseErrorOffset err
    tokenLength = parseErrorTokenLength src offset
    (startLine, startColumn) = offsetLineColumn src offset
    (endLine, endColumn) = offsetLineColumn src (offset + tokenLength)

firstParseErrorOffset :: ParseErrorBundle String Void -> Int
firstParseErrorOffset =
  errorOffset . NonEmpty.head . bundleErrors

parseErrorTokenLength :: String -> Int -> Int
parseErrorTokenLength src offset
  | "->" `isPrefixOf` drop offset src = 2
  | offset < length src = 1
  | otherwise = 1

offsetLineColumn :: String -> Int -> (Int, Int)
offsetLineColumn src offset =
  go 0 1 1 src
  where
    go current line column remaining
      | current >= offset = (line, column)
      | otherwise =
          case remaining of
            [] -> (line, column)
            '\n' : rest -> go (current + 1) (line + 1) 1 rest
            _ : rest -> go (current + 1) line (column + 1) rest

trim :: String -> String
trim =
  dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd predicate =
  reverse . dropWhile predicate . reverse

isSpace :: Char -> Bool
isSpace c =
  c == ' ' || c == '\t' || c == '\n' || c == '\r'
