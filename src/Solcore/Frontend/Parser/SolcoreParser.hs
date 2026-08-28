module Solcore.Frontend.Parser.SolcoreParser
  ( parseCompUnit,
    parseCompUnitWithPath,
    parseCompUnitWithOps,
    moduleParser,
  )
where

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
import Solcore.Frontend.Parser.OperatorScan (duplicateOperator, scanOperators, scanOperatorsLocated)
import Solcore.Frontend.Syntax.SyntaxTree (CompUnit, OperatorDecl)
import Text.Megaparsec (ParseErrorBundle, bundleErrors, errorBundlePretty, errorOffset, parse)

parseCompUnit :: String -> IO (Either String CompUnit)
parseCompUnit = parseCompUnitWithPath "<input>"

moduleParser :: [String] -> String -> IO (Either String CompUnit)
moduleParser _dirs =
  parseCompUnitWithPath "<input>"

parseCompUnitWithPath :: FilePath -> String -> IO (Either String CompUnit)
parseCompUnitWithPath = parseCompUnitWithOps []

-- Parse with a set of extra operators already in scope (e.g. imported from
-- other modules), on top of the ones declared in this source. The module
-- loader supplies the imported operators; see Module.Loader.
parseCompUnitWithOps :: [OperatorDecl] -> FilePath -> String -> IO (Either String CompUnit)
parseCompUnitWithOps extraOps sourcePath src =
  pure $
    -- Pre-scan the source for user-defined operator declarations so the
    -- expression grammar can be extended before the main parse. Each operator
    -- symbol may be declared at most once in a module; a redefinition or a
    -- second fixity for the same symbol is rejected here.
    case duplicateOperator (scanOperatorsLocated src) of
      Just (offset, sym) -> Left (duplicateOperatorDiagnostic sourcePath src offset sym)
      Nothing ->
        let ops = extraOps ++ scanOperators src
         in case parse (compUnitP ops) sourcePath src of
              Left err -> Left (parseDiagnostic sourcePath src err)
              Right compUnit -> Right compUnit

-- Diagnostic for an operator symbol declared more than once in a module.
duplicateOperatorDiagnostic :: FilePath -> String -> Int -> String -> String
duplicateOperatorDiagnostic sourcePath src offset sym =
  encodeDiagnostic
    Diagnostic
      { diagnosticSeverity = Error,
        diagnosticCode = Just (DiagnosticCode "SC0122"),
        diagnosticMessage = "operator (" ++ sym ++ ") is declared more than once",
        diagnosticLabels =
          [ Label
              { labelSpan = declSpan,
                labelStyle = Primary,
                labelMessage = Just "duplicate operator declaration"
              }
          ],
        diagnosticNotes =
          [ "an operator symbol may be declared at most once per module, including a second fixity for the same symbol"
          ],
        diagnosticHelp =
          [ "remove this declaration, or rename the operator"
          ]
      }
  where
    lineLength = max 1 (length (takeWhile (/= '\n') (drop offset src)))
    endOffset = offset + lineLength
    (startLine, startColumn) = offsetLineColumn src offset
    (endLine, endColumn) = offsetLineColumn src endOffset
    declSpan =
      SourceSpan
        { spanFile = sourcePath,
          spanStartByte = offset,
          spanEndByte = endOffset,
          spanStartLine = startLine,
          spanStartColumn = startColumn,
          spanEndLine = endLine,
          spanEndColumn = endColumn
        }

parseDiagnostic :: FilePath -> String -> ParseErrorBundle String Void -> String
parseDiagnostic sourcePath src err =
  encodeDiagnostic
    Diagnostic
      { diagnosticSeverity = Error,
        diagnosticCode = Just (DiagnosticCode "SC0001"),
        diagnosticMessage = parseDiagnosticMessage err,
        diagnosticLabels =
          [ Label
              { labelSpan = parseErrorSpan sourcePath src err,
                labelStyle = Primary,
                labelMessage = Just "unexpected token"
              }
          ],
        diagnosticNotes = parseDiagnosticNotes err,
        diagnosticHelp = []
      }

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
