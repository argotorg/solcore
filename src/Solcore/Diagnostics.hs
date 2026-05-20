module Solcore.Diagnostics
  ( Severity (..),
    DiagnosticCode (..),
    SourceSpan (..),
    LabelStyle (..),
    Label (..),
    Diagnostic (..),
    SourceId (..),
    SourceFile (..),
    SourceMap,
    DiagnosticFormat (..),
    ColorChoice (..),
    UnicodeChoice (..),
    DiagnosticRenderOptions (..),
    defaultDiagnosticRenderOptions,
    makeSourceFile,
    sourceMapFromFiles,
    emptySourceMap,
    insertSourceFile,
    legacyDiagnostic,
    diagnosticPrimarySpan,
    renderDiagnostic,
    renderDiagnostics,
  )
where

import Data.List (foldl')
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Prettyprinter (Doc, defaultLayoutOptions, layoutPretty, pretty, vsep)
import Prettyprinter.Render.String (renderString)

data Severity
  = Error
  | Warning
  deriving (Eq, Ord, Show)

newtype DiagnosticCode = DiagnosticCode String
  deriving (Eq, Ord, Show)

data SourceSpan
  = SourceSpan
  { spanFile :: FilePath,
    spanStartByte :: Int,
    spanEndByte :: Int,
    spanStartLine :: Int,
    spanStartColumn :: Int,
    spanEndLine :: Int,
    spanEndColumn :: Int
  }
  deriving (Eq, Ord, Show)

data LabelStyle
  = Primary
  | Secondary
  deriving (Eq, Ord, Show)

data Label
  = Label
  { labelSpan :: SourceSpan,
    labelStyle :: LabelStyle,
    labelMessage :: Maybe String
  }
  deriving (Eq, Ord, Show)

data Diagnostic
  = Diagnostic
  { diagnosticSeverity :: Severity,
    diagnosticCode :: Maybe DiagnosticCode,
    diagnosticMessage :: String,
    diagnosticLabels :: [Label],
    diagnosticNotes :: [String],
    diagnosticHelp :: [String]
  }
  deriving (Eq, Ord, Show)

newtype SourceId = SourceId FilePath
  deriving (Eq, Ord, Show)

data SourceFile
  = SourceFile
  { sourceId :: SourceId,
    sourcePath :: FilePath,
    sourceText :: String,
    sourceLineStarts :: [Int]
  }
  deriving (Eq, Ord, Show)

newtype SourceMap = SourceMap (Map FilePath SourceFile)
  deriving (Eq, Ord, Show)

data DiagnosticFormat
  = DiagnosticHuman
  | DiagnosticShort
  deriving (Eq, Ord, Show)

data ColorChoice
  = ColorAuto
  | ColorAlways
  | ColorNever
  deriving (Eq, Ord, Show)

data UnicodeChoice
  = UnicodeAuto
  | UnicodeAlways
  | UnicodeNever
  deriving (Eq, Ord, Show)

data DiagnosticRenderOptions
  = DiagnosticRenderOptions
  { diagnosticColor :: ColorChoice,
    diagnosticUnicode :: UnicodeChoice,
    diagnosticWidth :: Int,
    diagnosticFormat :: DiagnosticFormat
  }
  deriving (Eq, Ord, Show)

defaultDiagnosticRenderOptions :: DiagnosticRenderOptions
defaultDiagnosticRenderOptions =
  DiagnosticRenderOptions
    { diagnosticColor = ColorAuto,
      diagnosticUnicode = UnicodeAuto,
      diagnosticWidth = 100,
      diagnosticFormat = DiagnosticHuman
    }

makeSourceFile :: FilePath -> String -> SourceFile
makeSourceFile path content =
  SourceFile
    { sourceId = SourceId path,
      sourcePath = path,
      sourceText = content,
      sourceLineStarts = computeLineStarts content
    }

sourceMapFromFiles :: [SourceFile] -> SourceMap
sourceMapFromFiles =
  foldl' (\sourceMap source -> insertSourceFile source sourceMap) emptySourceMap

emptySourceMap :: SourceMap
emptySourceMap = SourceMap Map.empty

insertSourceFile :: SourceFile -> SourceMap -> SourceMap
insertSourceFile source (SourceMap sources) =
  SourceMap (Map.insert (sourcePath source) source sources)

legacyDiagnostic :: String -> Diagnostic
legacyDiagnostic msg =
  Diagnostic
    { diagnosticSeverity = Error,
      diagnosticCode = Nothing,
      diagnosticMessage = msg,
      diagnosticLabels = [],
      diagnosticNotes = [],
      diagnosticHelp = []
    }

diagnosticPrimarySpan :: Diagnostic -> Maybe SourceSpan
diagnosticPrimarySpan diagnostic =
  case filter ((== Primary) . labelStyle) (diagnosticLabels diagnostic) of
    label : _ -> Just (labelSpan label)
    [] ->
      case diagnosticLabels diagnostic of
        label : _ -> Just (labelSpan label)
        [] -> Nothing

renderDiagnostics :: DiagnosticRenderOptions -> SourceMap -> [Diagnostic] -> String
renderDiagnostics opts sources diagnostics =
  joinWithBlankLines (map (renderDiagnostic opts sources) diagnostics)

renderDiagnostic :: DiagnosticRenderOptions -> SourceMap -> Diagnostic -> String
renderDiagnostic opts sources diagnostic =
  case diagnosticFormat opts of
    DiagnosticShort -> renderShortDiagnostic diagnostic
    DiagnosticHuman -> renderDoc (vsep (map pretty (humanDiagnosticLines sources diagnostic)))

renderDoc :: Doc ann -> String
renderDoc = renderString . layoutPretty defaultLayoutOptions

renderShortDiagnostic :: Diagnostic -> String
renderShortDiagnostic diagnostic =
  case diagnosticPrimarySpan diagnostic of
    Just sourceSpan ->
      spanFile sourceSpan
        ++ ":"
        ++ show (spanStartLine sourceSpan)
        ++ ":"
        ++ show (spanStartColumn sourceSpan)
        ++ ": "
        ++ diagnosticHeader diagnostic
    Nothing -> diagnosticHeader diagnostic

humanDiagnosticLines :: SourceMap -> Diagnostic -> [String]
humanDiagnosticLines sources diagnostic =
  [diagnosticHeader diagnostic]
    ++ locationLines diagnostic
    ++ concatMap (labelSnippetLines sources) (diagnosticLabels diagnostic)
    ++ map ("note: " ++) (diagnosticNotes diagnostic)
    ++ map ("help: " ++) (diagnosticHelp diagnostic)

diagnosticHeader :: Diagnostic -> String
diagnosticHeader diagnostic =
  severityName (diagnosticSeverity diagnostic)
    ++ codeText (diagnosticCode diagnostic)
    ++ ": "
    ++ diagnosticMessage diagnostic

severityName :: Severity -> String
severityName Error = "error"
severityName Warning = "warning"

codeText :: Maybe DiagnosticCode -> String
codeText Nothing = ""
codeText (Just (DiagnosticCode code)) = "[" ++ code ++ "]"

locationLines :: Diagnostic -> [String]
locationLines diagnostic =
  case diagnosticPrimarySpan diagnostic of
    Nothing -> []
    Just sourceSpan ->
      [ "  --> "
          ++ spanFile sourceSpan
          ++ ":"
          ++ show (spanStartLine sourceSpan)
          ++ ":"
          ++ show (spanStartColumn sourceSpan)
      ]

labelSnippetLines :: SourceMap -> Label -> [String]
labelSnippetLines (SourceMap sources) label =
  case Map.lookup (spanFile sourceSpan) sources of
    Nothing -> []
    Just source -> sourceLabelSnippet source label
  where
    sourceSpan = labelSpan label

sourceLabelSnippet :: SourceFile -> Label -> [String]
sourceLabelSnippet source label =
  [gutter]
    ++ concatMap renderLine [firstLine .. lastLine]
  where
    sourceSpan = labelSpan label
    firstLine = max 1 (spanStartLine sourceSpan)
    lastLine = max firstLine (spanEndLine sourceSpan)
    lineNoWidth = length (show lastLine)
    gutter = replicate lineNoWidth ' ' ++ " |"
    marker = case labelStyle label of
      Primary -> '^'
      Secondary -> '-'

    renderLine lineNo =
      let lineText = sourceLine source lineNo
          underline = underlineForLine sourceSpan lineNo lineText marker
          message = if lineNo == firstLine then maybe "" (" " ++) (labelMessage label) else ""
       in [ padLeft lineNoWidth (show lineNo) ++ " | " ++ lineText,
            replicate lineNoWidth ' ' ++ " | " ++ underline ++ message
          ]

underlineForLine :: SourceSpan -> Int -> String -> Char -> String
underlineForLine sourceSpan lineNo lineText marker =
  replicate (startCol - 1) ' ' ++ replicate markerWidth marker
  where
    startCol
      | lineNo == spanStartLine sourceSpan = max 1 (spanStartColumn sourceSpan)
      | otherwise = 1
    endCol
      | lineNo == spanEndLine sourceSpan = max startCol (spanEndColumn sourceSpan)
      | otherwise = max startCol (length lineText + 1)
    markerWidth = max 1 (endCol - startCol)

sourceLine :: SourceFile -> Int -> String
sourceLine source lineNo =
  case drop (lineNo - 1) (sourceLines source) of
    lineText : _ -> lineText
    [] -> ""

sourceLines :: SourceFile -> [String]
sourceLines source =
  case lines (sourceText source) of
    [] -> [""]
    xs -> xs

computeLineStarts :: String -> [Int]
computeLineStarts =
  (0 :) . reverse . fst . foldl' step ([], 0)
  where
    step (starts, offset) '\n' = (offset + 1 : starts, offset + 1)
    step (starts, offset) _ = (starts, offset + 1)

padLeft :: Int -> String -> String
padLeft width str =
  replicate (max 0 (width - length str)) ' ' ++ str

joinWithBlankLines :: [String] -> String
joinWithBlankLines [] = ""
joinWithBlankLines [x] = x
joinWithBlankLines (x : xs) = x ++ "\n\n" ++ joinWithBlankLines xs
