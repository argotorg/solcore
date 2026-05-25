module Solcore.Diagnostics
  ( Severity (..),
    DiagnosticCode (..),
    SourceSpan (..),
    LabelStyle (..),
    Label (..),
    Diagnostic (..),
    SourceId (..),
    SourceToken (..),
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
    lookupSourceFile,
    sourceMapFiles,
    sourceMapNull,
    findTokenSpansInSource,
    findTextSpansInSource,
    legacyDiagnostic,
    encodeDiagnostic,
    decodeDiagnostic,
    diagnosticPrimarySpan,
    renderDiagnostic,
    renderDiagnostics,
  )
where

import Data.Char (isAlpha, isAlphaNum)
import Data.List (foldl', isPrefixOf, stripPrefix, tails)
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Prettyprinter (Doc, LayoutOptions (..), PageWidth (..), layoutPretty, pretty, vsep)
import Prettyprinter.Render.String (renderString)
import Text.Read (readMaybe)

data Severity
  = Error
  | Warning
  deriving (Eq, Ord, Read, Show)

newtype DiagnosticCode = DiagnosticCode String
  deriving (Eq, Ord, Read, Show)

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
  deriving (Eq, Ord, Read, Show)

data LabelStyle
  = Primary
  | Secondary
  deriving (Eq, Ord, Read, Show)

data Label
  = Label
  { labelSpan :: SourceSpan,
    labelStyle :: LabelStyle,
    labelMessage :: Maybe String
  }
  deriving (Eq, Ord, Read, Show)

data Diagnostic
  = Diagnostic
  { diagnosticSeverity :: Severity,
    diagnosticCode :: Maybe DiagnosticCode,
    diagnosticMessage :: String,
    diagnosticLabels :: [Label],
    diagnosticNotes :: [String],
    diagnosticHelp :: [String]
  }
  deriving (Eq, Ord, Read, Show)

newtype SourceId = SourceId FilePath
  deriving (Eq, Ord, Show)

data SourceFile
  = SourceFile
  { sourceId :: SourceId,
    sourcePath :: FilePath,
    sourceText :: String,
    sourceLineStarts :: [Int],
    sourceTokens :: [SourceToken]
  }
  deriving (Eq, Ord, Show)

data SourceToken
  = SourceToken
  { sourceTokenText :: String,
    sourceTokenSpan :: SourceSpan
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
      sourceLineStarts = computeLineStarts content,
      sourceTokens = computeSourceTokens path content
    }

sourceMapFromFiles :: [SourceFile] -> SourceMap
sourceMapFromFiles =
  foldl' (\sourceMap source -> insertSourceFile source sourceMap) emptySourceMap

emptySourceMap :: SourceMap
emptySourceMap = SourceMap Map.empty

insertSourceFile :: SourceFile -> SourceMap -> SourceMap
insertSourceFile source (SourceMap sources) =
  SourceMap (Map.insert (sourcePath source) source sources)

lookupSourceFile :: FilePath -> SourceMap -> Maybe SourceFile
lookupSourceFile path (SourceMap sources) =
  Map.lookup path sources

sourceMapFiles :: SourceMap -> [SourceFile]
sourceMapFiles (SourceMap sources) =
  Map.elems sources

sourceMapNull :: SourceMap -> Bool
sourceMapNull (SourceMap sources) =
  Map.null sources

findTokenSpansInSource :: SourceFile -> String -> [SourceSpan]
findTokenSpansInSource source needle
  | null needle = []
  | otherwise =
      [ sourceTokenSpan sourceToken
        | sourceToken <- sourceTokens source,
          sourceTokenText sourceToken == needle
      ]

findTextSpansInSource :: SourceFile -> String -> [SourceSpan]
findTextSpansInSource source needle
  | null needle = []
  | otherwise =
      [ lineSpan lineNo lineStart column
        | (lineNo, lineStart, lineText) <- sourceLinesWithOffsets source,
          column <- findColumns needle lineText
      ]
  where
    needleLen = length needle
    lineSpan lineNo lineStart column =
      SourceSpan
        { spanFile = sourcePath source,
          spanStartByte = lineStart + column - 1,
          spanEndByte = lineStart + column - 1 + needleLen,
          spanStartLine = lineNo,
          spanStartColumn = column,
          spanEndLine = lineNo,
          spanEndColumn = column + needleLen
        }

sourceLinesWithOffsets :: SourceFile -> [(Int, Int, String)]
sourceLinesWithOffsets source =
  zipWith3
    (\lineNo lineStart lineText -> (lineNo, lineStart, lineText))
    [1 ..]
    (sourceLineStarts source)
    (sourceLines source)

findColumns :: String -> String -> [Int]
findColumns needle haystack =
  [column | (column, suffix) <- zip [1 ..] (tails haystack), needle `isPrefixOf` suffix]

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

encodeDiagnostic :: Diagnostic -> String
encodeDiagnostic diagnostic =
  diagnosticEnvelopePrefix ++ show diagnostic

decodeDiagnostic :: String -> Maybe Diagnostic
decodeDiagnostic raw = do
  encoded <- stripPrefix diagnosticEnvelopePrefix raw
  readMaybe encoded

diagnosticEnvelopePrefix :: String
diagnosticEnvelopePrefix = "\STXsolcore-diagnostic\ETX"

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
    DiagnosticShort -> renderShortDiagnostic opts diagnostic
    DiagnosticHuman -> renderDoc opts (vsep (map pretty (humanDiagnosticLines opts sources diagnostic)))

renderDoc :: DiagnosticRenderOptions -> Doc ann -> String
renderDoc opts =
  renderString . layoutPretty layoutOptions
  where
    layoutOptions =
      LayoutOptions
        { layoutPageWidth = AvailablePerLine (max 20 (diagnosticWidth opts)) 1.0
        }

renderShortDiagnostic :: DiagnosticRenderOptions -> Diagnostic -> String
renderShortDiagnostic opts diagnostic =
  case diagnosticPrimarySpan diagnostic of
    Just sourceSpan ->
      spanFile sourceSpan
        ++ ":"
        ++ show (spanStartLine sourceSpan)
        ++ ":"
        ++ show (spanStartColumn sourceSpan)
        ++ ": "
        ++ diagnosticHeader opts diagnostic
    Nothing -> diagnosticHeader opts diagnostic

humanDiagnosticLines :: DiagnosticRenderOptions -> SourceMap -> Diagnostic -> [String]
humanDiagnosticLines opts sources diagnostic =
  [diagnosticHeader opts diagnostic]
    ++ locationLines opts diagnostic
    ++ concatMap (labelSnippetLines opts (diagnosticSeverity diagnostic) sources) (diagnosticLabels diagnostic)
    ++ concatMap (prefixedWrappedLines opts "note: ") (diagnosticNotes diagnostic)
    ++ concatMap (prefixedWrappedLines opts "help: ") (diagnosticHelp diagnostic)

diagnosticHeader :: DiagnosticRenderOptions -> Diagnostic -> String
diagnosticHeader opts diagnostic =
  colorize opts (severityAnsi (diagnosticSeverity diagnostic)) header
    ++ ": "
    ++ diagnosticMessage diagnostic
  where
    header = severityName (diagnosticSeverity diagnostic) ++ codeText (diagnosticCode diagnostic)

severityName :: Severity -> String
severityName Error = "error"
severityName Warning = "warning"

codeText :: Maybe DiagnosticCode -> String
codeText Nothing = ""
codeText (Just (DiagnosticCode code)) = "[" ++ code ++ "]"

locationLines :: DiagnosticRenderOptions -> Diagnostic -> [String]
locationLines opts diagnostic =
  case diagnosticPrimarySpan diagnostic of
    Nothing -> []
    Just sourceSpan ->
      [ colorize
          opts
          locationAnsi
          ( locationArrow opts
              ++ spanFile sourceSpan
              ++ ":"
              ++ show (spanStartLine sourceSpan)
              ++ ":"
              ++ show (spanStartColumn sourceSpan)
          )
      ]

labelSnippetLines :: DiagnosticRenderOptions -> Severity -> SourceMap -> Label -> [String]
labelSnippetLines opts severity (SourceMap sources) label =
  case Map.lookup (spanFile sourceSpan) sources of
    Nothing -> []
    Just source -> sourceLabelSnippet opts severity source label
  where
    sourceSpan = labelSpan label

sourceLabelSnippet :: DiagnosticRenderOptions -> Severity -> SourceFile -> Label -> [String]
sourceLabelSnippet opts severity source label =
  [gutter]
    ++ concatMap renderLine [firstLine .. lastLine]
  where
    sourceSpan = labelSpan label
    firstLine = max 1 (spanStartLine sourceSpan)
    lastLine = max firstLine (spanEndLine sourceSpan)
    lineNoWidth = length (show lastLine)
    gutter = gutterLine opts lineNoWidth
    marker = case labelStyle label of
      Primary -> '^'
      Secondary -> '-'
    markerAnsi = case labelStyle label of
      Primary -> severityAnsi severity
      Secondary -> secondaryAnsi

    renderLine lineNo =
      let lineText = sourceLine source lineNo
          displayLine = expandTabs tabWidth lineText
          underline = underlineForLine sourceSpan lineNo lineText marker
          message = if lineNo == firstLine then maybe "" (" " ++) (labelMessage label) else ""
       in [ colorize opts lineNumberAnsi (padLeft lineNoWidth (show lineNo))
              ++ gutterSeparator opts
              ++ " "
              ++ displayLine,
            gutterLine opts lineNoWidth
              ++ " "
              ++ colorize opts markerAnsi underline
              ++ message
          ]

underlineForLine :: SourceSpan -> Int -> String -> Char -> String
underlineForLine sourceSpan lineNo lineText marker =
  replicate (startCol - 1) ' ' ++ replicate markerWidth marker
  where
    startCol
      | lineNo == spanStartLine sourceSpan = sourceColumnToVisual lineText (max 1 (spanStartColumn sourceSpan))
      | otherwise = 1
    endCol
      | lineNo == spanEndLine sourceSpan = max startCol (sourceColumnToVisual lineText (max 1 (spanEndColumn sourceSpan)))
      | otherwise = max startCol (visualLength lineText + 1)
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

computeSourceTokens :: FilePath -> String -> [SourceToken]
computeSourceTokens path content =
  concat
    [ lineTokens path lineNo lineStart lineText
      | (lineNo, lineStart, lineText) <- zip3 [1 ..] (computeLineStarts content) (sourceLinesFromText content)
    ]

sourceLinesFromText :: String -> [String]
sourceLinesFromText content =
  case lines content of
    [] -> [""]
    xs -> xs

lineTokens :: FilePath -> Int -> Int -> String -> [SourceToken]
lineTokens path lineNo lineStart lineText =
  go 1 lineText
  where
    go _ [] = []
    go column chars@(c : rest)
      | isIdentifierStart c =
          let (tokenText, suffix) = spanQualifiedIdentifier chars
              endColumn = column + length tokenText
           in SourceToken
                { sourceTokenText = tokenText,
                  sourceTokenSpan =
                    SourceSpan
                      { spanFile = path,
                        spanStartByte = lineStart + column - 1,
                        spanEndByte = lineStart + endColumn - 1,
                        spanStartLine = lineNo,
                        spanStartColumn = column,
                        spanEndLine = lineNo,
                        spanEndColumn = endColumn
                      }
                }
                : go endColumn suffix
      | otherwise = go (column + 1) rest

spanQualifiedIdentifier :: String -> (String, String)
spanQualifiedIdentifier chars =
  let (segment, rest) = span isIdentifierContinue chars
   in case rest of
        '.' : next : suffix
          | isIdentifierStart next ->
              let (tailText, finalRest) = spanQualifiedIdentifier (next : suffix)
               in (segment ++ "." ++ tailText, finalRest)
        _ -> (segment, rest)

isIdentifierStart :: Char -> Bool
isIdentifierStart c =
  isAlpha c || c == '_'

isIdentifierContinue :: Char -> Bool
isIdentifierContinue c =
  isAlphaNum c || c == '_' || c == '\''

padLeft :: Int -> String -> String
padLeft width str =
  replicate (max 0 (width - length str)) ' ' ++ str

prefixedWrappedLines :: DiagnosticRenderOptions -> String -> String -> [String]
prefixedWrappedLines opts prefix body =
  case linesOrBlank body of
    [] -> [prefix]
    firstLine : restLines ->
      wrapPhysicalLine prefix firstLine
        ++ concatMap (wrapPhysicalLine continuationPrefix) restLines
  where
    continuationPrefix = replicate (length prefix) ' '
    wrapPhysicalLine linePrefix line =
      case wrapWords (max 20 (diagnosticWidth opts)) linePrefix line of
        [] -> [linePrefix]
        wrapped -> wrapped

wrapWords :: Int -> String -> String -> [String]
wrapWords width prefix raw =
  case words raw of
    [] -> [prefix]
    word : rest -> go [prefix ++ word] rest
  where
    continuationPrefix = replicate (length prefix) ' '
    go acc [] = reverse acc
    go [] _ = []
    go (line : acc) (word : rest)
      | length line + 1 + length word <= width =
          go ((line ++ " " ++ word) : acc) rest
      | length word + length continuationPrefix <= width =
          go ((continuationPrefix ++ word) : line : acc) rest
      | otherwise =
          go ((continuationPrefix ++ word) : line : acc) rest

linesOrBlank :: String -> [String]
linesOrBlank "" = [""]
linesOrBlank body = lines body

gutterLine :: DiagnosticRenderOptions -> Int -> String
gutterLine opts lineNoWidth =
  replicate lineNoWidth ' ' ++ gutterSeparator opts

gutterSeparator :: DiagnosticRenderOptions -> String
gutterSeparator opts
  | useUnicode opts = " │"
  | otherwise = " |"

locationArrow :: DiagnosticRenderOptions -> String
locationArrow opts
  | useUnicode opts = "  ──> "
  | otherwise = "  --> "

useUnicode :: DiagnosticRenderOptions -> Bool
useUnicode opts =
  case diagnosticUnicode opts of
    UnicodeAlways -> True
    UnicodeAuto -> False
    UnicodeNever -> False

useColor :: DiagnosticRenderOptions -> Bool
useColor opts =
  case diagnosticColor opts of
    ColorAlways -> True
    ColorAuto -> False
    ColorNever -> False

colorize :: DiagnosticRenderOptions -> String -> String -> String
colorize opts ansi body
  | useColor opts = "\ESC[" ++ ansi ++ "m" ++ body ++ "\ESC[0m"
  | otherwise = body

severityAnsi :: Severity -> String
severityAnsi Error = "1;31"
severityAnsi Warning = "1;33"

locationAnsi :: String
locationAnsi = "1;36"

lineNumberAnsi :: String
lineNumberAnsi = "1;34"

secondaryAnsi :: String
secondaryAnsi = "1;34"

tabWidth :: Int
tabWidth = 4

expandTabs :: Int -> String -> String
expandTabs width =
  go 1
  where
    go _ [] = []
    go column ('\t' : rest) =
      let spaces = width - ((column - 1) `mod` width)
       in replicate spaces ' ' ++ go (column + spaces) rest
    go column (c : rest) =
      c : go (column + 1) rest

visualLength :: String -> Int
visualLength =
  length . expandTabs tabWidth

sourceColumnToVisual :: String -> Int -> Int
sourceColumnToVisual lineText sourceColumn =
  visualLength (take (sourceColumn - 1) lineText) + 1

joinWithBlankLines :: [String] -> String
joinWithBlankLines [] = ""
joinWithBlankLines [x] = x
joinWithBlankLines (x : xs) = x ++ "\n\n" ++ joinWithBlankLines xs
