module Solcore.Pipeline.SolcorePipeline where

import Control.Applicative ((<|>))
import Control.Monad
import Control.Monad.Except
import Control.Monad.IO.Class (liftIO)
import Data.Bifunctor (first)
import Data.Char (isAlpha, isAlphaNum, isSpace)
import Data.List (isInfixOf, isPrefixOf, isSuffixOf, nub, stripPrefix)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe, maybeToList)
import Data.Time qualified as Time
import Language.Hull qualified as Hull
-- Pretty instances for MastCompUnit

import Solcore.Backend.ComptimeCheck (checkComptime)
import Solcore.Backend.EmitHull (emitHull)
import Solcore.Backend.Mast ()
import Solcore.Backend.MastEval (defaultFuel, eliminateDeadCode, evalCompUnit)
import Solcore.Backend.Specialise (specialiseCompUnit)
import Solcore.Desugarer.ContractDispatch (contractDispatchTopDecls, writeContractAbis)
import Solcore.Desugarer.DecisionTreeCompiler (matchCompiler, warningDiagnostic)
import Solcore.Desugarer.DeriveClasses (deriveClassTopDecls)
import Solcore.Desugarer.DeriveGeneric (collectDataDefs, deriveGenericTopDecls)
import Solcore.Desugarer.FieldAccess (fieldDesugarTopDecls)
import Solcore.Desugarer.IfDesugarer (ifDesugarer)
import Solcore.Desugarer.IndirectCall (indirectCallTopDecls)
import Solcore.Desugarer.IntLiteralDesugar (desugarIntLiterals)
import Solcore.Desugarer.ReplaceFunTypeArgs
import Solcore.Desugarer.ReplaceWildcard (replaceWildcardTopDecls)
import Solcore.Diagnostics
  ( CompilerError (..),
    Diagnostic (..),
    DiagnosticCode (..),
    Label (..),
    LabelStyle (..),
    Severity (..),
    SourceFile,
    SourceMap,
    SourceSpan (..),
    addDiagnosticHelp,
    addDiagnosticNote,
    compilerErrorDiagnostics,
    compilerErrorFromString,
    compilerErrorText,
    defaultDiagnosticRenderOptions,
    diagnosticMessage,
    diagnosticPrimarySpan,
    emptySourceMap,
    findTextSpansInSource,
    findTokenSpansInSource,
    insertSourceFile,
    lookupSourceFile,
    makeSourceFile,
    renderDiagnostics,
    resolveDiagnosticRenderOptions,
    sourceMapFiles,
  )
import Solcore.Diagnostics qualified as Diag
import Solcore.Frontend.ComptimeCheck (checkComptimeEarly)
import Solcore.Frontend.Module.Identity qualified as Mod
import Solcore.Frontend.Module.Loader (ModuleGraph (..), loadModuleGraph, moduleSourceMap, moduleSourcePath, moduleValidationTopDeclSegments)
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax hiding (contracts)
import Solcore.Frontend.Syntax.NameResolution
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.SccAnalysis
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Frontend.TypeInference.TcModule
import Solcore.Pipeline.Options (Option (..), WarningPolicy (..), argumentsParser, diagnosticRenderOptions)
import System.Directory (createDirectoryIfMissing, doesFileExist, makeAbsolute)
import System.Exit (ExitCode (..), exitWith)
import System.FilePath ((</>))
import System.TimeIt qualified as TimeIt

-- main compiler driver function
pipeline :: IO ()
pipeline = do
  _startTime <- Time.getCurrentTime
  opts <- argumentsParser
  result <- compileWithDiagnostics opts
  case result of
    Left err -> do
      rendered <- renderCompileDiagnosticsIO opts err
      putStrLn rendered
      exitWith (ExitFailure 1)
    Right contracts -> do
      let outDir = optOutputDir opts
      unless (null contracts) (createDirectoryIfMissing True outDir)
      forM_ (zip [(1 :: Int) ..] contracts) $ \(i, c) -> do
        let filename = outDir </> "output" <> show i <> ".hull"
        putStrLn ("Writing to " ++ filename)
        writeFile filename (show c)

data CompileDiagnostics
  = CompileDiagnostics
  { compileDiagnosticSources :: SourceMap,
    compileDiagnosticMessages :: [Diagnostic]
  }
  deriving (Eq, Show)

-- Version that returns Either for testing
compile :: Option -> IO (Either String [Hull.Object])
compile opts =
  first compileDiagnosticsText <$> compileWithDiagnostics opts

compileWithDiagnostics :: Option -> IO (Either CompileDiagnostics [Hull.Object])
compileWithDiagnostics opts = runExceptT $ do
  let verbose = optVerbose opts
      noMatchCompiler = optNoMatchCompiler opts
      noIfDesugar = optNoIfDesugar opts
      timeItNamed :: String -> IO a -> IO a
      timeItNamed = optTimeItNamed opts
      file = fileName opts
  mainRoot <- liftIO $ makeAbsolute (optRootDir opts)
  stdRoot <- liftEitherDiagnostic emptySourceMap (parseStdRoot (optImportDirs opts))
  externalLibs <- liftEitherDiagnostic emptySourceMap (parseExternalLibSpecs (optExternalLibs opts))

  -- Parsing and import loading
  graph <- liftEitherDiagnosticIO emptySourceMap (loadModuleGraph mainRoot stdRoot externalLibs file)
  let sources = moduleSourceMap graph

  -- Validate each module against only its own direct imports.
  forM_ (moduleOrder graph) $ \moduleId -> do
    sourcePath <- liftEitherDiagnostic sources (moduleSourcePath graph moduleId)
    (validationImports, validationSegments) <-
      liftEitherDiagnostic sources (moduleValidationTopDeclSegments graph moduleId)
    _ <-
      liftCompilerDiagnostic
        sources
        ( first (decorateCompilerDiagnosticContext ("module validation failed for " ++ sourcePath)) $
            validateDuplicateNamespacesInTopDeclSegments validationSegments
        )
    _ <-
      liftCompilerDiagnosticIO
        sources
        ( first (decorateCompilerDiagnosticContext ("module validation failed for " ++ sourcePath))
            <$> nameResolutionTopDeclSegments validationImports validationSegments
        )
    pure ()

  checkedModules <-
    liftCompilerDiagnosticIO
      sources
      ( timeItNamed "Typecheck modules" $
          runExceptT (typeCheckLoadedModules opts graph)
      )
  checkedAssembly <- liftEitherDiagnostic sources (assembleCheckedModules graph checkedModules)
  let typed = checkedAssemblyCompUnit checkedAssembly
      tcEnv = checkedAssemblyEnv checkedAssembly

  -- SAIL-level comptime verification
  liftEitherDiagnostic sources (checkComptimeEarly typed)

  -- If / boolean desugaring
  desugared <-
    liftIO $
      if noIfDesugar
        then pure typed
        else timeItNamed "If/Bool desugaring" (pure (ifDesugarer typed))

  liftIO $ when verbose $ do
    putStrLn "> If / Bool desugaring:"
    putStrLn $ pretty desugared

  -- Match compilation
  matchless <-
    if noMatchCompiler
      then pure desugared
      else do
        (ast, warns) <- liftEitherDiagnosticIO sources (timeItNamed "Match compiler" $ matchCompiler desugared)
        let warningDiagnostics = map (enrichDiagnostic sources . warningDiagnostic) warns
        handleWarningDiagnostics opts sources warningDiagnostics
        pure ast

  let printMatch = not noMatchCompiler && (verbose || optDumpDS opts)
  liftIO $ when printMatch $ do
    putStrLn "> Match compilation result:"
    putStrLn (pretty matchless)

  -- Specialization & Hull Generation
  if optNoSpec opts
    then pure []
    else do
      specialized <-
        liftIO $
          timeItNamed "Specialise    " $
            specialiseCompUnit matchless (optDebugSpec opts) tcEnv

      liftIO $ when (optDumpSpec opts) $ do
        putStrLn "> Specialised contract:"
        putStrLn (pretty specialized)

      evaluated <- liftIO $ timeItNamed "Comptime eval " $ do
        let peFuel = maybe defaultFuel id (optPEFuel opts)
            (evalResult, remainingFuel) = evalCompUnit peFuel specialized

        liftIO $
          when (remainingFuel <= 0) $
            putStrLn "!! Warning: partial evaluation ran out of fuel (use --pe-fuel N to increase)"

        liftIO $ when (optDumpSpec opts) $ do
          putStrLn "> After partial evaluation:"
          putStrLn (pretty evalResult)

        pure evalResult

      -- Dead code elimination: remove functions unreachable from 'start'/'main'
      let optimized = eliminateDeadCode evaluated

      liftIO $ when (optDumpSpec opts) $ do
        putStrLn "> After dead code elimination:"
        putStrLn (pretty optimized)

      -- Comptime verification: check comptime annotations are satisfied
      liftEitherDiagnostic sources (checkComptime optimized)

      hull <-
        liftIO $
          timeItNamed "Emit Hull     " $
            emitHull (optDebugHull opts) optimized

      liftIO $ when (optDumpHull opts) $ do
        putStrLn "> Hull contract(s):"
        forM_ hull (putStrLn . pretty)

      pure hull

renderCompileDiagnostics :: Option -> CompileDiagnostics -> String
renderCompileDiagnostics opts diagnostics =
  renderDiagnostics
    (diagnosticRenderOptions opts)
    (compileDiagnosticSources diagnostics)
    (compileDiagnosticMessages diagnostics)

renderCompileDiagnosticsIO :: Option -> CompileDiagnostics -> IO String
renderCompileDiagnosticsIO opts diagnostics = do
  renderOptions <- resolveDiagnosticRenderOptions (diagnosticRenderOptions opts)
  pure $
    renderDiagnostics
      renderOptions
      (compileDiagnosticSources diagnostics)
      (compileDiagnosticMessages diagnostics)

compileDiagnosticsText :: CompileDiagnostics -> String
compileDiagnosticsText diagnostics =
  renderDiagnostics
    defaultDiagnosticRenderOptions
    (compileDiagnosticSources diagnostics)
    (compileDiagnosticMessages diagnostics)

handleWarningDiagnostics :: Option -> SourceMap -> [Diagnostic] -> ExceptT CompileDiagnostics IO ()
handleWarningDiagnostics _ _ [] =
  pure ()
handleWarningDiagnostics opts sources diagnostics =
  case optWarningPolicy opts of
    WarningsNever -> pure ()
    WarningsDefault
      | optVerbose opts -> printWarnings
      | otherwise -> pure ()
    WarningsAlways -> printWarnings
    WarningsDeny ->
      throwError
        CompileDiagnostics
          { compileDiagnosticSources = sources,
            compileDiagnosticMessages = map denyWarning diagnostics
          }
  where
    printWarnings =
      liftIO $ do
        renderOptions <- resolveDiagnosticRenderOptions (diagnosticRenderOptions opts)
        putStrLn (renderDiagnostics renderOptions sources diagnostics)

denyWarning :: Diagnostic -> Diagnostic
denyWarning diagnostic =
  addDiagnosticHelp
    "pass --warnings=default, --warnings=always, or --warnings=never to allow this warning"
    diagnostic {diagnosticSeverity = Error}

liftEitherDiagnostic :: SourceMap -> Either String a -> ExceptT CompileDiagnostics IO a
liftEitherDiagnostic sources =
  ExceptT . pure . first (compileDiagnosticError sources)

liftEitherDiagnosticIO :: SourceMap -> IO (Either String a) -> ExceptT CompileDiagnostics IO a
liftEitherDiagnosticIO sources action =
  ExceptT $ do
    result <- action
    case result of
      Left err -> Left <$> compileDiagnosticErrorIO sources err
      Right value -> pure (Right value)

liftCompilerDiagnostic :: SourceMap -> Either CompilerError a -> ExceptT CompileDiagnostics IO a
liftCompilerDiagnostic sources =
  ExceptT . pure . first (compileCompilerError sources)

liftCompilerDiagnosticIO :: SourceMap -> IO (Either CompilerError a) -> ExceptT CompileDiagnostics IO a
liftCompilerDiagnosticIO sources action =
  ExceptT $ do
    result <- action
    case result of
      Left err -> Left <$> compileCompilerErrorIO sources err
      Right value -> pure (Right value)

compileDiagnosticError :: SourceMap -> String -> CompileDiagnostics
compileDiagnosticError sources err =
  compileCompilerError sources (compilerErrorFromString err)

compileDiagnosticErrorIO :: SourceMap -> String -> IO CompileDiagnostics
compileDiagnosticErrorIO sources err =
  compileCompilerErrorIO sources (compilerErrorFromString err)

compileCompilerError :: SourceMap -> CompilerError -> CompileDiagnostics
compileCompilerError sources err =
  let diagnostics = compilerErrorDiagnostics err
   in CompileDiagnostics
        { compileDiagnosticSources = sources,
          compileDiagnosticMessages = map (enrichDiagnostic sources) diagnostics
        }

compileCompilerErrorIO :: SourceMap -> CompilerError -> IO CompileDiagnostics
compileCompilerErrorIO sources err = do
  let diagnostics = compilerErrorDiagnostics err
  sources' <- ensureDiagnosticSources sources diagnostics
  pure
    CompileDiagnostics
      { compileDiagnosticSources = sources',
        compileDiagnosticMessages = map (enrichDiagnostic sources') diagnostics
      }

diagnosticsFromError :: String -> [Diagnostic]
diagnosticsFromError =
  compilerErrorDiagnostics . compilerErrorFromString

enrichDiagnostic :: SourceMap -> Diagnostic -> Diagnostic
enrichDiagnostic sources diagnostic
  | not (null (diagnosticLabels diagnostic)) = diagnostic
  | isDuplicateDiagnostic diagnostic,
    Just labels <- inferDuplicateLabels sources diagnostic =
      diagnostic {diagnosticLabels = labels}
  | Just label <- inferPrimaryLabel sources diagnostic =
      diagnostic {diagnosticLabels = [label]}
  | Just label <- inferFallbackLabel sources diagnostic =
      diagnostic {diagnosticLabels = [label]}
  | otherwise = diagnostic

inferPrimaryLabel :: SourceMap -> Diagnostic -> Maybe Label
inferPrimaryLabel sources diagnostic = do
  term <- firstMatchTerm sources diagnostic (diagnosticSearchTerms diagnostic)
  foundSpan <- firstSpanForTerm sources diagnostic term
  pure
    Label
      { labelSpan = foundSpan,
        labelStyle = Primary,
        labelMessage = Just (primaryLabelMessage diagnostic)
      }

inferDuplicateLabels :: SourceMap -> Diagnostic -> Maybe [Label]
inferDuplicateLabels sources diagnostic =
  case firstTwoSpans of
    [previous, duplicate] ->
      Just
        [ Label previous Secondary (Just "previous definition"),
          Label duplicate Primary (Just "duplicate definition")
        ]
    _ -> Nothing
  where
    firstTwoSpans =
      take 2 $
        concat
          [ spansForTerm sources diagnostic term
          | term <- duplicateSearchTerms diagnostic
          ]

firstMatchTerm :: SourceMap -> Diagnostic -> [String] -> Maybe String
firstMatchTerm sources diagnostic =
  go . filter (not . null)
  where
    go [] = Nothing
    go (term : terms)
      | null (spansForTerm sources diagnostic term) = go terms
      | otherwise = Just term

firstSpanForTerm :: SourceMap -> Diagnostic -> String -> Maybe SourceSpan
firstSpanForTerm sources diagnostic term =
  case spansForTerm sources diagnostic term of
    foundSpan : _ -> Just foundSpan
    [] -> Nothing

spansForTerm :: SourceMap -> Diagnostic -> String -> [SourceSpan]
spansForTerm sources diagnostic term =
  concatMap (`spansInSource` term) (candidateSources sources diagnostic)

spansInSource :: SourceFile -> String -> [SourceSpan]
spansInSource source term =
  case findTokenSpansInSource source term of
    [] -> findTextSpansInSource source term
    tokenSpans -> tokenSpans

candidateSources :: SourceMap -> Diagnostic -> [SourceFile]
candidateSources sources diagnostic =
  case mapMaybe (`lookupSourceFile` sources) (diagnosticSourcePaths diagnostic) of
    [] -> sourceMapFiles sources
    matched -> matched

diagnosticSearchTerms :: Diagnostic -> [String]
diagnosticSearchTerms diagnostic =
  uniqueStrings $
    concat
      [ prefixedTerms
          [ "undefined name: ",
            "undefined type constructor: ",
            "undefined type: ",
            "undefined field: ",
            "undefined constructor: ",
            "undefined class: ",
            "invalid pattern syntax: "
          ]
          (diagnosticMessage diagnostic),
        typeMismatchTerms diagnostic,
        unknownImportTerms diagnostic,
        moduleReferenceTerms diagnostic,
        duplicateSearchTerms diagnostic,
        declarationSearchTerms diagnostic,
        inContextSearchTerms diagnostic
      ]

inferFallbackLabel :: SourceMap -> Diagnostic -> Maybe Label
inferFallbackLabel sources diagnostic = do
  source <- firstSource (candidateSources sources diagnostic)
  pure
    Label
      { labelSpan = sourceFallbackSpan source,
        labelStyle = Primary,
        labelMessage = Just (fallbackLabelMessage diagnostic)
      }

firstSource :: [SourceFile] -> Maybe SourceFile
firstSource [] = Nothing
firstSource (source : _) = Just source

sourceFallbackSpan :: SourceFile -> SourceSpan
sourceFallbackSpan source =
  case Diag.sourceTokens source of
    token : _ -> Diag.sourceTokenSpan token
    [] ->
      SourceSpan
        { spanFile = Diag.sourcePath source,
          spanStartByte = 0,
          spanEndByte = 1,
          spanStartLine = 1,
          spanStartColumn = 1,
          spanEndLine = 1,
          spanEndColumn = 2
        }

fallbackLabelMessage :: Diagnostic -> String
fallbackLabelMessage diagnostic =
  case diagnosticCode diagnostic of
    Nothing -> "diagnostic reported here"
    Just _ -> primaryLabelMessage diagnostic

duplicateSearchTerms :: Diagnostic -> [String]
duplicateSearchTerms diagnostic =
  uniqueStrings $
    concatMap indentedTerms (allDiagnosticText diagnostic)
      ++ prefixedTerms
        [ "Duplicated function definition:",
          "Duplicated class definition:",
          "Duplicated class method definition:",
          "Duplicated type synonym definition:"
        ]
        (diagnosticMessage diagnostic)

typeMismatchTerms :: Diagnostic -> [String]
typeMismatchTerms diagnostic =
  case diagnosticCode diagnostic of
    Just (DiagnosticCode "SC0201") ->
      [trim (drop (length inPrefix) note) | note <- diagnosticNotes diagnostic, inPrefix `isPrefixOf` note, isSmallNote note]
    _ -> []
  where
    inPrefix = "in: "
    isSmallNote note = length note <= 80 && '\n' `notElem` note

unknownImportTerms :: Diagnostic -> [String]
unknownImportTerms diagnostic =
  concatMap itemTerms (allDiagnosticText diagnostic)
  where
    itemTerms line =
      case words (trim line) of
        [word]
          | "." `isInfixOf` word ->
              [word, lastSegment word]
        _ -> []

moduleReferenceTerms :: Diagnostic -> [String]
moduleReferenceTerms diagnostic =
  concatMap referenceTerms (allDiagnosticText diagnostic)
  where
    referenceTerms line =
      case words (trim line) of
        ("import" : path : _) -> modulePathTerms path
        ("export" : path : _) -> modulePathTerms path
        _ -> []

modulePathTerms :: String -> [String]
modulePathTerms raw =
  uniqueStrings
    [ dropAt cleanPath,
      cleanPath,
      lastSegment cleanPath
    ]
  where
    cleanPath =
      trimModulePath raw

trimModulePath :: String -> String
trimModulePath =
  takeWhile (\c -> c /= ';' && c /= ',' && c /= '{' && c /= '}')

declarationSearchTerms :: Diagnostic -> [String]
declarationSearchTerms diagnostic =
  concatMap declarationTerms (allDiagnosticText diagnostic)
  where
    declarationTerms raw =
      case words (stripContextPrefix (trim raw)) of
        "function" : declName : _ -> [stripTrailingParens declName]
        "contract" : declName : _ -> [stripTrailingParens declName]
        "class" : _vars : ":" : declName : _ -> [stripTrailingParens declName]
        "class" : declName : _ -> [stripTrailingParens declName]
        "data" : declName : _ -> [stripTrailingParens declName]
        "type" : declName : _ -> [stripTrailingParens declName]
        "constructor" : _ -> ["constructor"]
        "instance" : _mainTy : ":" : instanceClassName : _ -> [stripTrailingParens instanceClassName, "instance"]
        "instance" : instanceClassName : _ -> [stripTrailingParens instanceClassName, "instance"]
        _ -> []

inContextSearchTerms :: Diagnostic -> [String]
inContextSearchTerms diagnostic =
  concatMap contextTerms (allDiagnosticText diagnostic)
  where
    contextTerms raw =
      case stripPrefix "in: " (trim raw) <|> stripPrefix "- in:" (trim raw) of
        Nothing -> []
        Just context ->
          take 1 (declarationSearchTerms (diagnostic {diagnosticMessage = context, diagnosticNotes = []}))

stripContextPrefix :: String -> String
stripContextPrefix raw =
  case stripPrefix "- in:" raw of
    Just rest -> trim rest
    Nothing ->
      case stripPrefix "in: " raw of
        Just rest -> trim rest
        Nothing -> raw

stripTrailingParens :: String -> String
stripTrailingParens =
  takeWhile (\c -> c /= '(' && c /= ',' && c /= ';' && c /= '{')

prefixedTerms :: [String] -> String -> [String]
prefixedTerms prefixes body =
  [trim rest | prefix <- prefixes, Just rest <- [stripPrefix prefix body]]

primaryLabelMessage :: Diagnostic -> String
primaryLabelMessage diagnostic =
  case diagnosticCode diagnostic of
    Just (DiagnosticCode "SC0101") -> "unknown name"
    Just (DiagnosticCode "SC0102") -> "undefined type variable"
    Just (DiagnosticCode "SC0103") -> "undefined type constructor"
    Just (DiagnosticCode "SC0104") -> "invalid type synonym"
    Just (DiagnosticCode "SC0105") -> "undefined class"
    Just (DiagnosticCode "SC0106") -> "unqualified constructor"
    Just (DiagnosticCode "SC0107") -> "invalid pattern"
    Just (DiagnosticCode "SC0110") -> "unknown import item"
    Just (DiagnosticCode "SC0201") -> "expression has mismatched type"
    Just (DiagnosticCode "SC0202") -> "unknown name"
    Just (DiagnosticCode "SC0203") -> "undefined type"
    Just (DiagnosticCode "SC0204") -> "undefined field"
    Just (DiagnosticCode "SC0205") -> "undefined constructor"
    Just (DiagnosticCode "SC0206") -> "undefined function"
    Just (DiagnosticCode "SC0207") -> "undefined class"
    Just (DiagnosticCode "SC0208") -> "undefined type synonym"
    Just (DiagnosticCode "SC0209") -> "type is not polymorphic enough"
    Just (DiagnosticCode "SC0220") -> "incomplete signature"
    Just (DiagnosticCode "SC0221") -> "incomplete method signature"
    Just (DiagnosticCode "SC0222") -> "return before end of block"
    Just (DiagnosticCode "SC0223") -> "unsolved constraint"
    Just (DiagnosticCode "SC0224") -> "shorthand constructor"
    Just (DiagnosticCode "SC0225") -> "duplicate function"
    Just (DiagnosticCode "SC0226") -> "duplicate type synonym"
    Just (DiagnosticCode "SC0227") -> "duplicate class"
    Just (DiagnosticCode "SC0228") -> "duplicate class method"
    Just (DiagnosticCode "SC0229") -> "duplicate type"
    Just (DiagnosticCode "SC0299") -> "diagnostic reported here"
    Just (DiagnosticCode "SC0301") -> "redundant clause"
    Just (DiagnosticCode "SC0302") -> "non-exhaustive match"
    Just (DiagnosticCode "SC0109") -> "module reference"
    Just (DiagnosticCode "SC0118") -> "external library import"
    Just (DiagnosticCode "SC0119") -> "source file"
    Nothing -> "diagnostic reported here"
    _ -> defaultPrimaryLabelMessage diagnostic

defaultPrimaryLabelMessage :: Diagnostic -> String
defaultPrimaryLabelMessage diagnostic
  | diagnosticMessageLooksLikeHeader diagnostic = "diagnostic reported here"
  | otherwise = diagnosticMessage diagnostic

diagnosticMessageLooksLikeHeader :: Diagnostic -> Bool
diagnosticMessageLooksLikeHeader diagnostic =
  '\n' `elem` message || length message > 80
  where
    message = diagnosticMessage diagnostic

isDuplicateDiagnostic :: Diagnostic -> Bool
isDuplicateDiagnostic diagnostic =
  any
    (`isInfixOf` diagnosticMessage diagnostic)
    [ "Duplicate declarations",
      "duplicate declarations",
      "Duplicated ",
      "duplicate ",
      "Duplicate exported",
      "duplicate exported",
      "Duplicate import",
      "duplicate import",
      "Duplicate names",
      "duplicate name"
    ]
    || any ("Duplicate declarations" `isInfixOf`) (diagnosticNotes diagnostic)
    || any ("duplicate declarations" `isInfixOf`) (diagnosticNotes diagnostic)

diagnosticSourcePaths :: Diagnostic -> [FilePath]
diagnosticSourcePaths diagnostic =
  uniqueStrings (concatMap sourcePathsFromLine (allDiagnosticText diagnostic))

sourcePathsFromLine :: String -> [FilePath]
sourcePathsFromLine line =
  mapMaybe (`sourcePathAfterPrefix` line) prefixes ++ standaloneSourcePaths line
  where
    prefixes =
      [ "module validation failed for ",
        "module typecheck failed for ",
        "source file is outside library root:"
      ]

standaloneSourcePaths :: String -> [FilePath]
standaloneSourcePaths line =
  [path | let path = trim line, ".solc" `isSuffixOf` path]

sourcePathAfterPrefix :: String -> String -> Maybe FilePath
sourcePathAfterPrefix prefix line = do
  rest <- stripPrefix prefix (trim line)
  let path = trim (takeWhile (\c -> c /= ':' && c /= '(') rest)
  if null path then Nothing else Just path

allDiagnosticText :: Diagnostic -> [String]
allDiagnosticText diagnostic =
  concatMap lines (diagnosticMessage diagnostic : diagnosticNotes diagnostic ++ diagnosticHelp diagnostic)

indentedTerms :: String -> [String]
indentedTerms line
  | "  " `isPrefixOf` line =
      case words (trim line) of
        [term]
          | term /= "module" -> [term, lastSegment term]
        _ -> []
  | otherwise = []

lastSegment :: String -> String
lastSegment =
  reverse . takeWhile (/= '.') . reverse

dropAt :: String -> String
dropAt ('@' : rest) = rest
dropAt path = path

uniqueStrings :: [String] -> [String]
uniqueStrings =
  nub . filter (not . null) . map trim

trim :: String -> String
trim =
  dropWhileEnd isSpace . dropWhile isSpace

dropWhileEnd :: (a -> Bool) -> [a] -> [a]
dropWhileEnd p =
  reverse . dropWhile p . reverse

ensureDiagnosticSources :: SourceMap -> [Diagnostic] -> IO SourceMap
ensureDiagnosticSources =
  foldM ensureDiagnosticSource

ensureDiagnosticSource :: SourceMap -> Diagnostic -> IO SourceMap
ensureDiagnosticSource sources diagnostic =
  foldM ensureSourcePath sources (diagnosticReferencedSourcePaths diagnostic)

diagnosticReferencedSourcePaths :: Diagnostic -> [FilePath]
diagnosticReferencedSourcePaths diagnostic =
  uniqueStrings $
    maybeToList (spanFile <$> diagnosticPrimarySpan diagnostic)
      ++ diagnosticSourcePaths diagnostic

ensureSourcePath :: SourceMap -> FilePath -> IO SourceMap
ensureSourcePath sources path
  | null path = pure sources
  | Just _ <- lookupSourceFile path sources = pure sources
  | otherwise = do
      exists <- doesFileExist path
      if exists
        then do
          content <- readFile path
          pure (insertSourceFile (makeSourceFile path content) sources)
        else pure sources

typeCheckLoadedModules :: Option -> ModuleGraph -> ExceptT CompilerError IO (Map Mod.ModuleId CheckedModule)
typeCheckLoadedModules opts graph =
  Map.fromList <$> mapM (typeCheckModuleFromGraph opts graph) (moduleOrder graph)

typeCheckModuleFromGraph ::
  Option ->
  ModuleGraph ->
  Mod.ModuleId ->
  ExceptT CompilerError IO (Mod.ModuleId, CheckedModule)
typeCheckModuleFromGraph opts graph moduleId = do
  sourcePath <- ExceptT $ pure (first compilerErrorFromString (moduleSourcePath graph moduleId))
  resolvedInput <-
    ExceptT $
      first (moduleTypeCheckError sourcePath "input") <$> loadModuleLocalTypeCheckInput graph moduleId
  liftIO $ dumpModuleResolvedAST opts sourcePath resolvedInput
  moduleInput <- prepareModuleTypeCheckInput opts resolvedInput
  (typed, tcEnv) <-
    ExceptT $
      first (moduleTypeCheckError sourcePath "") <$> typeInferModuleLocals opts moduleInput
  liftIO $
    when (optVerbose opts) $
      putStrLn ("No type errors found for " ++ sourcePath ++ "!")
  liftIO $ dumpModuleTypeInference opts sourcePath typed tcEnv
  pure
    ( moduleId,
      CheckedModule
        { checkedModuleId = moduleId,
          checkedModuleInput = moduleInput,
          checkedModuleTyped = typed,
          checkedModuleEnv = tcEnv
        }
    )

prepareModuleTypeCheckInput ::
  Option ->
  ModuleResolvedTypeCheckInput ->
  ExceptT CompilerError IO ModuleTypeCheckInput
prepareModuleTypeCheckInput opts resolvedInput = do
  inferenceDecls <- prepareModuleInferenceDeclsForTypeInference opts resolvedInput
  pure (withPreparedModuleInferenceDecls resolvedInput inferenceDecls)

prepareModuleInferenceDeclsForTypeInference ::
  Option ->
  ModuleResolvedTypeCheckInput ->
  ExceptT CompilerError IO [ModuleInferenceDecl]
prepareModuleInferenceDeclsForTypeInference opts input =
  prepareInferenceDeclsForTypeInference
    opts
    (emitModulePreparationDiagnostics opts)
    (moduleResolvedImports input)
    (moduleInitialInferenceDecls input)

dumpModuleResolvedAST :: Option -> FilePath -> ModuleResolvedTypeCheckInput -> IO ()
dumpModuleResolvedAST opts sourcePath input =
  when (optVerbose opts || optDumpAST opts) $ do
    putStrLn ("> AST after name resolution for " ++ sourcePath)
    putStrLn $ pretty dumpCompUnit
  where
    dumpCompUnit =
      CompUnit
        (moduleResolvedImports input)
        ( moduleResolvedQualifiedDecls input
            ++ moduleResolvedLocalDecls input
            ++ moduleResolvedImportedDecls input
        )

emitModulePreparationDiagnostics :: Option -> Bool
emitModulePreparationDiagnostics opts =
  or
    [ optVerbose opts,
      optDumpDispatch opts,
      optDumpDF opts
    ]

dumpModuleTypeInference :: Option -> FilePath -> CompUnit Id -> TcEnv -> IO ()
dumpModuleTypeInference opts sourcePath typed tcEnv =
  when (optVerbose opts) $ do
    putStrLn ("> Type inference logs for " ++ sourcePath ++ ":")
    mapM_ putStrLn (reverse $ logs tcEnv)
    putStrLn ("> Elaborated tree for " ++ sourcePath ++ ":")
    putStrLn $ pretty typed

moduleTypeCheckError :: FilePath -> String -> CompilerError -> CompilerError
moduleTypeCheckError sourcePath phase err =
  decorateCompilerDiagnosticContext
    ("module typecheck failed for " ++ sourcePath ++ phaseSuffix)
    err
  where
    phaseSuffix
      | null phase = ""
      | otherwise = " (" ++ phase ++ ")"

decorateDiagnosticContext :: String -> String -> String
decorateDiagnosticContext context err =
  compilerErrorText (decorateCompilerDiagnosticContext context (compilerErrorFromString err))

decorateCompilerDiagnosticContext :: String -> CompilerError -> CompilerError
decorateCompilerDiagnosticContext context (CompilerDiagnostics diagnostics) =
  CompilerDiagnostics (map (addDiagnosticNote context) diagnostics)
decorateCompilerDiagnosticContext context (CompilerLegacyError err) =
  CompilerLegacyError (context ++ ":\n" ++ err)

prepareInferenceDeclsForTypeInference ::
  Option ->
  Bool ->
  [Import] ->
  [ModuleInferenceDecl] ->
  ExceptT CompilerError IO [ModuleInferenceDecl]
prepareInferenceDeclsForTypeInference opts emitOutput imps inferenceDecls = do
  let verbose = emitOutput && optVerbose opts
      noDesugarCalls = optNoDesugarCalls opts
      noGenDispatch = optNoGenDispatch opts
      prettyInferenceDecls inferenceDumpDecls =
        pretty (CompUnit imps (moduleInferenceTopDecls inferenceDumpDecls))
      timeItNamed :: String -> IO a -> IO a
      timeItNamed
        | emitOutput = optTimeItNamed opts
        | otherwise = \_ action -> action

  -- contract field access desugaring
  let accessed = mapModuleInferenceTopDecls fieldDesugarTopDecls inferenceDecls
  liftIO $ when verbose $ do
    putStrLn "Contract field access desugaring:"
    putStrLn $ prettyInferenceDecls accessed

  -- Emit a JSON ABI file for each of the module's own contracts, named
  -- <ContractName>.abi. Gated by --abi.
  liftIO $ when (optEmitAbi opts) $ do
    let localTopDecls =
          [ moduleInferenceDeclTopDecl d
          | d <- accessed,
            moduleInferenceDeclSegment d == ModuleLocalDecl
          ]
    writeContractAbis (optOutputDir opts) localTopDecls

  -- contract dispatch generation
  dispatched <-
    liftIO $
      if noGenDispatch
        then pure accessed
        else timeItNamed "Contract dispatch generation" $ pure (mapModuleInferenceTopDecls contractDispatchTopDecls accessed)

  liftIO $ when (emitOutput && optDumpDispatch opts) $ do
    putStrLn "> Dispatch:"
    putStrLn $ prettyInferenceDecls dispatched

  -- Generic instance derivation (also for data types declared inside contracts:
  -- collectDataDefs descends into contracts).  The generated instances are
  -- top-level, but the contract-local type they reference stays inside its
  -- contract; it is made visible to them by registering it globally during type
  -- checking (see registerContractDataTypes).
  let localData =
        collectDataDefs
          [d | ModuleInferenceDecl ModuleLocalDecl d <- dispatched]
  derived <-
    ExceptT $
      fmap (first compilerErrorFromString) $
        runExceptT $
          traverseModuleInferenceTopDecls (ExceptT . pure . deriveGenericTopDecls localData) dispatched

  liftIO $ when verbose $ do
    putStrLn "> Generic instance derivation:"
    putStrLn $ prettyInferenceDecls derived

  -- Class instance derivation from `deriving (...)` clauses
  derivedClasses <-
    ExceptT $
      fmap (first compilerErrorFromString) $
        runExceptT $
          traverseModuleInferenceTopDecls (ExceptT . pure . deriveClassTopDecls localData) derived

  liftIO $ when verbose $ do
    putStrLn "> Class instance derivation:"
    putStrLn $ prettyInferenceDecls derivedClasses

  -- SCC analysis
  connected <-
    ExceptT $
      fmap (first compilerErrorFromString) $
        timeItNamed "SCC           " $
          runExceptT $
            traverseModuleInferenceTopDecls (ExceptT . sccAnalysisTopDecls) derivedClasses

  liftIO $ when verbose $ do
    putStrLn "> SCC Analysis:"
    putStrLn $ prettyInferenceDecls connected

  -- Indirect call handling
  direct <-
    liftIO $
      if noDesugarCalls
        then pure connected
        else
          timeItNamed "Indirect Calls" $
            traverseModuleInferenceTopDecls (fmap fst . indirectCallTopDecls) connected

  liftIO $ when (emitOutput && (optVerbose opts || optDumpDF opts)) $ do
    putStrLn "> Indirect call desugaring:"
    putStrLn $ prettyInferenceDecls direct

  -- Pattern wildcard desugaring
  let noWild = mapModuleInferenceTopDecls replaceWildcardTopDecls direct
  liftIO $ when verbose $ do
    putStrLn "> Pattern wildcard desugaring:"
    putStrLn $ prettyInferenceDecls noWild

  -- Eliminate function type arguments
  let noFun = if noDesugarCalls then noWild else mapModuleInferenceTopDecls replaceFunParam noWild
  liftIO $ when verbose $ do
    putStrLn "> Eliminating argments with function types"
    putStrLn $ prettyInferenceDecls noFun

  -- Integer literal desugaring: wrap bare integer literals in fromInteger()
  let withFromInt = mapModuleInferenceTopDecls desugarIntLiterals noFun
  liftIO $ when verbose $ do
    putStrLn "> Integer literal desugaring:"
    putStrLn $ prettyInferenceDecls withFromInt

  pure withFromInt

parseExternalLibSpecs :: [String] -> Either String [(Name, FilePath)]
parseExternalLibSpecs =
  fmap reverse . foldM step []
  where
    step acc spec = do
      (libName, libPath) <- splitSpec spec
      when (any ((== libName) . fst) acc) $
        Left ("Duplicate external library name: " ++ show libName)
      pure ((libName, libPath) : acc)

    splitSpec spec =
      case break (== '=') spec of
        (libNameStr, '=' : path)
          | null libNameStr || null path ->
              Left ("Invalid external library spec: " ++ spec)
          | not (validLibName libNameStr) ->
              Left ("Invalid external library name: " ++ libNameStr)
          | otherwise ->
              Right (Name libNameStr, path)
        _ ->
          Left ("Invalid external library spec: " ++ spec)

    validLibName [] = False
    validLibName (c : cs) =
      (isAlpha c || c == '_')
        && all (\ch -> isAlphaNum ch || ch == '_') cs

parseStdRoot :: String -> Either String (Maybe FilePath)
parseStdRoot spec =
  case filter (not . null) (splitColon spec) of
    [] -> Right Nothing
    [root] -> Right (Just root)
    _ ->
      Left "Multiple --include roots are no longer supported; use --lib for external libraries."
  where
    splitColon [] = []
    splitColon s =
      case break (== ':') s of
        (chunk, ':' : rest) -> chunk : splitColon rest
        (chunk, _) -> [chunk]

-- add declarations generated in the previous step
-- and moving data types inside contracts to the
-- global scope.
moveData :: CompUnit Name -> CompUnit Name
moveData (CompUnit imps decls1) =
  CompUnit imps (foldr step [] decls1)
  where
    step (TContr c) ac =
      let (dts, c') = extractData c
          dts' = map TDataDef dts
       in (TContr c') : dts' ++ ac
    step d ac = d : ac

extractData :: Contract Name -> ([DataTy], Contract Name)
extractData (Contract n ts ds) =
  (ds1, Contract n ts ds0)
  where
    (ds1, ds0) = foldr step ([], []) ds
    step (CDataDecl dt) (dts, cs) = (dt : dts, cs)
    step c (dts, cs) = (dts, c : cs)

addGenerated ::
  CompUnit Name ->
  [TopDecl Name] ->
  CompUnit Name
addGenerated (CompUnit imps ds) ts =
  CompUnit imps (ds ++ ts)

optTimeItNamed :: Option -> String -> IO a -> IO a
optTimeItNamed opts s a = if (optTiming opts) then TimeIt.timeItNamed s a else a
