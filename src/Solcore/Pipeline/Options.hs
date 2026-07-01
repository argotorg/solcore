module Solcore.Pipeline.Options where

import Options.Applicative
import Solcore.Diagnostics

data Option
  = Option
  { fileName :: !FilePath,
    optRootDir :: !FilePath,
    optImportDirs :: !String,
    optExternalLibs :: ![String],
    optOutputDir :: !FilePath,
    optEmitAbi :: !Bool,
    optNoSpec :: !Bool,
    optNoDesugarCalls :: !Bool,
    optNoMatchCompiler :: !Bool,
    optNoIfDesugar :: !Bool,
    optNoGenDispatch :: !Bool,
    -- Options controlling printing
    optVerbose :: !Bool,
    optDumpAST :: !Bool,
    optDumpDispatch :: !Bool,
    optDumpDS :: !Bool,
    optDumpDF :: !Bool,
    optDumpSpec :: !Bool,
    optDumpHull :: !Bool,
    -- Options controlling diagnostic output
    optDebugSpec :: !Bool,
    optDebugHull :: !Bool,
    optTiming :: !Bool,
    optDiagnosticColor :: !ColorChoice,
    optDiagnosticUnicode :: !UnicodeChoice,
    optDiagnosticWidth :: !Int,
    optDiagnosticFormat :: !DiagnosticFormat,
    optWarningPolicy :: !WarningPolicy,
    -- Partial evaluation options
    optPEFuel :: !(Maybe Int)
  }
  deriving (Eq, Show)

data WarningPolicy
  = WarningsDefault
  | WarningsAlways
  | WarningsNever
  | WarningsDeny
  deriving (Eq, Ord, Show)

emptyOption :: FilePath -> Option
emptyOption path =
  Option
    { fileName = path,
      optRootDir = ".",
      optImportDirs = "std",
      optExternalLibs = [],
      optOutputDir = ".",
      optEmitAbi = False,
      optNoSpec = False,
      optNoDesugarCalls = False,
      optNoMatchCompiler = False,
      optNoIfDesugar = False,
      optNoGenDispatch = False,
      -- Options controlling printing
      optVerbose = False,
      optDumpAST = False,
      optDumpDispatch = False,
      optDumpDS = False,
      optDumpDF = False,
      optDumpSpec = False,
      optDumpHull = False,
      -- Options controlling diagnostic output
      optDebugSpec = False,
      optDebugHull = False,
      optTiming = False,
      optDiagnosticColor = diagnosticColor defaultDiagnosticRenderOptions,
      optDiagnosticUnicode = diagnosticUnicode defaultDiagnosticRenderOptions,
      optDiagnosticWidth = diagnosticWidth defaultDiagnosticRenderOptions,
      optDiagnosticFormat = diagnosticFormat defaultDiagnosticRenderOptions,
      optWarningPolicy = WarningsDefault,
      -- Partial evaluation options
      optPEFuel = Nothing
    }

stdOpt :: Option
stdOpt = emptyOption mempty

noDesugarOpt :: Option
noDesugarOpt =
  stdOpt
    { optNoGenDispatch = True,
      optNoDesugarCalls = True,
      optNoSpec = True,
      optNoMatchCompiler = True,
      optNoIfDesugar = True
    }

options :: Parser Option
options =
  Option
    <$> strOption
      ( long "file"
          <> short 'f'
          <> metavar "FILE"
          <> help "Input file name"
      )
    <*> strOption
      ( long "root"
          <> metavar "DIR"
          <> value (optRootDir stdOpt)
          <> help "Set the main library root."
      )
    <*> strOption
      ( long "include"
          <> short 'i'
          <> metavar "DIR"
          <> value (optImportDirs stdOpt)
          <> help "Set the std library root."
      )
    <*> many
      ( strOption
          ( long "lib"
              <> metavar "NAME=DIR"
              <> help "Register an external library root for @NAME imports."
          )
      )
    <*> strOption
      ( long "output-dir"
          <> short 'o'
          <> metavar "DIR"
          <> value (optOutputDir stdOpt)
          <> help "Directory for generated output files (default: current directory)"
      )
    <*> switch
      ( long "abi"
          <> help "Emit a JSON ABI file (<ContractName>.abi) for each contract"
      )
    <*> switch
      ( long "no-specialise"
          <> short 'n'
          <> help "Skip specialisation and hull emission phases"
      )
    <*> switch
      ( long "no-desugar-calls"
          <> short 's'
          <> help "Skip indirect call desugaring"
      )
    <*> switch
      ( long "no-match-compiler"
          <> short 'm'
          <> help "Skip match compilation"
      )
    <*> switch
      ( long "no-if-desugar"
          <> short 'd'
          <> help "Skip if / bool desugaring"
      )
    <*> switch
      ( long "no-gen-dispatch"
          <> short 'g'
          <> help "Skip contract dispatch generation"
      )
    -- Options controlling printing
    <*> switch
      ( long "verbose"
          <> short 'v'
          <> help "Verbose output"
      )
    <*> switch
      ( long "dump-ast"
          <> help "Dump AST after name resolution"
      )
    <*> switch
      ( long "dump-dispatch"
          <> help "Dump dispatched contract"
      )
    <*> switch
      ( long "dump-ds"
          <> help "Dump desugared contract"
      )
    <*> switch
      ( long "dump-df"
          <> help "Dump defunctionalised contract"
      )
    <*> switch
      ( long "dump-spec"
          <> help "Dump specialised contract"
      )
    <*> switch
      ( long "dump-hull"
          <> help "Dump low-level hull"
      )
    -- Options controlling diagnostic output
    <*> switch
      ( long "debug-spec"
          <> help "Debug specialisation"
      )
    <*> switch
      ( long "debug-hull"
          <> help "Debug hull emission"
      )
    <*> switch
      ( long "timing"
          <> help "Measure time of some phases"
      )
    <*> option
      colorChoiceReader
      ( long "color"
          <> metavar "auto|always|never"
          <> value (optDiagnosticColor stdOpt)
          <> showDefaultWith showColorChoice
          <> help "Configure diagnostic colors"
      )
    <*> option
      unicodeChoiceReader
      ( long "unicode"
          <> metavar "auto|always|never"
          <> value (optDiagnosticUnicode stdOpt)
          <> showDefaultWith showUnicodeChoice
          <> help "Configure diagnostic Unicode output"
      )
    <*> option
      auto
      ( long "diagnostic-width"
          <> metavar "N"
          <> value (optDiagnosticWidth stdOpt)
          <> showDefault
          <> help "Set diagnostic output width"
      )
    <*> option
      diagnosticFormatReader
      ( long "diagnostic-format"
          <> metavar "human|short"
          <> value (optDiagnosticFormat stdOpt)
          <> showDefaultWith showDiagnosticFormat
          <> help "Configure diagnostic output format"
      )
    <*> option
      warningPolicyReader
      ( long "warnings"
          <> metavar "default|always|never|deny"
          <> value (optWarningPolicy stdOpt)
          <> showDefaultWith showWarningPolicy
          <> help "Configure compiler warning diagnostics"
      )
    -- Partial evaluation options
    <*> optional
      ( option
          auto
          ( long "pe-fuel"
              <> metavar "N"
              <> help "Fuel for partial evaluation inlining depth limit (default: 100)"
          )
      )

-- parsing command line arguments
argumentsParser :: IO Option
argumentsParser = do
  let opts =
        info
          (options <**> helper)
          ( fullDesc
              <> header "Solcore - solidity core language"
          )
  execParser opts

diagnosticRenderOptions :: Option -> DiagnosticRenderOptions
diagnosticRenderOptions opts =
  DiagnosticRenderOptions
    { diagnosticColor = optDiagnosticColor opts,
      diagnosticUnicode = optDiagnosticUnicode opts,
      diagnosticWidth = optDiagnosticWidth opts,
      diagnosticFormat = optDiagnosticFormat opts
    }

colorChoiceReader :: ReadM ColorChoice
colorChoiceReader =
  eitherReader $ \raw ->
    case raw of
      "auto" -> Right ColorAuto
      "always" -> Right ColorAlways
      "never" -> Right ColorNever
      _ -> Left "expected one of: auto, always, never"

unicodeChoiceReader :: ReadM UnicodeChoice
unicodeChoiceReader =
  eitherReader $ \raw ->
    case raw of
      "auto" -> Right UnicodeAuto
      "always" -> Right UnicodeAlways
      "never" -> Right UnicodeNever
      _ -> Left "expected one of: auto, always, never"

diagnosticFormatReader :: ReadM DiagnosticFormat
diagnosticFormatReader =
  eitherReader $ \raw ->
    case raw of
      "human" -> Right DiagnosticHuman
      "short" -> Right DiagnosticShort
      _ -> Left "expected one of: human, short"

warningPolicyReader :: ReadM WarningPolicy
warningPolicyReader =
  eitherReader $ \raw ->
    case raw of
      "default" -> Right WarningsDefault
      "always" -> Right WarningsAlways
      "never" -> Right WarningsNever
      "deny" -> Right WarningsDeny
      _ -> Left "expected one of: default, always, never, deny"

showColorChoice :: ColorChoice -> String
showColorChoice ColorAuto = "auto"
showColorChoice ColorAlways = "always"
showColorChoice ColorNever = "never"

showUnicodeChoice :: UnicodeChoice -> String
showUnicodeChoice UnicodeAuto = "auto"
showUnicodeChoice UnicodeAlways = "always"
showUnicodeChoice UnicodeNever = "never"

showDiagnosticFormat :: DiagnosticFormat -> String
showDiagnosticFormat DiagnosticHuman = "human"
showDiagnosticFormat DiagnosticShort = "short"

showWarningPolicy :: WarningPolicy -> String
showWarningPolicy WarningsDefault = "default"
showWarningPolicy WarningsAlways = "always"
showWarningPolicy WarningsNever = "never"
showWarningPolicy WarningsDeny = "deny"
