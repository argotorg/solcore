module Solcore.Pipeline.Options where
import Options.Applicative

data Option
  = Option
    { fileName      :: !FilePath
    , optImportDirs :: !String
    , optNoSpec     :: !Bool
    , optNoDesugarCalls :: !Bool
    , optNoMatchCompiler :: !Bool
    , optNoIfDesugar :: !Bool
    , optNoGenDispatch :: !Bool
    -- Options controlling printing
    , optVerbose   :: !Bool
    , optDumpAST   :: !Bool
    , optDumpEnv   :: !Bool
    , optDumpDispatch :: !Bool
    , optDumpDS    :: !Bool
    , optDumpDF    :: !Bool
    , optDumpSpec  :: !Bool
    , optDumpCore  :: !Bool
    -- Options controlling diagnostic output
    , optDebugSpec :: !Bool
    , optDebugCore :: !Bool
    , optTiming    :: !Bool
    } deriving (Eq, Show)

emptyOption :: FilePath -> Option
emptyOption path = Option
    { fileName          = path
    , optImportDirs     = "std"
    , optNoSpec         = False
    , optNoDesugarCalls = False
    , optNoMatchCompiler = False
    , optNoIfDesugar    = False
    , optNoGenDispatch  = False
    -- Options controlling printing
    , optVerbose        = False
    , optDumpAST        = False
    , optDumpEnv        = False
    , optDumpDispatch   = False
    , optDumpDS         = False
    , optDumpDF         = False
    , optDumpSpec       = False
    , optDumpCore       = False
    -- Options controlling diagnostic output
    , optDebugSpec      = False
    , optDebugCore      = False
    , optTiming         = False
    }

stdOpt :: Option
stdOpt = emptyOption mempty

options :: Parser Option
options
  = Option <$> strOption (
                  long "file"
               <> short 'f'
               <> metavar "FILE"
               <> help "Input file name")
           <*> strOption ( long "include"
               <> short 'i'
               <> metavar "dirs"
               <> value (optImportDirs stdOpt)
               <> help "This flag appends a colon-separated list of dirs to the search path.")
           <*> switch ( long "no-specialise"
               <> short 'n'
               <> help "Skip specialisation and core emission phases")
           <*> switch ( long "no-desugar-calls"
               <> short 's'
               <> help "Skip indirect call desugaring")
           <*> switch (long "no-match-compiler"
               <> short 'm'
               <> help "Skip match compilation")
           <*> switch ( long "no-if-desugar"
               <> short 'd'
               <> help "Skip if / bool desugaring")
           <*> switch ( long "no-gen-dispatch"
               <> short 'g'
               <> help "Skip contract dispatch generation")
           -- Options controlling printing
           <*> switch ( long "verbose"
               <> short 'v'
               <> help "Verbose output")
           <*> switch ( long "dump-ast"
               <> help "Dump AST after name resolution")
           <*> switch ( long "dump-env"
               <> help "Dump env after name resolution")
           <*> switch ( long "dump-dispatch"
               <> help "Dump dispatched contract")
           <*> switch ( long "dump-ds"
               <> help "Dump desugared contract")
           <*> switch ( long "dump-df"
               <> help "Dump defunctionalised contract")
           <*> switch ( long "dump-spec"
               <> help "Dump specialised contract")
           <*> switch ( long "dump-core"
               <> help "Dump low-level core")
           -- Options controlling diagnostic output
           <*> switch ( long "debug-spec"
               <> help "Debug specialisation")
           <*> switch ( long "debug-core"
               <> help "Debug core emission")
           <*> switch ( long "timing"
               <> help "Measure time of some phases")


-- parsing command line arguments
argumentsParser :: IO Option
argumentsParser = do
  let opts = info (options <**> helper)
                  (fullDesc <>
                   header "Solcore - solidity core language")
  execParser opts
