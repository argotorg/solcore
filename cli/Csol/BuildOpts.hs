module Csol.BuildOpts
  ( BuildOpts (..),
    EmitTarget (..),
    SolcOpts (..),
    buildOptsParser,
  )
where

import Data.Char (toLower)
import Data.List.Split (splitOn)
import Data.Set (Set)
import Data.Set qualified as Set
import Options qualified as Yule
import Options.Applicative
import Solcore.Pipeline.Options qualified as Solcore

data EmitTarget = EmitHull | EmitYul | EmitEvm
  deriving (Eq, Ord, Show)

data SolcOpts = SolcOpts
  { soOptimize :: Bool,
    soOptimizeRuns :: Maybe Int
  }
  deriving (Show)

data BuildOpts = BuildOpts
  { boInput :: FilePath,
    boOutput :: Maybe FilePath,
    boContract :: Maybe String,
    boEmit :: Set EmitTarget,
    boSolcore :: Solcore.Option,
    boYule :: Yule.Options,
    boSolc :: SolcOpts
  }
  deriving (Show)

buildOptsParser :: Parser BuildOpts
buildOptsParser =
  assemble
    <$> argument
      str
      ( metavar "FILE"
          <> help "Input .solc file"
      )
    <*> optional
      ( strOption
          ( long "output"
              <> short 'o'
              <> metavar "FILE"
              <> help "Output file path"
          )
      )
    <*> optional
      ( strOption
          ( long "contract"
              <> short 'c'
              <> metavar "NAME"
              <> help "Select which contract to compile (required when source has multiple contracts)"
          )
      )
    <*> option
      parseEmitTargets
      ( long "emit"
          <> metavar "TARGETS"
          <> value (Set.singleton EmitYul)
          <> help "Comma-separated emit targets: hull, yul, evm (default: yul)"
      )
    <*> switch (long "verbose" <> short 'v' <> help "Verbose output")
    <*> solcoreParser
    <*> yuleParser
    <*> solcOptsParser
  where
    assemble input output contract emit verbose sc yu solc =
      BuildOpts
        { boInput = input,
          boOutput = output,
          boContract = contract,
          boEmit = emit,
          boSolcore = sc {Solcore.fileName = input, Solcore.optVerbose = verbose},
          boYule = yu {Yule.verbose = verbose},
          boSolc = solc
        }

-- | Parse Solcore.Option fields. The fileName, optVerbose, and optOutputDir
-- fields are filled in by 'assemble' above (they overlap with shared flags).
solcoreParser :: Parser Solcore.Option
solcoreParser =
  Solcore.Option
    <$> pure "" -- fileName: filled by assemble
    <*> strOption
      ( long "include"
          <> short 'i'
          <> metavar "DIRS"
          <> value "std"
          <> help "Colon-separated list of include directories"
      )
    <*> switch (long "no-specialise" <> short 'n' <> help "Skip specialisation")
    <*> switch (long "no-desugar-calls" <> short 's' <> help "Skip indirect call desugaring")
    <*> switch (long "no-match-compiler" <> short 'm' <> help "Skip match compilation")
    <*> switch (long "no-if-desugar" <> short 'd' <> help "Skip if/bool desugaring")
    <*> switch (long "no-gen-dispatch" <> short 'g' <> help "Skip contract dispatch generation")
    <*> pure False -- optVerbose: filled by assemble
    <*> switch (long "dump-ast" <> help "Dump AST after name resolution")
    <*> switch (long "dump-dispatch" <> help "Dump dispatched contract")
    <*> switch (long "dump-ds" <> help "Dump desugared contract")
    <*> switch (long "dump-df" <> help "Dump defunctionalised contract")
    <*> switch (long "dump-spec" <> help "Dump specialised contract")
    <*> switch (long "dump-hull" <> help "Dump low-level hull")
    <*> switch (long "debug-spec" <> help "Debug specialisation")
    <*> switch (long "debug-hull" <> help "Debug hull emission")
    <*> switch (long "timing" <> help "Measure time of some phases")
    <*> optional
      ( option
          auto
          ( long "pe-fuel"
              <> metavar "N"
              <> help "Fuel for partial evaluation inlining depth limit"
          )
      )
    <*> pure Nothing -- optOutputDir: not used in csol

-- | Parse Yule.Options fields. The input, output, and verbose fields are
-- filled in by 'assemble' above (they overlap with shared flags or are unused).
yuleParser :: Parser Yule.Options
yuleParser =
  Yule.Options
    <$> pure "" -- input: not used in csol (hull objects passed in memory)
    <*> pure "Output" -- contract: not used in csol (name comes from hull objName)
    <*> pure "" -- output: not used in csol
    <*> pure False -- verbose: filled by assemble
    <*> switch (long "debug-translate" <> help "Debug Yul translation")
    <*> switch (long "compress" <> short 'O' <> help "Compress sums (experimental)")
    <*> switch (long "wrap" <> short 'w' <> help "Wrap Yul in a Solidity contract")
    <*> switch (long "nodeploy" <> help "Skip deployment code generation")

solcOptsParser :: Parser SolcOpts
solcOptsParser =
  SolcOpts
    <$> switch (long "solc-optimize" <> help "Enable solc optimizer")
    <*> optional
      ( option
          auto
          ( long "solc-optimize-runs"
              <> metavar "N"
              <> help "Optimizer runs parameter (implies --solc-optimize)"
          )
      )

parseEmitTargets :: ReadM (Set EmitTarget)
parseEmitTargets = eitherReader $ \s ->
  let parts = splitOn "," s
   in case mapM parseTarget parts of
        Nothing -> Left "Invalid emit target. Valid targets: hull, yul, evm"
        Just ts -> Right (Set.fromList ts)

parseTarget :: String -> Maybe EmitTarget
parseTarget s = case map toLower s of
  "hull" -> Just EmitHull
  "yul" -> Just EmitYul
  "evm" -> Just EmitEvm
  _ -> Nothing
