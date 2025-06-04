module Options where

import Options.Applicative

data Options = Options
    { input :: FilePath
    , contract :: String
    , output :: FilePath
    , verbose :: Bool
    , debug :: Bool
    , compress :: Bool
    , yof :: Bool
    } deriving Show

optionsParser :: Parser Options
optionsParser = Options
    <$> argument str
        ( metavar "FILE"
        <> help "Input file" )
    <*> strOption
        ( long "contract"
        <> short 'c'
        <> metavar "NAME"
        <> help "Contract name"
        <> value "Output"
        <> showDefault
        )
    <*> strOption
        ( long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "Output file"
        <> value "Output.sol"
        <> showDefault
        )
    <*> switch
        ( long "verbose"
        <> short 'v'
        <> help "Verbosity level"
        )
    <*> switch
        ( long "debug"
        <> short 'd'
        <> help "Diagnostic output"
        )
    <*> switch
        ( long "compress"
        <> short 'O'
        <> help "Compress sums (experimental)"
        )
    <*> switch
        ( long "yof"
        <> short 'y'
        <> help "Output a bare Yul object"
        )

parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Compile a Core program to Yul"
     <> header "yule - experiments with Yul codegen" )
