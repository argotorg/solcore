module Options where

import Options.Applicative

data Options = Options
    { input :: FilePath
    , contract :: String
    , output :: FilePath
    , verbose :: Bool
    , debug :: Bool
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
        <> value "Output")
    <*> strOption
        ( long "output"
        <> short 'o'
        <> metavar "FILE"
        <> help "Output file"
        <> value "Output.sol")
    <*> switch
        ( long "verbose"
        <> short 'v'
        <> help "Verbosity level"
        <> showDefault
        )
    <*> switch
        ( long "debug"
        <> short 'd'
        <> help "Diagnostic output"
        <> showDefault
        )

parseOptions :: IO Options
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Compile a Core program to Yul"
     <> header "yule - experiments with Yul codegen" )