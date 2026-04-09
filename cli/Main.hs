module Main where

import Options.Applicative
import Csol.Build (runBuild)
import Csol.BuildOpts (BuildOpts, buildOptsParser)
import Csol.Run (execute)
import Csol.RunOpts (RunOpts, runOptsParser)

data Command = Build BuildOpts | Run BuildOpts RunOpts

commandParser :: ParserInfo Command
commandParser = info (commands <**> helper)
  ( fullDesc
      <> header "csol - solcore compiler toolkit"
  )

commands :: Parser Command
commands = hsubparser
  ( command "build" (info (Build <$> buildOptsParser)
      (progDesc "Compile a .solc file to Hull, Yul, or hex"))
 <> command "run" (info (Run <$> buildOptsParser <*> runOptsParser)
      (progDesc "Build and execute a .solc file"))
  )

main :: IO ()
main = execParser commandParser >>= \case
  Build opts      -> runBuild opts
  Run bOpts rOpts -> execute bOpts rOpts
