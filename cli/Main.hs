module Main where

import Options.Applicative
import Csol.Build (runBuild)
import Csol.BuildOpts (BuildOpts, buildOptsParser)

newtype Command = Build BuildOpts

commandParser :: ParserInfo Command
commandParser = info (commands <**> helper)
  ( fullDesc
      <> header "csol - solcore compiler toolkit"
  )

commands :: Parser Command
commands = hsubparser
  ( command "build" (info (Build <$> buildOptsParser)
      (progDesc "Compile a .solc file to Hull, Yul, or hex"))
  )

main :: IO ()
main = execParser commandParser >>= \case
  Build opts -> runBuild opts
