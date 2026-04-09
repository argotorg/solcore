module Csol.RunOpts
  ( RunOpts (..),
    CalldataSpec (..),
    runOptsParser,
  )
where

import EVM.Types (W256)
import Options.Applicative

data CalldataSpec
  = AbiCall String [String]
  | RawHex String
  | NoCalldata
  deriving (Show)

data RunOpts = RunOpts
  { roCreate :: Bool,
    roRuntimeCalldata :: CalldataSpec,
    roRuntimeCallvalue :: Maybe W256,
    roCreateCalldata :: CalldataSpec,
    roCreateCallvalue :: Maybe W256
  }
  deriving (Show)

runOptsParser :: Parser RunOpts
runOptsParser =
  RunOpts
    <$> createFlag
    <*> calldataParser "runtime" "runtime-raw-calldata"
    <*> optional
      ( option
          auto
          ( long "runtime-callvalue"
              <> metavar "WEI"
              <> help "Value in wei for runtime call"
          )
      )
    <*> calldataParser "create" "create-raw-args"
    <*> optional
      ( option
          auto
          ( long "create-callvalue"
              <> metavar "WEI"
              <> help "Value in wei for create call"
          )
      )

calldataParser :: String -> String -> Parser CalldataSpec
calldataParser prefix rawName =
  AbiCall
    <$> sigParser
    <*> many argParser
      <|> RawHex
    <$> rawParser
      <|> pure NoCalldata
  where
    sigParser =
      strOption
        ( long (prefix ++ "-sig")
            <> metavar "SIG"
            <> help ("Function signature, e.g. 'transfer(address,uint256)'")
        )
    argParser =
      strOption
        ( long (prefix ++ "-arg")
            <> metavar "ARG"
            <> help "Function argument (repeatable)"
        )
    rawParser =
      strOption
        ( long rawName
            <> metavar "HEX"
            <> help "Raw hex calldata"
        )

createFlag :: Parser Bool
createFlag =
  flag' True (long "create" <> help "Run initcode to deploy (default)")
    <|> flag' False (long "no-create" <> help "Skip deployment, run bytecode directly")
    <|> pure True
