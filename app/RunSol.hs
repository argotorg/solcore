{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Control.Monad (when, unless)
import Data.List (isPrefixOf)
import Data.Maybe (fromMaybe, isJust)
import System.Exit (exitFailure, exitSuccess, ExitCode(..))
import System.FilePath (takeDirectory, takeBaseName, dropExtension, (</>), (<.>))
import System.Process (readProcessWithExitCode, callProcess)
import System.Directory (createDirectoryIfMissing, listDirectory, removeFile)
import qualified Data.ByteString.Lazy as BL
import qualified Data.Aeson as A
import Options.Applicative
import Text.Read (readMaybe)

import Solcore.Pipeline.SolcorePipeline (compile)
import Solcore.Pipeline.Options (Option(..))
import qualified Language.Core as Core
import Yule.Translate (translateObject)
import Yule.TM (runTM, CEnv)
import Yule.Compress (compress)
import qualified Yule.Options as YuleOpts
import Common.Pretty (render, ppr)
import Language.Yul (YulObject(..))

-- ============================================================================
-- Data Types
-- ============================================================================

data RunSolOptions = RunSolOptions
  { inputFile :: FilePath
  , buildDir :: FilePath
  , runtimeCalldataSig :: Maybe String
  , runtimeCalldataArgs :: [String]
  , runtimeRawCalldata :: Maybe String
  , runtimeCallvalue :: Maybe String
  , debugRuntime :: Bool
  , shouldCreate :: Bool
  , createArgumentsSig :: Maybe String
  , createArgs :: [String]
  , createRawArgs :: Maybe String
  , createCallvalue :: Maybe String
  , debugCreate :: Bool
  } deriving (Show, Eq)

data EVMResult = EVMResult
  { evmOutput :: String
  , evmError :: Maybe String
  , evmExitCode :: ExitCode
  } deriving (Show)

-- ============================================================================
-- Command-Line Argument Parser
-- ============================================================================

optionsParser :: Parser RunSolOptions
optionsParser = RunSolOptions
  <$> argument str
      (metavar "FILE" <> help "Input .solc file")
  <*> strOption
      (long "build-dir"
      <> value "build"
      <> showDefault
      <> help "Build directory")
  <*> optional (strOption
      (long "runtime-calldata"
      <> metavar "SIG"
      <> help "Runtime function signature for calldata generation"))
  <*> many (argument str (metavar "ARGS..." <> help "Runtime calldata arguments"))
  <*> optional (strOption
      (long "runtime-raw-calldata"
      <> metavar "HEX"
      <> help "Raw hex calldata for runtime"))
  <*> optional (strOption
      (long "runtime-callvalue"
      <> metavar "VALUE"
      <> help "Callvalue for runtime execution (in wei)"))
  <*> switch
      (long "debug-runtime"
      <> help "Debug runtime execution")
  <*> flag True False
      (long "no-create"
      <> help "Skip contract creation phase")
  <*> optional (strOption
      (long "create-arguments"
      <> metavar "SIG"
      <> help "Constructor signature for calldata generation"))
  <*> many (argument str (metavar "ARGS..." <> help "Create calldata arguments"))
  <*> optional (strOption
      (long "create-raw-arguments"
      <> metavar "HEX"
      <> help "Raw hex calldata for constructor"))
  <*> optional (strOption
      (long "create-callvalue"
      <> metavar "VALUE"
      <> help "Callvalue for create execution (in wei)"))
  <*> switch
      (long "debug-create"
      <> help "Debug create execution")

parseOptions :: IO RunSolOptions
parseOptions = execParser opts
  where
    opts = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Solcore pipeline runner"
     <> header "runsol - Run solc compiler with evm execution" )

-- ============================================================================
-- Compilation Functions
-- ============================================================================

-- Compile .solc to Core using the pipeline
compileSolcoreToCore :: FilePath -> IO [Core.Object]
compileSolcoreToCore file = do
  let opts = Option
        { fileName = file
        , optImportDirs = "std"
        , optNoSpec = False
        , optNoDesugarCalls = False
        , optNoMatchCompiler = False
        , optNoIfDesugar = False
        , optNoGenDispatch = False
        , optVerbose = False
        , optDumpAST = False
        , optDumpEnv = False
        , optDumpDispatch = False
        , optDumpDS = False
        , optDumpDF = False
        , optDumpSpec = False
        , optDumpCore = False
        , optDebugSpec = False
        , optDebugCore = False
        , optTiming = False
        }
  result <- compile opts
  case result of
    Left err -> do
      putStrLn $ "Error during compilation: " ++ err
      exitFailure
    Right objs -> return objs

-- Translate Core to Yul using integrated yule
translateCoreToYul :: Core.Object -> IO YulObject
translateCoreToYul coreObj = do
  let yuleOpts = YuleOpts.Options
        { YuleOpts.input = ""
        , YuleOpts.contract = "Output"
        , YuleOpts.output = ""
        , YuleOpts.verbose = False
        , YuleOpts.debug = False
        , YuleOpts.compress = False
        , YuleOpts.wrap = False
        , YuleOpts.runOnce = False
        }
  result <- runTM yuleOpts (translateObject coreObj)
  return result

-- Compile Yul to bytecode using solc
compileToBytecode :: FilePath -> YulObject -> IO String
compileToBytecode outputFile yulObj = do
  let yulSource = render (ppr yulObj)
  -- Write Yul to temporary file
  let yulFile = dropExtension outputFile <.> "yul"
  writeFile yulFile yulSource
  -- Call solc
  (exitCode, stdout, stderr) <- readProcessWithExitCode
    "solc"
    ["--strict-assembly", "--bin", "--optimize", yulFile]
    ""
  case exitCode of
    ExitSuccess -> do
      let bytecode = last (lines stdout)
      putStrLn $ "Hex output: " ++ outputFile
      writeFile outputFile bytecode
      return bytecode
    ExitFailure code -> do
      putStrLn $ "Error: solc compilation failed with code " ++ show code
      putStrLn $ "stderr: " ++ stderr
      exitFailure

-- ============================================================================
-- Process Execution Helpers
-- ============================================================================

-- Call cast to encode calldata
castCalldataEncode :: String -> [String] -> IO String
castCalldataEncode sig args = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode
    "cast"
    ("calldata" : sig : args)
    ""
  case exitCode of
    ExitSuccess -> return (head (lines stdout))
    ExitFailure code -> do
      putStrLn $ "Error: cast calldata failed with code " ++ show code
      putStrLn $ "stderr: " ++ stderr
      exitFailure

-- Call cast to decode output
castAbiDecode :: String -> String -> IO String
castAbiDecode sig hexOutput = do
  (exitCode, stdout, stderr) <- readProcessWithExitCode
    "cast"
    ["abi-decode", sig, hexOutput]
    ""
  case exitCode of
    ExitSuccess -> return (unlines (lines stdout))
    ExitFailure code -> do
      putStrLn $ "Error: cast abi-decode failed with code " ++ show code
      putStrLn $ "stderr: " ++ stderr
      exitFailure

-- Build EVM command and execute create phase
executeCreate :: RunSolOptions -> String -> FilePath -> IO (String, Maybe String)
executeCreate opts bytecode buildDir = do
  putStrLn "Executing create phase..."
  let traceFile = buildDir </> "trace.create.jsonl"
  let poststateFile = buildDir </> "create.poststate.json"

  -- Prepare bytecode with constructor args if provided
  let hexFileWithArgs = if isJust (createArgumentsSig opts)
        then case createArgumentsSig opts of
          Just sig -> do
            args <- castCalldataEncode sig (createArgs opts)
            return $ bytecode ++ drop 2 args  -- drop "0x" prefix
          Nothing -> return bytecode
        else if isJust (createRawArgs opts)
          then return $ bytecode ++ drop 2 (fromMaybe "" (createRawArgs opts))
          else return bytecode

  bytecodeWithArgs <- hexFileWithArgs

  -- Build evm command
  let evmCmd = ["evm", "run"]
              ++ ["--trace", "--trace.nomemory=false", "--trace.noreturndata=false"]
              ++ ["--create", "--dump", "--codefile", "-"]
              ++ (case createCallvalue opts of
                    Just v -> ["--value", v]
                    Nothing -> [])

  -- Execute evm
  (exitCode, stdout, stderr) <- readProcessWithExitCode
    "evm"
    (tail evmCmd)  -- drop "evm" command
    bytecodeWithArgs

  let output = if exitCode == ExitSuccess then stdout else stderr

  -- Extract post-state JSON from output
  let stateLines = dropWhile (not . ("{" `isPrefixOf`)) (lines output)

  when (debugCreate opts) $
    putStrLn $ "Create output: " ++ output

  let result = if null stateLines then "" else unlines stateLines
  let error = if exitCode == ExitSuccess then Nothing else Just result

  -- Save trace and post-state
  writeFile traceFile output

  return (result, error)

-- Build EVM command and execute runtime phase
executeRuntime :: RunSolOptions -> String -> FilePath -> IO (String, Maybe String)
executeRuntime opts bytecode buildDir = do
  putStrLn "Executing runtime phase..."
  let traceFile = buildDir </> "trace.runtime.jsonl"

  -- Prepare runtime calldata if provided
  let inputOpt = case runtimeCalldataSig opts of
        Just sig -> do
          calldata <- castCalldataEncode sig (runtimeCalldataArgs opts)
          return ["--input", calldata]
        Nothing -> case runtimeRawCalldata opts of
          Just hex -> return ["--input", hex]
          Nothing -> return []

  inputArgs <- inputOpt

  -- Build evm command
  let evmCmd = ["evm", "run"]
              ++ ["--trace", "--trace.nomemory=false", "--trace.noreturndata=false"]
              ++ ["--codefile", "-"]
              ++ inputArgs
              ++ (case runtimeCallvalue opts of
                    Just v -> ["--value", v]
                    Nothing -> [])

  -- Execute evm
  (exitCode, stdout, stderr) <- readProcessWithExitCode
    "evm"
    (tail evmCmd)  -- drop "evm" command
    bytecode

  let output = if exitCode == ExitSuccess then stdout else stderr

  when (debugRuntime opts) $
    putStrLn $ "Runtime output: " ++ output

  -- Save trace
  writeFile traceFile output

  let result = if exitCode == ExitSuccess then stdout else stderr
  let error = if exitCode == ExitSuccess then Nothing else Just result

  return (result, error)

-- ============================================================================
-- Main Function
-- ============================================================================

main :: IO ()
main = do
  opts <- parseOptions

  -- Create build directory
  createDirectoryIfMissing True (buildDir opts)

  putStrLn $ "Processing: " ++ inputFile opts

  -- Step 1: Compile .solc to Core
  putStrLn "Compiling to core..."
  coreObjs <- compileSolcoreToCore (inputFile opts)

  when (null coreObjs) $ do
    putStrLn "Error: No core objects generated"
    exitFailure

  let coreObj = head coreObjs

  -- Step 2: Translate Core to Yul
  putStrLn "Generating Yul..."
  yulObj <- translateCoreToYul coreObj

  -- Step 3: Compile Yul to bytecode
  putStrLn "Compiling to bytecode..."
  let base = dropExtension (takeBaseName (inputFile opts))
  let hexFile = buildDir opts </> base <.> "hex"
  bytecode <- compileToBytecode hexFile yulObj

  -- Step 4: Execute create phase (if enabled)
  when (shouldCreate opts) $ do
    (createResult, createError) <- executeCreate opts bytecode (buildDir opts)

    case createError of
      Nothing -> do
        putStrLn "Creation successful"
        unless (null createResult) $
          putStrLn $ "returndata: 0x" ++ createResult
      Just err -> do
        putStrLn $ "Creation failed: " ++ err
        unless (null createResult) $
          putStrLn $ "returndata: 0x" ++ createResult

  -- Step 5: Execute runtime phase
  (runtimeResult, runtimeError) <- executeRuntime opts bytecode (buildDir opts)

  case runtimeError of
    Nothing -> do
      putStrLn "Execution successful"
      case runtimeCalldataSig opts of
        Just sig -> do
          when (not (null runtimeResult)) $ do
            decoded <- castAbiDecode sig ("0x" ++ runtimeResult)
            putStrLn $ "Decoded output: " ++ decoded
        Nothing -> do
          unless (null runtimeResult) $
            putStrLn $ "returndata: 0x" ++ runtimeResult
    Just err -> do
      putStrLn $ "Execution failed: " ++ err
      unless (null runtimeResult) $
        putStrLn $ "returndata: 0x" ++ runtimeResult
      exitFailure
