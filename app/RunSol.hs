{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuasiQuotes #-}
module Main where

import Control.Monad (when, unless)
import Control.Applicative ((<|>))
import Data.List (isPrefixOf)
import Data.Maybe (isJust, fromMaybe, mapMaybe)
import System.Exit (exitFailure, ExitCode(..))
import System.FilePath (takeBaseName, dropExtension, (<.>), (</>))
import System.Process (readProcessWithExitCode)
import System.Directory (createDirectoryIfMissing)
import Options.Applicative
import qualified Data.Aeson as JSON
import Data.Aeson (Object, Value(..), (.:), decode, fromJSON, Result(..))
import qualified Data.ByteString.Lazy as BL
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as B8
import qualified Data.Text as T
import qualified Data.Aeson.KeyMap as KM
import Data.Aeson.Key (fromString)

import Solcore.Pipeline.SolcorePipeline (compile)
import Solcore.Pipeline.Options (Option(..))
import qualified Language.Core as Core
import Yule.Translate (translateObject)
import Yule.TM (runTM)
import qualified Yule.Options as YuleOpts
import Common.Pretty (render, ppr)
import Language.Yul (YulObject(..), YulCode(..))
import Language.Yul.QuasiQuote

-- ============================================================================
-- Data Types
-- ============================================================================

data RunSolOptions = RunSolOptions
  { -- Input file
    inputFile :: FilePath
  , buildDir :: FilePath
  -- Sol-core compilation options
  , importDirs :: String
  , noSpecialise :: Bool
  , noDesugarCalls :: Bool
  , noMatchCompiler :: Bool
  , noIfDesugar :: Bool
  , noGenDispatch :: Bool
  -- Output/debugging options
  , verbose :: Bool
  , dumpAST :: Bool
  , dumpEnv :: Bool
  , dumpDispatch :: Bool
  , dumpDS :: Bool
  , dumpDF :: Bool
  , dumpSpec :: Bool
  , dumpCore :: Bool
  , debugSpec :: Bool
  , debugCore :: Bool
  , timing :: Bool
  -- Execution options
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
-- JSONL Parsing Helpers
-- ============================================================================

-- Parse JSONL output from evm and extract output/error from last object
extractEVMResult :: String -> (String, Maybe String)
extractEVMResult output =
  case lastJsonLine (lines output) of
    Nothing -> ("", Nothing)
    Just (outputVal, errorVal) -> (outputVal, errorVal)

-- Get the last valid JSON object and extract output/error fields
lastJsonLine :: [String] -> Maybe (String, Maybe String)
lastJsonLine lns =
  case mapMaybe parseJsonLine (reverse lns) of
    [] -> Nothing
    (result:_) -> Just result

-- Parse a single line as JSON object and extract output/error
parseJsonLine :: String -> Maybe (String, Maybe String)
parseJsonLine line =
  case decode (BL.fromStrict (B8.pack line)) :: Maybe Value of
    Just (Object obj) -> Just (getOutput obj, getError obj)
    _ -> Nothing

-- Extract output field from JSON object
getOutput :: Object -> String
getOutput obj =
  case KM.lookup (fromString "output") obj of
    Just (String s) -> T.unpack s
    _ -> ""

-- Extract error field from JSON object as a string
-- Returns Nothing if error is null, otherwise returns the error string
getError :: Object -> Maybe String
getError obj =
  case KM.lookup (fromString "error") obj of
    Just Null -> Nothing
    Just (String s) -> Just (T.unpack s)
    Just v -> Just (T.unpack $ renderJSON v)
    Nothing -> Nothing

-- Simple JSON rendering for non-string values
renderJSON :: Value -> T.Text
renderJSON Null = "null"
renderJSON (Bool b) = if b then "true" else "false"
renderJSON (Number n) = T.pack (show n)
renderJSON (Array _) = "array"
renderJSON (Object _) = "object"
renderJSON (String s) = s

-- ============================================================================
-- Yul Wrapping Helpers
-- ============================================================================

-- Add return code to Yul object to ensure result is returned
addRetCode :: YulCode -> YulCode
addRetCode c = c <> retCode where
    retCode = YulCode [yulBlock|
    {
      mstore(0, _mainresult)
      return(0, 32)
    }
    |]

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
  -- Sol-core compilation options
  <*> strOption
      (long "include"
      <> short 'i'
      <> metavar "DIRS"
      <> value "std"
      <> showDefault
      <> help "Colon-separated list of import directories")
  <*> switch
      (long "no-specialise"
      <> short 'n'
      <> help "Skip specialisation and core emission phases")
  <*> switch
      (long "no-desugar-calls"
      <> short 's'
      <> help "Skip indirect call desugaring")
  <*> switch
      (long "no-match-compiler"
      <> short 'm'
      <> help "Skip match compilation")
  <*> switch
      (long "no-if-desugar"
      <> short 'd'
      <> help "Skip if / bool desugaring")
  <*> switch
      (long "no-gen-dispatch"
      <> short 'g'
      <> help "Skip contract dispatch generation")
  -- Output/debugging options
  <*> switch
      (long "verbose"
      <> short 'v'
      <> help "Verbose output")
  <*> switch
      (long "dump-ast"
      <> help "Dump AST after name resolution")
  <*> switch
      (long "dump-env"
      <> help "Dump env after name resolution")
  <*> switch
      (long "dump-dispatch"
      <> help "Dump dispatched contract")
  <*> switch
      (long "dump-ds"
      <> help "Dump desugared contract")
  <*> switch
      (long "dump-df"
      <> help "Dump defunctionalised contract")
  <*> switch
      (long "dump-spec"
      <> help "Dump specialised contract")
  <*> switch
      (long "dump-core"
      <> help "Dump low-level core")
  <*> switch
      (long "debug-spec"
      <> help "Debug specialisation")
  <*> switch
      (long "debug-core"
      <> help "Debug core emission")
  <*> switch
      (long "timing"
      <> help "Measure time of some phases")
  -- Execution options
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
compileSolcoreToCore :: RunSolOptions -> IO [Core.Object]
compileSolcoreToCore runOpts = do
  let opts = Option
        { fileName = inputFile runOpts
        , optImportDirs = importDirs runOpts
        , optNoSpec = noSpecialise runOpts
        , optNoDesugarCalls = noDesugarCalls runOpts
        , optNoMatchCompiler = noMatchCompiler runOpts
        , optNoIfDesugar = noIfDesugar runOpts
        , optNoGenDispatch = noGenDispatch runOpts
        , optVerbose = verbose runOpts
        , optDumpAST = dumpAST runOpts
        , optDumpEnv = dumpEnv runOpts
        , optDumpDispatch = dumpDispatch runOpts
        , optDumpDS = dumpDS runOpts
        , optDumpDF = dumpDF runOpts
        , optDumpSpec = dumpSpec runOpts
        , optDumpCore = dumpCore runOpts
        , optDebugSpec = debugSpec runOpts
        , optDebugCore = debugCore runOpts
        , optTiming = timing runOpts
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
  -- Wrap Yul object with return code to ensure result is returned
  let (YulObject name code inners) = yulObj
  let wrappedYul = YulObject name (addRetCode code) inners
  let yulSource = render (ppr wrappedYul)
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

  -- evm writes to stderr by default, so we need to use that
  -- Combine both stdout and stderr to handle both cases
  let output = if null stderr then stdout else stderr

  when (debugCreate opts) $
    putStrLn $ "Create output: " ++ output

  -- Save trace
  writeFile traceFile output

  -- Extract actual return data from JSONL output
  let (returnData, evmError) = extractEVMResult output
  -- Check if there was an error in EVM execution (evmError will be Nothing if error field was null)
  let errorMsg = evmError <|> (case exitCode of
                                 ExitFailure code -> Just $ "EVM process failed with exit code " ++ show code
                                 ExitSuccess -> Nothing)

  return (returnData, errorMsg)

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

  -- evm writes to stderr by default, so we need to use that
  -- Combine both stdout and stderr to handle both cases
  let output = if null stderr then stdout else stderr

  when (debugRuntime opts) $
    putStrLn $ "Runtime output: " ++ output

  -- Save trace
  writeFile traceFile output

  -- Extract actual return data from JSONL output
  let (returnData, evmError) = extractEVMResult output

  -- Debug output
  when (debugRuntime opts) $ do
    putStrLn $ "DEBUG: Raw output length: " ++ show (length output)
    putStrLn $ "DEBUG: Extracted returnData: " ++ show returnData
    putStrLn $ "DEBUG: Extracted error: " ++ show evmError
    putStrLn $ "DEBUG: First 500 chars: " ++ take 500 output

  -- For runtime, preserve execution errors from EVM
  let errorMsg = evmError <|> (case exitCode of
                                 ExitFailure code -> Just $ "EVM process failed with exit code " ++ show code
                                 ExitSuccess -> Nothing)

  return (returnData, errorMsg)

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
  coreObjs <- compileSolcoreToCore opts

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
            let hexData = if "0x" `isPrefixOf` runtimeResult then runtimeResult else "0x" ++ runtimeResult
            decoded <- castAbiDecode sig hexData
            putStrLn $ "Decoded output: " ++ decoded
        Nothing -> do
          unless (null runtimeResult) $ do
            let hexData = if "0x" `isPrefixOf` runtimeResult then runtimeResult else "0x" ++ runtimeResult
            putStrLn $ "returndata: " ++ hexData
    Just err -> do
      putStrLn $ "Execution failed: " ++ err
      unless (null runtimeResult) $ do
        let hexData = if "0x" `isPrefixOf` runtimeResult then runtimeResult else "0x" ++ runtimeResult
        putStrLn $ "returndata: " ++ hexData
      exitFailure
