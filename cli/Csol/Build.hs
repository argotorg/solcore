module Csol.Build (runBuild, buildToBytes, compileYul, compileHulls, hullToBytes, selectHull) where

import Control.Monad (forM_, when)
import Data.Aeson (Value, object, (.=), encode, eitherDecodeStrict)
import Data.Aeson.Key qualified as Key
import Data.Aeson.Lens (key, _String)
import Data.ByteString (ByteString)
import Data.ByteString.Base16 qualified as BS16
import Data.ByteString.Lazy qualified as LBS
import Data.List (intercalate)
import Data.Set qualified as Set
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8, encodeUtf8)
import Control.Lens ((^?))
import Pipeline (lower)
import Solcore.Pipeline.SolcorePipeline (compile)
import System.Directory (createDirectoryIfMissing)
import System.Exit (die)
import System.FilePath (dropExtension, takeDirectory)
import System.IO (hClose, hPutStrLn, stderr)
import System.IO.Temp (withSystemTempFile)
import System.Process (readProcess)

import Language.Hull qualified as Hull
import Csol.BuildOpts

-- | Compile Yul source to creation bytecode by calling solc --standard-json.
compileYul :: SolcOpts -> String -> String -> IO ByteString
compileYul solcOpts name src =
  withSystemTempFile "csol.yul" $ \path handle -> do
    hClose handle
    writeFile path src
    let pathText = T.pack path
        stdjson = solcStdJson pathText (optimizerSettings solcOpts)
    output <- T.pack <$> readProcess "solc" ["--allow-paths", path, "--standard-json"] (T.unpack stdjson)
    extractBytecode pathText name output

-- | Build the solc standard JSON input for a Yul file.
solcStdJson :: T.Text -> Maybe Value -> T.Text
solcStdJson path mOptimizer =
  decodeUtf8 $ LBS.toStrict $ encode $
    object [ "language" .= ("Yul" :: T.Text)
           , "sources" .= object
               [ Key.fromText path .= object [ "urls" .= [path] ] ]
           , "settings" .= settingsObj
           ]
  where
    settingsObj = object $ [ "outputSelection" .= object
                               [ "*" .= object
                                   [ "*" .= (["evm.bytecode.object" :: T.Text])
                                   ]
                               ]
                           ] <> maybe [] (\o -> ["optimizer" .= o]) mOptimizer

-- | Build the optimizer JSON value from SolcOpts.
optimizerSettings :: SolcOpts -> Maybe Value
optimizerSettings (SolcOpts False Nothing) = Nothing
optimizerSettings (SolcOpts _ mRuns) =
  Just $ object $ [ "enabled" .= True ]
    <> maybe [] (\n -> ["runs" .= n]) mRuns

-- | Extract creation bytecode from solc --standard-json output.
-- Structure: {"contracts":{"<file>":{"<object>":{"evm":{"bytecode":{"object":"<hex>"}}}}}}
extractBytecode :: T.Text -> String -> T.Text -> IO ByteString
extractBytecode srcPath name output = do
  let bs = encodeUtf8 output
  case eitherDecodeStrict bs of
    Left err -> die $ "compileYul: failed to parse solc output for " <> name <> ": " <> err
    Right (json :: Value) ->
      case json ^? key "contracts" . key (Key.fromText srcPath) . key (Key.fromText (T.pack name))
                 . key "evm" . key "bytecode" . key "object" . _String of
        Just hex -> case BS16.decode (encodeUtf8 hex) of
          Right decoded -> pure decoded
          Left err -> die $ "compileYul: invalid hex in solc output for " <> name <> ": " <> err
        Nothing -> die $ "compileYul: bytecode not found for " <> name
                      <> "\nsolc output: " <> T.unpack (T.take 500 output)

-- | Lower a single hull object to EVM bytecode (hull -> yul -> evm).
hullToBytes :: BuildOpts -> Hull.Object -> IO ByteString
hullToBytes opts hull = do
  (objName, yulText) <- lower (boYule opts) hull
  compileYul (boSolc opts) objName yulText

-- | Select a hull object by name from a list.
-- If no name is given and there is exactly one object, return it.
-- If no name is given and there are multiple objects, error with available names.
-- If a name is given, find the matching object or error.
selectHull :: Maybe String -> [Hull.Object] -> IO Hull.Object
selectHull _ [] = die "no hull objects produced"
selectHull Nothing [h] = pure h
selectHull Nothing hs = die $
  "multiple contracts found: " <> intercalate ", " names
  <> "\nuse --contract to select one"
  where names = map Hull.objName hs
selectHull (Just name) hs =
  case filter ((== name) . Hull.objName) hs of
    [h] -> pure h
    []  -> die $ "contract " <> show name <> " not found; available: "
                <> intercalate ", " (map Hull.objName hs)
    _   -> die $ "multiple contracts named " <> show name

-- | Run the full pipeline: .solc -> Hull -> Yul -> EVM bytecode.
buildToBytes :: BuildOpts -> IO ByteString
buildToBytes opts = do
  hPutStrLn stderr "Compiling to Hull..."
  hulls <- compileHulls opts
  hull <- selectHull (boContract opts) hulls
  hPutStrLn stderr "Lowering to Yul and compiling to EVM bytecode..."
  hullToBytes opts hull

runBuild :: BuildOpts -> IO ()
runBuild opts = do
  allHulls <- compileHulls opts
  hulls <- case boContract opts of
    Nothing -> pure allHulls
    Just _  -> (:[]) <$> selectHull (boContract opts) allHulls
  let yuleOpts = boYule opts
      emit     = boEmit opts
      emitYul  = Set.member EmitYul emit
      emitEvm  = Set.member EmitEvm emit
      total    = length hulls
  when (total > 1 && boOutput opts /= Nothing) $
    die "-o cannot be used with multiple contracts; use --contract to select one"
  forM_ hulls $ \hull -> do
    let base = outputBase opts hull

    when (Set.member EmitHull emit) $ do
      let path = base <> ".hull"
      ensureDir path
      hPutStrLn stderr ("Writing " <> path)
      writeFile path (show hull)

    when (emitYul || emitEvm) $ do
      if emitYul then do
        (name, yulText) <- lower yuleOpts hull
        let path = base <> ".yul"
        ensureDir path
        hPutStrLn stderr ("Writing " <> path)
        writeFile path yulText
        when emitEvm $ do
          evmBytes <- compileYul (boSolc opts) name yulText
          writeEvmFile base evmBytes
      else do
        evmBytes <- hullToBytes opts hull
        writeEvmFile base evmBytes

-- | Shared: compile .solc to Hull objects, exit on failure.
compileHulls :: BuildOpts -> IO [Hull.Object]
compileHulls opts =
  compile (boSolcore opts) >>= \case
    Left err -> die err
    Right hs -> pure hs

-- Determine the output base name (without extension) for a hull object.
outputBase :: BuildOpts -> Hull.Object -> FilePath
outputBase opts hull =
  case boOutput opts of
    Just outPath -> dropExtension outPath
    Nothing      -> Hull.objName hull

writeEvmFile :: FilePath -> ByteString -> IO ()
writeEvmFile base evmBytes = do
  let path = base <> ".evm"
      hex = decodeUtf8 (BS16.encode evmBytes)
  ensureDir path
  hPutStrLn stderr ("Writing " <> path)
  writeFile path (T.unpack hex)

ensureDir :: FilePath -> IO ()
ensureDir path =
  let dir = takeDirectory path
  in when (not (null dir) && dir /= ".") $ createDirectoryIfMissing True dir
