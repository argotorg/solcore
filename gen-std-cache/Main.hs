-- | Build tool: write the precompiled std typecheck-cache blob to a file.
--
-- The browser IDE loads this blob on its first run (before IndexedDB is
-- populated) so it reuses std instead of retypechecking it. It is generated
-- natively — deterministic and free of the JS backend's synchronous-callback /
-- async-fs pitfalls — and is byte-identical to a blob dumped by the JS build,
-- because the content-hash cache keys and the serialized AST are toolchain
-- independent (pure keccak over a deterministic 'show').
module Main where

import Control.Monad.Except (runExceptT)
import Data.ByteString.Lazy qualified as BL
import Data.Map qualified as Map
import Solcore.Api (defaultOptions, dumpStdCacheBlob, indexCheckedByKey)
import Solcore.Frontend.Module.Loader (loadModuleGraphFromSource)
import Solcore.Pipeline.SolcorePipeline (compileGraphWithCache)
import Solcore.Pipeline.TypecheckCache (moduleCacheKeys)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

-- Importing std and std.dispatch pulls the whole std closure (dispatch imports
-- opcodes); the trivial contract makes it a well-formed compile.
warmup :: String
warmup =
  "import std.{*};\n"
    ++ "import std.dispatch.{*};\n"
    ++ "contract W { constructor() {} }\n"

main :: IO ()
main = do
  args <- getArgs
  case args of
    [out] -> generate out
    _ -> hPutStrLn stderr "usage: gen-std-cache <out.bin>" >> exitFailure

generate :: FilePath -> IO ()
generate out = do
  graphE <- loadModuleGraphFromSource warmup
  case graphE >>= \graph -> (,) graph <$> moduleCacheKeys defaultOptions graph of
    Left err -> die ("gen-std-cache: " ++ err)
    Right (graph, keys) -> do
      res <- runExceptT (compileGraphWithCache defaultOptions graph Map.empty)
      case res of
        Left err -> die ("gen-std-cache: warm-up compile failed: " ++ err)
        Right (_, checked) -> do
          let blob = dumpStdCacheBlob (indexCheckedByKey keys checked)
          BL.writeFile out blob
          putStrLn ("gen-std-cache: wrote " ++ out ++ " (" ++ show (BL.length blob) ++ " bytes)")

die :: String -> IO ()
die msg = hPutStrLn stderr msg >> exitFailure
