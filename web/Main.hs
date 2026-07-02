{-# LANGUAGE JavaScriptFFI #-}

-- | GHCJS FFI boundary: exposes the in-memory solcore compiler to JavaScript.
--
-- On startup it installs, on @globalThis@:
--
--   * @compileSolcore(source, flags)@ — a synchronous compile returning
--     @{ ok, output, yul, errors }@.
--   * @solcoreDumpStdCache()@ / @solcoreLoadStdCache(blob)@ — the typecheck
--     cache's persistence hooks. The blob is a Latin-1 string (one byte per
--     char) so it stores directly in IndexedDB; @worker.js@ loads it on startup
--     and writes it back after the first successful compile.
module Main where

import Data.List (intercalate)
import GHC.JS.Foreign.Callback (Callback, syncCallback', syncCallback1', syncCallback2')
import GHC.JS.Prim (JSVal, fromJSString, toJSString)
import Solcore.Api (CompileResult (..), compileSolcore, defaultOptions, dumpStdCache, loadStdCache)
import Solcore.Pipeline.Options (Option (..))

foreign import javascript "((f) => { globalThis.compileSolcore = f; })"
  registerCompile :: Callback (JSVal -> JSVal -> IO JSVal) -> IO ()

foreign import javascript "((f) => { globalThis.solcoreDumpStdCache = f; })"
  registerDumpStdCache :: Callback (IO JSVal) -> IO ()

foreign import javascript "((f) => { globalThis.solcoreLoadStdCache = f; })"
  registerLoadStdCache :: Callback (JSVal -> IO JSVal) -> IO ()

-- | Marshal a Haskell 'Int' to a JS number (identity across the FFI boundary).
foreign import javascript "((n) => n)"
  js_int :: Int -> JSVal

-- | Read a boolean property from a JS object (missing / falsy => False).
foreign import javascript "((obj, key) => (obj && obj[key] ? 1 : 0))"
  js_boolField :: JSVal -> JSVal -> IO Int

-- | Build the JS result object returned to the caller.
foreign import javascript "((ok, output, yul, errors) => ({ ok: ok !== 0, output: output, yul: yul, errors: errors }))"
  js_result :: Int -> JSVal -> JSVal -> JSVal -> IO JSVal

boolField :: JSVal -> String -> IO Bool
boolField obj key = (/= 0) <$> js_boolField obj (toJSString key)

-- | Map the flags object (UI checkboxes) onto compiler options.
optionsFromFlags :: JSVal -> IO Option
optionsFromFlags flags = do
  noGenDispatch <- boolField flags "noGenDispatch"
  noSpec <- boolField flags "noSpec"
  noMatchCompiler <- boolField flags "noMatchCompiler"
  noIfDesugar <- boolField flags "noIfDesugar"
  noDesugarCalls <- boolField flags "noDesugarCalls"
  pure
    defaultOptions
      { optNoGenDispatch = noGenDispatch,
        optNoSpec = noSpec,
        optNoMatchCompiler = noMatchCompiler,
        optNoIfDesugar = noIfDesugar,
        optNoDesugarCalls = noDesugarCalls
      }

compile :: JSVal -> JSVal -> IO JSVal
compile sourceVal flagsVal = do
  opts <- optionsFromFlags flagsVal
  result <- compileSolcore opts (fromJSString sourceVal)
  case result of
    CompileResult (Just output) yul _ ->
      js_result 1 (toJSString output) (toJSString (maybe "" id yul)) (toJSString "")
    CompileResult Nothing _ errors ->
      js_result 0 (toJSString "") (toJSString "") (toJSString (intercalate "\n\n" errors))

-- | Dump the persisted (std) typecheck cache as a Latin-1 string.
dumpCache :: IO JSVal
dumpCache = toJSString <$> dumpStdCache

-- | Merge a persisted cache blob into the session, returning the count loaded.
loadCache :: JSVal -> IO JSVal
loadCache blob = js_int <$> loadStdCache (fromJSString blob)

main :: IO ()
main = do
  -- Register the cache hooks before the compile entry point, so that once
  -- @compileSolcore@ is visible (what worker.js polls on) all three exist.
  syncCallback' dumpCache >>= registerDumpStdCache
  syncCallback1' loadCache >>= registerLoadStdCache
  syncCallback2' compile >>= registerCompile
