{-# LANGUAGE JavaScriptFFI #-}

-- | GHCJS FFI boundary: exposes the in-memory solcore compiler to JavaScript.
--
-- On startup it installs @globalThis.compileSolcore(source, flags)@, a
-- synchronous function that takes the source text and a flags object (the UI's
-- checkboxes) and returns @{ ok, output, errors }@.
module Main where

import Data.List (intercalate)
import GHC.JS.Foreign.Callback (Callback, syncCallback2')
import GHC.JS.Prim (JSVal, fromJSString, toJSString)
import Solcore.Api (CompileResult (..), compileSolcore, defaultOptions)
import Solcore.Pipeline.Options (Option (..))

foreign import javascript "((f) => { globalThis.compileSolcore = f; })"
  registerCompile :: Callback (JSVal -> JSVal -> IO JSVal) -> IO ()

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

main :: IO ()
main = syncCallback2' compile >>= registerCompile
