-- | In-memory compilation entry point.
--
-- This is the seam the web IDE talks to: source text in (from an editor
-- buffer), structured result out (compiler output or diagnostics), with no
-- file-system access. It reuses the whole existing pipeline via
-- 'compileGraphWithCache', differing from the CLI only in how the module graph
-- is obtained — and in that it keeps a session-level typecheck cache so
-- repeated compiles (the edit-recompile loop) reuse unchanged modules.
module Solcore.Api
  ( CompileResult (..),
    compileSolcore,
    defaultOptions,
  )
where

import Control.Monad.Except (runExceptT)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Language.Hull qualified as Hull
import Language.Hull.ToYul.Assemble (objectToYul)
import Solcore.Frontend.Module.Identity qualified as Mod
import Solcore.Frontend.Module.Loader (ModuleGraph (..), loadModuleGraphFromSource)
import Solcore.Frontend.Pretty.SolcorePretty (pretty)
import Solcore.Frontend.TypeInference.TcModule (CheckedModule)
import Solcore.Pipeline.Options (Option, emptyOption)
import Solcore.Pipeline.SolcorePipeline (compileGraphWithCache)
import Solcore.Pipeline.TypecheckCache (TcCacheKey, moduleCacheKeys)
import System.IO.Unsafe (unsafePerformIO)

-- | Outcome of an in-memory compilation.
--
--   * 'compileOutput' — pretty-printed Hull objects (the frontend result).
--   * 'compileYul'    — generated Yul (the backend result), when the frontend
--     succeeded and Yul translation didn't fail.
--   * 'compileErrors' — diagnostics from whichever stage failed.
data CompileResult
  = CompileResult
  { compileOutput :: Maybe String,
    compileYul :: Maybe String,
    compileErrors :: [String]
  }
  deriving (Eq, Show)

-- | Session-level typecheck cache, keyed by content hash ('TcCacheKey'). The
-- GHCJS worker loads the compiler once and calls 'compileSolcore' repeatedly,
-- so this top-level ref persists across compiles within a session: the std
-- modules typechecked on the first compile are reused on every subsequent one,
-- while an edited module (whose key changes) is recomputed. Not persisted
-- across page reloads — that is a later tier (IndexedDB / embedded dump).
{-# NOINLINE tcCache #-}
tcCache :: IORef (Map TcCacheKey CheckedModule)
tcCache = unsafePerformIO (newIORef Map.empty)

-- | Baseline options for the in-memory path. The file-system fields of
-- 'Option' (roots, output dir) are unused here; the UI is expected to override
-- the boolean pass toggles (checkboxes) and the partial-evaluation fuel (an
-- input field) on top of this.
defaultOptions :: Option
defaultOptions = emptyOption "Main.solc"

-- | Compile a single solcore source module in memory. 'Option' is supplied by
-- the caller (the UI), so pass toggles and fuel are driven from the frontend.
compileSolcore :: Option -> String -> IO CompileResult
compileSolcore opts source = do
  graphResult <- loadModuleGraphFromSource source
  case graphResult of
    Left err -> pure (CompileResult Nothing Nothing [err])
    Right graph ->
      case moduleCacheKeys opts graph of
        Left err -> pure (CompileResult Nothing Nothing [err])
        Right keys -> do
          seed <- cacheSeed graph keys
          compiled <- runExceptT (compileGraphWithCache opts graph seed)
          case compiled of
            Left err -> pure (CompileResult Nothing Nothing [err])
            Right (objs, checked) -> do
              cacheCheckedModules keys checked
              let hull = renderObjects objs
              yulResult <- objectsToYul objs
              pure $ case yulResult of
                Left err -> CompileResult (Just hull) Nothing [err]
                Right yul -> CompileResult (Just hull) (Just yul) []

-- | Build the per-compile reuse map: every module whose current key is already
-- in the session cache maps to its stored 'CheckedModule'. Modules whose source
-- (or an imported interface, or a relevant flag) changed have a new key and are
-- absent here, so the pipeline re-typechecks them.
cacheSeed ::
  ModuleGraph ->
  Map Mod.ModuleId TcCacheKey ->
  IO (Map Mod.ModuleId CheckedModule)
cacheSeed graph keys = do
  store <- readIORef tcCache
  pure $
    Map.fromList
      [ (moduleId, cached)
      | moduleId <- moduleOrder graph,
        Just key <- [Map.lookup moduleId keys],
        Just cached <- [Map.lookup key store]
      ]

-- | Index freshly-checked modules by their cache key and merge them into the
-- session cache, so the next compile can reuse the ones whose key is unchanged.
cacheCheckedModules :: Map Mod.ModuleId TcCacheKey -> Map Mod.ModuleId CheckedModule -> IO ()
cacheCheckedModules keys checked =
  modifyIORef' tcCache (Map.union additions)
  where
    additions =
      Map.fromList
        [ (key, cm)
        | (moduleId, cm) <- Map.toList checked,
          Just key <- [Map.lookup moduleId keys]
        ]

renderObjects :: [Hull.Object] -> String
renderObjects = unlines . map pretty

-- | Translate all hull objects to Yul, concatenating them. Fails with the first
-- Hull/Yul type error encountered.
objectsToYul :: [Hull.Object] -> IO (Either String String)
objectsToYul objs = do
  results <- mapM objectToYul objs
  pure (intercalate "\n\n" <$> sequence results)
