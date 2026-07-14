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
    entryContractAbis,
    defaultOptions,
    renderObjects,
    -- Typecheck-cache persistence (Tier 2). The blob functions are pure and
    -- testable; the 'IO' wrappers drive the session cache and marshal to a
    -- Latin-1 string for the JS FFI / IndexedDB.
    indexCheckedByKey,
    dumpStdCacheBlob,
    loadStdCacheBlob,
    dumpStdCache,
    loadStdCache,
  )
where

import Control.Monad.Except (runExceptT)
import Data.ByteString.Lazy qualified as BL
import Data.Char (chr, ord)
import Data.IORef (IORef, modifyIORef', newIORef, readIORef)
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Language.Hull qualified as Hull
import Language.Hull.ToYul.Assemble (objectToYul)
import Solcore.Desugarer.ContractDispatch (contractAbiJson, nameStr)
import Solcore.Frontend.Module.Identity (LibraryId (StdLibrary), ModuleId (moduleLibrary))
import Solcore.Frontend.Module.Identity qualified as Mod
import Solcore.Frontend.Module.Loader (ModuleGraph (..), loadModuleGraphFromSource)
import Solcore.Frontend.Pretty.SolcorePretty (pretty)
import Solcore.Frontend.Syntax.Contract (Contract (name), TopDecl (TContr))
import Solcore.Frontend.TypeInference.TcModule (CheckedModule (..), moduleInferenceLocalDecls)
import Solcore.Pipeline.Options (Option, emptyOption)
import Solcore.Pipeline.SolcorePipeline (compileDiagnosticsText, compileGraphWithCache)
import Solcore.Pipeline.TcCacheSerialize (decodeCache, encodeCache, fromCachedModule, toCachedModule)
import Solcore.Pipeline.TypecheckCache (TcCacheKey, moduleCacheKeys)
import System.IO.Unsafe (unsafePerformIO)

-- | Outcome of an in-memory compilation.
--
--   * 'compileOutput' — pretty-printed Hull objects (the frontend result).
--   * 'compileYul'    — generated Yul (the backend result), when the frontend
--     succeeded and Yul translation didn't fail.
--   * 'compileErrors' — diagnostics from whichever stage failed.
--   * 'compileCacheStatus' — per module (in dependency order), whether its
--     typecheck was reused from the session cache (@True@) or recomputed this
--     compile (@False@). Drives the UI's cache-hit indicator.
--   * 'compileAbis' — the JSON ABI of each contract defined in the entry module,
--     as @(contractName, abiJson)@. Always produced (the browser IDE emits ABIs
--     by default), mirroring the CLI's @--abi@ output without touching disk.
data CompileResult
  = CompileResult
  { compileOutput :: Maybe String,
    compileYul :: Maybe String,
    compileErrors :: [String],
    compileCacheStatus :: [(String, Bool)],
    compileAbis :: [(String, String)]
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
    Left err -> pure (CompileResult Nothing Nothing [err] [] [])
    Right graph ->
      case moduleCacheKeys opts graph of
        Left err -> pure (CompileResult Nothing Nothing [err] [] [])
        Right keys -> do
          seed <- cacheSeed graph keys
          let cacheStatus =
                [ (Mod.moduleIdDisplay moduleId, Map.member moduleId seed)
                | moduleId <- moduleOrder graph
                ]
          compiled <- runExceptT (compileGraphWithCache opts graph seed)
          case compiled of
            Left diags -> pure (CompileResult Nothing Nothing [compileDiagnosticsText diags] cacheStatus [])
            Right (objs, checked) -> do
              cacheCheckedModules keys checked
              let hull = renderObjects objs
                  abis = entryContractAbis graph checked
              yulResult <- objectsToYul objs
              pure $ case yulResult of
                Left err -> CompileResult (Just hull) Nothing [err] cacheStatus abis
                Right yul -> CompileResult (Just hull) (Just yul) [] cacheStatus abis

-- | The JSON ABI of every contract defined in the entry module, as
-- @(contractName, abiJson)@ — the same computation the CLI's @--abi@ performs,
-- reading the field-desugared local declarations the type checker already
-- prepared ('checkedModuleInput'). Safe to force even on a cache hit: the entry
-- module is the user's own source, so it is never one of the std modules served
-- from the reconstructed persisted cache (whose 'checkedModuleInput' is a thunk);
-- a session-cached entry stores the genuine 'CheckedModule'.
entryContractAbis :: ModuleGraph -> Map Mod.ModuleId CheckedModule -> [(String, String)]
entryContractAbis graph checked =
  case Map.lookup (entryModule graph) checked of
    Nothing -> []
    Just cm ->
      [ (nameStr (name c), contractAbiJson c)
      | TContr c <- moduleInferenceLocalDecls (checkedModuleInput cm)
      ]

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
  modifyIORef' tcCache (Map.union (indexCheckedByKey keys checked))

-- | Re-key checked modules by their content hash — the session-cache
-- representation ('TcCacheKey' is content-addressed, so this is what both the
-- in-memory cache and the persisted blob are keyed by).
indexCheckedByKey :: Map Mod.ModuleId TcCacheKey -> Map Mod.ModuleId CheckedModule -> Map TcCacheKey CheckedModule
indexCheckedByKey keys checked =
  Map.fromList
    [ (key, cm)
    | (moduleId, cm) <- Map.toList checked,
      Just key <- [Map.lookup moduleId keys]
    ]

-- | Serialize the std-library subset of a session cache to a blob. Only std
-- modules are persisted: they are the slow part, while the user's own modules
-- are cheap to recheck and change every keystroke. 'encodeCache' prefixes a
-- magic + version header, so a stale or foreign blob is rejected as a clean miss
-- on load rather than misread.
dumpStdCacheBlob :: Map TcCacheKey CheckedModule -> BL.ByteString
dumpStdCacheBlob session =
  encodeCache (Map.map toCachedModule (Map.filter isStd session))
  where
    isStd cm = moduleLibrary (checkedModuleId cm) == StdLibrary

-- | Reconstruct session-cache entries from a persisted blob. A blob that fails
-- the header check decodes to an empty map (clean miss → recompute). The
-- 'Option' only feeds 'initTcEnv' for the reconstructed env; since a non-entry
-- module's env is read solely for its @typeTable@, the choice does not affect
-- results.
loadStdCacheBlob :: Option -> BL.ByteString -> Map TcCacheKey CheckedModule
loadStdCacheBlob opts blob =
  case decodeCache blob of
    Nothing -> Map.empty
    Just cached -> Map.map (fromCachedModule opts) cached

-- | Serialize the std subset of the live session cache as a Latin-1 string (one
-- byte per BMP code point, 0..255) for handing across the JS FFI to IndexedDB.
dumpStdCache :: IO String
dumpStdCache = blobToLatin1 . dumpStdCacheBlob <$> readIORef tcCache

-- | Merge a persisted std cache (Latin-1-encoded blob) into the live session
-- cache, returning the number of modules loaded (0 on a header mismatch or
-- empty blob). Reconstructs with 'defaultOptions' — see 'loadStdCacheBlob'.
loadStdCache :: String -> IO Int
loadStdCache s = do
  let recovered = loadStdCacheBlob defaultOptions (latin1ToBlob s)
  modifyIORef' tcCache (Map.union recovered)
  pure (Map.size recovered)

-- | Byte string ⇄ Latin-1 string: each byte is one code point in 0..255, all
-- below the surrogate range, so 'toJSString'/'fromJSString' round-trip it
-- faithfully.
blobToLatin1 :: BL.ByteString -> String
blobToLatin1 = map (chr . fromIntegral) . BL.unpack

latin1ToBlob :: String -> BL.ByteString
latin1ToBlob = BL.pack . map (fromIntegral . ord)

renderObjects :: [Hull.Object] -> String
renderObjects = unlines . map pretty

-- | Translate all hull objects to Yul, concatenating them. Fails with the first
-- Hull/Yul type error encountered.
objectsToYul :: [Hull.Object] -> IO (Either String String)
objectsToYul objs = do
  results <- mapM objectToYul objs
  pure (intercalate "\n\n" <$> sequence results)
