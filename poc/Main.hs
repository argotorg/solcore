-- | PoC harness for the typecheck cache.
--
-- Phase 1 (caching seam): reusing a previously-computed 'CheckedModule' instead
-- of re-typechecking yields byte-identical downstream output, and saves the
-- typecheck time. No serialization yet — the cache is an in-process 'Map'.
--
-- Phase 2 (Merkle keying): a content-addressed key per module drives cache
-- reuse. We check keying determinism, the precise-invalidation property (editing
-- a module invalidates exactly it plus its transitive dependents), and use the
-- keys to select which modules to reuse when the entry module is "edited".
module Main where

import Control.Monad (forM_, unless)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.IO.Class (liftIO)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BL
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (isNothing)
import Data.Set qualified as Set
import Language.Hull qualified as Hull
import Solcore.Frontend.Module.Identity qualified as Mod
import Solcore.Frontend.Module.Loader (LoadedModule (..), ModuleGraph (..), loadModuleGraph)
import Solcore.Frontend.TypeInference.TcModule (CheckedModule)
import Solcore.Pipeline.Options (Option (..), emptyOption)
import Solcore.Pipeline.SolcorePipeline (compileDiagnosticsText, compileGraphWithCache, parseExternalLibSpecs, parseStdRoot)
import Solcore.Pipeline.TcCacheSerialize (decodeCache, encodeCache, fromCachedModule, toCachedModule)
import Solcore.Pipeline.TypecheckCache
  ( TcCacheKey (..),
    moduleCacheKeys,
    moduleCacheKeysWith,
    moduleHasContracts,
    transitiveDependents,
  )
import System.Directory (getTemporaryDirectory, makeAbsolute)
import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.FilePath ((</>))
import System.TimeIt (timeItT)
import Text.Printf (printf)

main :: IO ()
main = do
  args <- getArgs
  let file = case args of
        (f : _) -> f
        [] -> "test/examples/dispatch/counter.solc"
      opts = (emptyOption file) {optTiming = True}
  putStrLn ("PoC file: " ++ file)
  graph <- loadGraph opts file
  let order = moduleOrder graph
      entry = entryModule graph
      display = Mod.moduleIdDisplay
      hasContracts m = moduleHasContracts (loadedCompUnit (modules graph Map.! m))
  printf "modules in graph: %d (entry: %s)\n" (length order) (display entry)

  keys <- orDie (moduleCacheKeys opts graph)

  -- Cache keys ----------------------------------------------------------------
  section "cache keys (Merkle: source + reference keys + flags)"
  forM_ order $ \m ->
    printf "  %-42s key=%s contracts=%s\n" (display m) (shortKey (keys Map.! m)) (show (hasContracts m))

  -- Keying is deterministic across a fresh load of the same sources.
  graph2 <- loadGraph opts file
  keys2 <- orDie (moduleCacheKeys opts graph2)
  assert "keys are stable across a reload" (keys == keys2)

  -- Precise invalidation: editing a module changes the key of exactly that
  -- module plus every module that (transitively) references it.
  section "invalidation property (edit M -> which modules must be rechecked)"
  forM_ order $ \m -> do
    edited <- orDie (moduleCacheKeysWith (bump m) opts graph)
    let changed = Set.fromList [x | x <- order, Map.lookup x keys /= Map.lookup x edited]
        expected = transitiveDependents graph m
    printf
      "  edit %-42s -> %d recheck(s): %s\n"
      (display m)
      (Set.size changed)
      (unwords (map display (Set.toList changed)))
    assert ("invalidation set matches dependents for " ++ display m) (changed == expected)

  -- Correctness + timing ------------------------------------------------------
  section "compile runs (cold / key-driven warm / full hit)"
  (hull0, checked0, t0) <- runPipeline "cold  (empty cache)" opts graph Map.empty

  -- Simulate the IDE editing the entry module: reuse every module whose key is
  -- unchanged by that edit. That set is computed purely from the keys.
  editedKeys <- orDie (moduleCacheKeysWith (bump entry) opts graph)
  let reusable = Set.fromList [m | m <- order, Map.lookup m keys == Map.lookup m editedKeys]
      stdCache = Map.restrictKeys checked0 reusable
  printf "entry edit reuses %d/%d modules by key\n" (Set.size reusable) (length order)
  (hull1, _, t1) <- runPipeline "warm  (key-driven: entry edited)" opts graph stdCache

  (hull2, _, t2) <- runPipeline "hit   (all modules cached)" opts graph checked0

  -- Phase 3: serialize the cold-checked modules (keyed by Merkle key), round
  -- trip through disk with binary, and compile again reusing the decoded cache.
  section "serialization round-trip (binary, on disk)"
  tmp <- getTemporaryDirectory
  let cacheFile = tmp </> "solcore-tc-cache.bin"
      keyed = Map.fromList [(keys Map.! m, toCachedModule cm) | (m, cm) <- Map.toList checked0]
  BL.writeFile cacheFile (encodeCache keyed)
  blob <- BL.readFile cacheFile
  decoded <-
    maybe
      (putStrLn "cache decode failed (bad header/version)" >> exitFailure)
      pure
      (decodeCache blob)
  let fromDisk =
        Map.fromList
          [ (m, fromCachedModule opts cm)
            | m <- order,
              Just cm <- [Map.lookup (keys Map.! m) decoded]
          ]
  printf "encoded %d modules -> %s (%d bytes)\n" (Map.size decoded) cacheFile (BL.length blob)
  -- Format guard: a blob with a corrupted header is rejected (miss, not crash).
  assert
    "tampered/foreign dump is rejected by the version guard"
    (isNothing (decodeCache (BL.cons 0xff (BL.drop 1 blob))))
  putStrLn "format guard: tampered dump correctly rejected (would recompute)"
  (hullD, _, tD) <- runPipeline "disk  (all modules from on-disk cache)" opts graph fromDisk

  let render = map show :: [Hull.Object] -> [String]
      ok = all (== render hull0) [render hull1, render hull2, render hullD]
  printf
    "\nobjects: %d | cold %.2fs | warm %.2fs | hit %.2fs | disk %.2fs\n"
    (length hull0)
    t0
    t1
    t2
    tD
  if ok
    then putStrLn "RESULT: OK — in-process and on-disk caches both reproduce the cold run"
    else putStrLn "RESULT: MISMATCH — a cached run differs from the cold run" >> exitFailure

-- | Perturb one module's content hash, to simulate an edit to it.
bump :: Mod.ModuleId -> Mod.ModuleId -> BS.ByteString -> BS.ByteString
bump target moduleId h = if moduleId == target then h <> "EDIT" else h

shortKey :: TcCacheKey -> String
shortKey (TcCacheKey bs) = concatMap (printf "%02x") (BS.unpack (BS.take 6 bs))

section :: String -> IO ()
section title = putStrLn ("\n== " ++ title ++ " ==")

assert :: String -> Bool -> IO ()
assert msg cond = unless cond (putStrLn ("FAIL: " ++ msg) >> exitFailure)

orDie :: Either String a -> IO a
orDie = either (\err -> putStrLn err >> exitFailure) pure

loadGraph :: Option -> FilePath -> IO ModuleGraph
loadGraph opts file = do
  result <- runExceptT $ do
    mainRoot <- liftIO (makeAbsolute (optRootDir opts))
    stdRoot <- ExceptT (pure (parseStdRoot (optImportDirs opts)))
    externalLibs <- ExceptT (pure (parseExternalLibSpecs (optExternalLibs opts)))
    ExceptT (loadModuleGraph mainRoot stdRoot externalLibs file)
  orDie result

runPipeline ::
  String ->
  Option ->
  ModuleGraph ->
  Map Mod.ModuleId CheckedModule ->
  IO ([Hull.Object], Map Mod.ModuleId CheckedModule, Double)
runPipeline label opts graph cache = do
  printf "\n-- %s: %d cached --\n" label (Map.size cache)
  (elapsed, result) <- timeItT (runExceptT (compileGraphWithCache opts graph cache))
  case result of
    Left err -> putStrLn ("compile error: " ++ compileDiagnosticsText err) >> exitFailure
    Right (hull, checked) -> pure (hull, checked, elapsed)
