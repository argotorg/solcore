-- | Round-trip tests for the typecheck-cache persistence layer (Tier 2).
--
-- The property under test: dumping the std-library subset of a session cache to
-- a blob and reloading it reproduces a cold compile byte-for-byte. This is the
-- path that exercises @binary@ serialization end to end — encode, decode, and
-- the 'fromCachedModule' reconstruction whose dropped fields are loud error
-- thunks. If any of those thunks were actually forced downstream, the warm
-- compile below would crash rather than match.
module TcCacheTests where

import Control.Monad.Except (runExceptT)
import Data.Map qualified as Map
import Solcore.Api (defaultOptions, dumpStdCacheBlob, indexCheckedByKey, loadStdCacheBlob, renderObjects)
import Solcore.Frontend.Module.Loader (ModuleGraph (..), loadModuleGraphFromSource)
import Solcore.Pipeline.SolcorePipeline (compileGraphWithCache)
import Solcore.Pipeline.TypecheckCache (moduleCacheKeys)
import Test.Tasty
import Test.Tasty.HUnit

tcCacheTests :: TestTree
tcCacheTests =
  testGroup
    "Typecheck cache (dump/load round-trip)"
    [ testCase "std dump reloaded from a blob reproduces cold output byte-for-byte" $ do
        source <- readFile "test/examples/dispatch/basic.solc"
        graphE <- loadModuleGraphFromSource source
        graph <- case graphE of
          Left err -> assertFailure ("graph load failed: " ++ err)
          Right g -> pure g
        keys <- case moduleCacheKeys defaultOptions graph of
          Left err -> assertFailure ("cache keys failed: " ++ err)
          Right ks -> pure ks
        -- Cold compile with an empty seed: every module is typechecked.
        coldE <- runExceptT (compileGraphWithCache defaultOptions graph Map.empty)
        (coldObjs, coldChecked) <- case coldE of
          Left err -> assertFailure ("cold compile failed: " ++ err)
          Right r -> pure r
        -- Persist the std subset and read it back through the blob format.
        let session = indexCheckedByKey keys coldChecked
            blob = dumpStdCacheBlob session
            recovered = loadStdCacheBlob defaultOptions blob
        assertBool "expected some std modules to be cached" (not (Map.null recovered))
        -- Seed a fresh compile from the reloaded std entries only.
        let seed =
              Map.fromList
                [ (mid, cm)
                | mid <- moduleOrder graph,
                  Just k <- [Map.lookup mid keys],
                  Just cm <- [Map.lookup k recovered]
                ]
        assertBool "expected the seed to cover std modules" (not (Map.null seed))
        warmE <- runExceptT (compileGraphWithCache defaultOptions graph seed)
        warmObjs <- case warmE of
          Left err -> assertFailure ("warm compile failed: " ++ err)
          Right (objs, _) -> pure objs
        renderObjects warmObjs @?= renderObjects coldObjs
    ]
