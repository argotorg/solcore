-- | Merkle cache keys for the typecheck cache (Phase 2).
--
-- A module's typechecked output is a pure function of its own source, the
-- public interfaces of the modules it references, and the compile flags that
-- reach the typecheck phase. We capture that as a content-addressed key:
--
-- @
--   key(M) = H( contentHash(M)
--            ++ sorted [ key(R) | R <- references(M) ]
--            ++ flagComponent(M) )
-- @
--
-- folded over the graph in dependency order. Two modules with equal keys are
-- guaranteed to typecheck to the same result, so a cache indexed by key is
-- sound; editing a module changes its key and, transitively, the keys of every
-- module that references it — and nothing else.
--
-- The content hash is taken over the /parsed/ compilation unit (which the graph
-- already holds), so it is insensitive to comments and whitespace for free.
module Solcore.Pipeline.TypecheckCache
  ( TcCacheKey (..),
    moduleHasContracts,
    moduleCacheKeys,
    moduleCacheKeysWith,
    transitiveDependents,
  )
where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BC
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Solcore.Frontend.Module.Identity qualified as Mod
import Solcore.Frontend.Module.Loader
import Solcore.Frontend.Syntax.SyntaxTree (CompUnit (..), TopDecl (..))
import Solcore.Pipeline.Options (Option (..))
import Solcore.Util.Keccak (keccak256)

-- | A content-addressed typecheck-cache key (a keccak digest).
newtype TcCacheKey = TcCacheKey ByteString
  deriving (Eq, Ord, Show)

-- | Does this module define any contracts? Only contract-bearing modules are
-- affected by dispatch generation, so only they fold the -g flag into their
-- key — keeping library (std) keys stable across -g toggles in the UI.
moduleHasContracts :: CompUnit -> Bool
moduleHasContracts unit = any isContract (contracts unit)
  where
    isContract (TContr _) = True
    isContract _ = False

-- | The compile flags reaching the typecheck phase, serialized for the key.
-- -g (dispatch) is only included for contract-bearing modules.
flagComponent :: Option -> Bool -> ByteString
flagComponent opts hasContracts =
  BC.pack
    ( "s="
        ++ show (optNoDesugarCalls opts)
        ++ (if hasContracts then ";g=" ++ show (optNoGenDispatch opts) else "")
    )

-- | Compute the Merkle cache key of every module in the graph.
moduleCacheKeys :: Option -> ModuleGraph -> Either String (Map Mod.ModuleId TcCacheKey)
moduleCacheKeys = moduleCacheKeysWith (\_ h -> h)

-- | 'moduleCacheKeys' with a hook to perturb a module's content hash — used to
-- simulate edits (bump the edited module's content and observe which keys move)
-- without touching disk or re-typechecking.
--
-- Modules are folded in dependency order ('moduleOrder' lists dependencies
-- before dependents), so each module's reference keys are already known when it
-- is processed. A missing reference key means a cyclic import group, which this
-- PoC does not yet handle (SCC-group keying is deferred); we fail loudly.
moduleCacheKeysWith ::
  (Mod.ModuleId -> ByteString -> ByteString) ->
  Option ->
  ModuleGraph ->
  Either String (Map Mod.ModuleId TcCacheKey)
moduleCacheKeysWith perturb opts graph =
  foldl step (Right Map.empty) (moduleOrder graph)
  where
    step (Left err) _ = Left err
    step (Right keys) moduleId =
      case mapM (lookupKey keys) refs of
        Left err -> Left err
        Right refKeys ->
          let material =
                BS.concat
                  ( contentHash moduleId
                      : sort [k | TcCacheKey k <- refKeys]
                      ++ [flagComponent opts (moduleHasContracts (unitOf moduleId))]
                  )
           in Right (Map.insert moduleId (TcCacheKey (keccak256 material)) keys)
      where
        refs = Map.findWithDefault [] moduleId (referenceDependencies graph)

    lookupKey keys ref =
      maybe
        (Left ("cyclic import group at " ++ Mod.moduleIdDisplay ref ++ " (SCC keying not yet implemented)"))
        Right
        (Map.lookup ref keys)

    unitOf moduleId = loadedCompUnit (modules graph Map.! moduleId)
    contentHash moduleId = perturb moduleId (keccak256 (BC.pack (show (unitOf moduleId))))

-- | Every module whose key depends on the given module: the module itself plus
-- everything that transitively references it. Exactly the set that must be
-- re-typechecked when the given module is edited.
transitiveDependents :: ModuleGraph -> Mod.ModuleId -> Set Mod.ModuleId
transitiveDependents graph target = go (Set.singleton target) [target]
  where
    revAdj =
      Map.fromListWith
        (++)
        [ (ref, [moduleId])
          | (moduleId, refs) <- Map.toList (referenceDependencies graph),
            ref <- refs
        ]
    go seen [] = seen
    go seen (x : xs) =
      let next = filter (`Set.notMember` seen) (Map.findWithDefault [] x revAdj)
       in go (foldr Set.insert seen next) (next ++ xs)
