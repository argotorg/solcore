-- The coercion graph: the mechanism behind implicit coercions
-- Coercions are declared as Coerce instances whose head encodes the edge as
-- Pair(S, Proxy(T)) : Coerce(T). This module reads those
-- edges, builds a directed graph over coercion-endpoint types, and resolves a
-- needed coercion S -> T to a single, deterministic canonical path.
module Solcore.Frontend.TypeInference.CoercionGraph
  ( CoercionEdge (..),
    CoercionGraph,
    emptyCoercionGraph,
    coercionEdgeOf,
    buildCoercionGraph,
    canonicalCoercionPath,
    coercionNodes,
  )
where

import Data.List (intercalate, nub, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Ty

-- A declared coercion edge S -> T with its witness function/constructor.
data CoercionEdge = CoercionEdge
  { ceSrc :: Ty,
    ceTgt :: Ty,
    ceWitness :: Name
  }
  deriving (Eq, Show)

-- A location-insensitive structural key for a (ground) coercion endpoint.
tyKey :: Ty -> String
tyKey (TyVar v) = "#" ++ show (tyvarName v)
tyKey (Meta m) = "?" ++ show (metaName m)
tyKey (TyCon n ts) = show n ++ "(" ++ intercalate "," (map tyKey ts) ++ ")"

-- The graph: adjacency keyed by source-node key, plus a representative Ty
-- per node key so callers can recover the endpoint types.
data CoercionGraph = CoercionGraph
  { cgAdj :: Map String [(String, Name)],
    cgRep :: Map String Ty
  }
  deriving (Show)

emptyCoercionGraph :: CoercionGraph
emptyCoercionGraph = CoercionGraph Map.empty Map.empty

-- Recognize a Coerce instance head and read off its edge. The head is
-- Pair(S, Proxy(T)) : Coerce(T); return (S, T).
coercionEdgeOf :: Name -> Ty -> Maybe (Ty, Ty)
coercionEdgeOf cls main
  | cls == Name "Coerce",
    TyCon pair [s, TyCon proxy [t]] <- main,
    pair == Name "Pair",
    proxy == Name "Proxy" =
      Just (s, t)
  | otherwise = Nothing

-- Build the graph from all declared coercion edges, running the decidable
-- coherence check (no two edges with the same (src,tgt) but different
-- witnesses). Returns a diagnostic on failure.
buildCoercionGraph :: [CoercionEdge] -> Either String CoercionGraph
buildCoercionGraph edges =
  do
    checkDuplicateEdges edges
    let adj =
          Map.fromListWith
            (++)
            [(tyKey (ceSrc e), [(tyKey (ceTgt e), ceWitness e)]) | e <- edges]
        rep =
          Map.fromList $
            concat [[(tyKey (ceSrc e), ceSrc e), (tyKey (ceTgt e), ceTgt e)] | e <- edges]
    pure (CoercionGraph adj rep)

-- Reject a source/target pair declared with more than one distinct witness.
checkDuplicateEdges :: [CoercionEdge] -> Either String ()
checkDuplicateEdges edges =
  let byPair =
        Map.fromListWith
          (++)
          [((tyKey (ceSrc e), tyKey (ceTgt e)), [ceWitness e]) | e <- edges]
      clash = [(s, t, nub ws) | ((s, t), ws) <- Map.toList byPair, length (nub ws) > 1]
   in case clash of
        [] -> Right ()
        ((s, t, ws) : _) ->
          Left $
            "coercion "
              ++ s
              ++ " -> "
              ++ t
              ++ " declared with multiple different witnesses: "
              ++ intercalate ", " (map show ws)

-- The node keys present in the graph.
coercionNodes :: CoercionGraph -> [String]
coercionNodes = Map.keys . cgRep

-- The canonical coercion path S -> T
canonicalCoercionPath :: CoercionGraph -> Ty -> Ty -> Maybe [Name]
canonicalCoercionPath g s t
  | sk == tk = Just []
  | otherwise = bfs (cgAdj g) sk tk
  where
    sk = tyKey s
    tk = tyKey t

-- Deterministic breadth-first search. Neighbors are explored in sorted key
-- order and nodes are marked visited on enqueue, so the first path reaching the
-- goal is the shortest one under a fixed tie-break: the canonical path.
bfs :: Map String [(String, Name)] -> String -> String -> Maybe [Name]
bfs adj start goal = go [(start, [])] (Set.singleton start)
  where
    go :: [(String, [Name])] -> Set String -> Maybe [Name]
    go [] _ = Nothing
    go ((cur, revWits) : queue) visited
      | cur == goal = Just (reverse revWits)
      | otherwise =
          let neighbors = sortOn fst (Map.findWithDefault [] cur adj)
              fresh = [(n, w : revWits) | (n, w) <- neighbors, not (Set.member n visited)]
              visited' = foldr (Set.insert . fst) visited fresh
           in go (queue ++ fresh) visited'
