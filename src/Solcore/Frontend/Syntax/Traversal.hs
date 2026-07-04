{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Solcore.Frontend.Syntax.Traversal
  ( opaqueToTypes,
    everywhereButSpans,
    everywhereMButSpans,
    everythingButSpans,
  )
where

import Data.Generics (GenericM, GenericQ, GenericT, everywhereBut, extQ, gmapM, gmapQ, mkQ)
import Solcore.Diagnostics (SourceSpan)
import Solcore.Frontend.Syntax.Location (NodeLocation)
import Solcore.Frontend.Syntax.Name (Name)

-- Nodes that carry no 'Ty' and no 'Id': source-location metadata.  A 'Name'
-- is only spans, strings and nested 'Name's; 'NodeLocation'/'SourceSpan' are
-- pure position data.  A 'Ty'/'Id' is never reachable *through* one of these,
-- so it is safe to stop SYB descent here.
opaqueToTypes :: GenericQ Bool
opaqueToTypes =
  False
    `mkQ` (const True :: Name -> Bool)
    `extQ` (const True :: NodeLocation -> Bool)
    `extQ` (const True :: SourceSpan -> Bool)

-- Drop-in replacement for @everywhere t@ that never walks into source-span /
-- name metadata.  Semantically identical to @everywhere t@
everywhereButSpans :: GenericT -> GenericT
everywhereButSpans = everywhereBut opaqueToTypes

-- Monadic 'everywhereM' that stops at source-span / name metadata.  SYB has
-- no @everywhereMBut@, so we spell it out (bottom-up, same shape as
-- 'everywhereM').  Safe for the same reason as 'everywhereButSpans'.
everywhereMButSpans :: forall m. (Monad m) => GenericM m -> GenericM m
everywhereMButSpans f = go
  where
    go :: GenericM m
    go x
      | opaqueToTypes x = pure x
      | otherwise = f =<< gmapM go x

-- 'everything' that stops at source-span / name metadata.  Identical results
-- for any query that collects from 'Ty'/'Id'/'Exp'/… nodes, since spans/names
-- contribute the query's empty element and hold no such nodes.
everythingButSpans :: forall r. (r -> r -> r) -> GenericQ r -> GenericQ r
everythingButSpans k q = go
  where
    go :: GenericQ r
    go x
      | opaqueToTypes x = q x
      | otherwise = foldl k (q x) (gmapQ go x)
