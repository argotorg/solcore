module Solcore.Frontend.Syntax.Location where

import Control.Applicative ((<|>))
import Data.Generics (Data, Typeable)
import Solcore.Diagnostics (SourceSpan, combineSourceSpans)

newtype NodeLocation
  = NodeLocation {nodeLocationSpan :: Maybe SourceSpan}
  deriving (Show, Data, Typeable)

instance Eq NodeLocation where
  _ == _ = True

instance Ord NodeLocation where
  compare _ _ = EQ

unlocatedNode :: NodeLocation
unlocatedNode = NodeLocation Nothing

locatedNode :: SourceSpan -> NodeLocation
locatedNode = NodeLocation . Just

withNodeSourceSpan :: Maybe SourceSpan -> NodeLocation
withNodeSourceSpan = NodeLocation

combineMaybeSourceSpans :: Maybe SourceSpan -> Maybe SourceSpan -> Maybe SourceSpan
combineMaybeSourceSpans Nothing right = right
combineMaybeSourceSpans left Nothing = left
combineMaybeSourceSpans (Just left) (Just right) = Just (combineSourceSpans left right)

firstSourceSpan :: [Maybe SourceSpan] -> Maybe SourceSpan
firstSourceSpan = foldr (<|>) Nothing

class HasSourceSpan a where
  sourceSpanOf :: a -> Maybe SourceSpan

instance HasSourceSpan NodeLocation where
  sourceSpanOf = nodeLocationSpan

instance (HasSourceSpan a) => HasSourceSpan (Maybe a) where
  sourceSpanOf = (>>= sourceSpanOf)

instance (HasSourceSpan a) => HasSourceSpan [a] where
  sourceSpanOf = firstSourceSpan . map sourceSpanOf

instance (HasSourceSpan a, HasSourceSpan b) => HasSourceSpan (a, b) where
  sourceSpanOf (left, right) = firstSourceSpan [sourceSpanOf left, sourceSpanOf right]

instance (HasSourceSpan a, HasSourceSpan b, HasSourceSpan c) => HasSourceSpan (a, b, c) where
  sourceSpanOf (left, middle, right) =
    firstSourceSpan [sourceSpanOf left, sourceSpanOf middle, sourceSpanOf right]
