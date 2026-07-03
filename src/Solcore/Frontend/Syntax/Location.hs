module Solcore.Frontend.Syntax.Location where

import Control.Applicative ((<|>))
import Data.Generics (Data, Typeable, everything, mkQ)
import Solcore.Diagnostics (SourceSpan, combineSourceSpans)

data NodeOrigin
  = SourceNode SourceSpan
  | GeneratedNode
  deriving (Show, Data, Typeable)

newtype NodeLocation
  = NodeLocation {nodeLocationOrigin :: NodeOrigin}
  deriving (Show, Data, Typeable)

instance Eq NodeLocation where
  _ == _ = True

instance Ord NodeLocation where
  compare _ _ = EQ

unlocatedNode :: NodeLocation
unlocatedNode = generatedNode

generatedNode :: NodeLocation
generatedNode = NodeLocation GeneratedNode

locatedNode :: SourceSpan -> NodeLocation
locatedNode = NodeLocation . SourceNode

withNodeSourceSpan :: Maybe SourceSpan -> NodeLocation
withNodeSourceSpan Nothing = generatedNode
withNodeSourceSpan (Just sourceSpan) = locatedNode sourceSpan

nodeLocationSpan :: NodeLocation -> Maybe SourceSpan
nodeLocationSpan (NodeLocation (SourceNode sourceSpan)) = Just sourceSpan
nodeLocationSpan (NodeLocation GeneratedNode) = Nothing

isSourceNodeLocation :: NodeLocation -> Bool
isSourceNodeLocation (NodeLocation (SourceNode _)) = True
isSourceNodeLocation (NodeLocation GeneratedNode) = False

isGeneratedNodeLocation :: NodeLocation -> Bool
isGeneratedNodeLocation = not . isSourceNodeLocation

nodeLocationsOf :: (Data a) => a -> [NodeLocation]
nodeLocationsOf =
  everything (++) (mkQ [] nodeLocationList)
  where
    nodeLocationList :: NodeLocation -> [NodeLocation]
    nodeLocationList location = [location]

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
