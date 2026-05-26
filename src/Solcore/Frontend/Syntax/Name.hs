{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}

module Solcore.Frontend.Syntax.Name where

import Common.Pretty
import Control.Applicative ((<|>))
import Data.Generics (Data, Typeable)
import Data.String
import Solcore.Diagnostics (SourceSpan, combineSourceSpans)

data Name
  = NameWithSpan (Maybe SourceSpan) String
  | QualNameWithSpan (Maybe SourceSpan) Name String
  deriving (Data, Typeable)

pattern Name :: String -> Name
pattern Name s <- NameWithSpan _ s
  where
    Name s = NameWithSpan Nothing s

pattern QualName :: Name -> String -> Name
pattern QualName n s <- QualNameWithSpan _ n s
  where
    QualName n s = QualNameWithSpan Nothing n s

{-# COMPLETE Name, QualName #-}

instance Eq Name where
  left == right = nameSegments left == nameSegments right

instance Ord Name where
  compare left right = compare (nameSegments left) (nameSegments right)

instance Show Name where
  show (Name s) = s
  show (QualName n s) =
    show n ++ "." ++ s

instance IsString Name where
  fromString = Name

instance Pretty Name where
  ppr (QualName n s) = ppr n <> text "." <> text s
  ppr (Name s) = text s

nameSourceSpan :: Name -> Maybe SourceSpan
nameSourceSpan (NameWithSpan sourceSpan _) = sourceSpan
nameSourceSpan (QualNameWithSpan sourceSpan qualifier _) = sourceSpan <|> nameSourceSpan qualifier

locatedName :: SourceSpan -> Name -> Name
locatedName sourceSpan (Name s) = NameWithSpan (Just sourceSpan) s
locatedName sourceSpan (QualName qualifier s) = QualNameWithSpan (Just sourceSpan) qualifier s

withNameSourceSpan :: Maybe SourceSpan -> Name -> Name
withNameSourceSpan Nothing name = name
withNameSourceSpan (Just sourceSpan) name = locatedName sourceSpan name

copyNameSourceSpan :: Name -> Name -> Name
copyNameSourceSpan source target =
  withNameSourceSpan (nameSourceSpan source) target

qualifyName :: Name -> Name -> Name
qualifyName qualifier leaf =
  copyNameSourceSpan leaf (QualName qualifier (show leaf))

locatedQualName :: Name -> SourceSpan -> String -> Name
locatedQualName qualifier leafSpan leaf =
  QualNameWithSpan (Just sourceSpan) qualifier leaf
  where
    sourceSpan = maybe leafSpan (`combineSourceSpans` leafSpan) (nameSourceSpan qualifier)

stripNameSourceSpan :: Name -> Name
stripNameSourceSpan (Name s) = Name s
stripNameSourceSpan (QualName qualifier s) = QualName (stripNameSourceSpan qualifier) s

nameSegments :: Name -> [String]
nameSegments (Name s) = [s]
nameSegments (QualName n s) = nameSegments n ++ [s]
