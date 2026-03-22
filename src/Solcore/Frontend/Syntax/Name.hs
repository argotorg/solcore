{-# LANGUAGE OverloadedStrings #-}

module Solcore.Frontend.Syntax.Name where

import Common.Pretty
import Data.Generics (Data, Typeable)
import Data.String

data Name
  = Name String
  | QualName Name String
  deriving (Eq, Ord, Data, Typeable)

instance Show Name where
  show (Name s) = s
  show (QualName n s) =
    show n ++ "." ++ s

instance IsString Name where
  fromString = Name

instance Pretty Name where
  ppr (QualName n s) = ppr n <> text "." <> text s
  ppr (Name s) = text s
