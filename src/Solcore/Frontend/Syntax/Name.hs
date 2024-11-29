{-# LANGUAGE OverloadedStrings #-}

module Solcore.Frontend.Syntax.Name where

import Data.Generics (Data, Typeable)
import Data.String

import System.FilePath

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

toFilePath :: FilePath -> Name -> FilePath
toFilePath base =
  (base </>) . (<.> "solc") . foldr step "" . show
 where
  step c ac
    | c == '.' = pathSeparator : ac
    | otherwise = c : ac
