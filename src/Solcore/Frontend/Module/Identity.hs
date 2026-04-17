module Solcore.Frontend.Module.Identity
  ( LibraryId (..),
    ModuleId (..),
    modulePathName,
    modulePathDisplay,
    moduleIdDisplay,
    splitQualifiedName,
    joinQualifiedName,
    appendRelativeModulePath,
    moduleDirectory,
    moduleFilePath,
  )
where

import Data.List (intercalate)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

data LibraryId
  = MainLibrary
  | StdLibrary
  | ExternalLibrary Name
  deriving (Eq, Ord, Show)

data ModuleId
  = ModuleId
  { moduleLibrary :: LibraryId,
    moduleName :: Name
  }
  deriving (Eq, Ord, Show)

modulePathName :: ModulePath -> Name
modulePathName (RelativePath n) = n
modulePathName (LibraryPath n) = n
modulePathName (ExternalPath _ n) = n

modulePathDisplay :: ModulePath -> String
modulePathDisplay (RelativePath n) = show n
modulePathDisplay (LibraryPath n) = "lib." ++ show n
modulePathDisplay (ExternalPath libName n) = "@" ++ show libName ++ "." ++ show n

moduleIdDisplay :: ModuleId -> String
moduleIdDisplay (ModuleId MainLibrary n) = show n
moduleIdDisplay (ModuleId StdLibrary n)
  | n == Name "std" = "std"
  | otherwise = "std." ++ show n
moduleIdDisplay (ModuleId (ExternalLibrary libName) n) =
  "@" ++ show libName ++ "." ++ show n

splitQualifiedName :: Name -> [String]
splitQualifiedName (Name n) = [n]
splitQualifiedName (QualName n s) = splitQualifiedName n ++ [s]

joinQualifiedName :: [String] -> Name
joinQualifiedName [] = error "joinQualifiedName: empty qualified name"
joinQualifiedName (n : ns) = foldl QualName (Name n) ns

moduleDirectory :: Name -> [String]
moduleDirectory n =
  case splitQualifiedName n of
    [] -> []
    [_] -> []
    xs -> init xs

appendRelativeModulePath :: Name -> Name -> Name
appendRelativeModulePath base rel =
  joinQualifiedName (moduleDirectory base ++ splitQualifiedName rel)

moduleFilePath :: Name -> FilePath
moduleFilePath =
  (++ ".solc") . intercalate "/" . splitQualifiedName
