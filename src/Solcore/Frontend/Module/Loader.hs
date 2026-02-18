module Solcore.Frontend.Module.Loader
  ( ModuleGraph (..),
    loadModuleGraph,
    flattenModuleCompUnit,
    loadCompUnit,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List (intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set (Set)
import Data.Set qualified as Set
import Solcore.Frontend.Parser.SolcoreParser (parseCompUnit)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree
import System.Directory (canonicalizePath, doesFileExist)
import System.FilePath

data LoadState
  = LoadState
  { loadedModules :: Map FilePath CompUnit,
    moduleDeps :: Map FilePath [FilePath],
    loadOrder :: [FilePath]
  }

emptyLoadState :: LoadState
emptyLoadState = LoadState Map.empty Map.empty []

data ModuleGraph
  = ModuleGraph
  { entryModule :: FilePath,
    modules :: Map FilePath CompUnit,
    dependencies :: Map FilePath [FilePath],
    moduleOrder :: [FilePath]
  }
  deriving (Eq, Show)

loadModuleGraph :: [FilePath] -> FilePath -> IO (Either String ModuleGraph)
loadModuleGraph roots entryFile = runExceptT do
  entryCanonical <- liftIO $ canonicalizePath entryFile
  st <- execStateT (visit roots [] entryCanonical) emptyLoadState
  pure
    ( ModuleGraph
        { entryModule = entryCanonical,
          modules = loadedModules st,
          dependencies = moduleDeps st,
          moduleOrder = reverse (loadOrder st)
        }
    )

-- Loads the entry file and all recursive imports, and keeps the old
-- flattened-declaration behavior for compatibility.
loadCompUnit :: [FilePath] -> FilePath -> IO (Either String CompUnit)
loadCompUnit roots entryFile = runExceptT do
  graph <- ExceptT $ loadModuleGraph roots entryFile
  ExceptT $ pure (flattenModuleCompUnit graph (entryModule graph))

visit :: [FilePath] -> [FilePath] -> FilePath -> StateT LoadState (ExceptT String IO) ()
visit roots stack canonicalPath = do
  alreadyLoaded <- gets (Map.member canonicalPath . loadedModules)
  unless alreadyLoaded do
    when (canonicalPath `elem` stack) $
      throwError (cycleError canonicalPath stack)
    content <- liftIO (readFile canonicalPath)
    parsed <- liftIO (parseCompUnit content)
    cunit <- either throwError pure parsed
    importPaths <- mapM (resolveImportPath roots) (imports cunit)
    canonicalImports <- liftIO (mapM canonicalizePath importPaths)
    mapM_ (visit roots (canonicalPath : stack)) canonicalImports
    modify
      ( \st ->
          st
            { loadedModules = Map.insert canonicalPath cunit (loadedModules st),
              moduleDeps = Map.insert canonicalPath canonicalImports (moduleDeps st),
              loadOrder = canonicalPath : loadOrder st
            }
      )

resolveImportPath :: [FilePath] -> Import -> StateT LoadState (ExceptT String IO) FilePath
resolveImportPath roots imp =
  go roots
  where
    go [] =
      throwError $
        "import "
          ++ show (importModuleName imp)
          ++ ": file not found"
    go (dir : rest) = do
      let path = toFilePath dir (importModuleName imp)
      exists <- liftIO $ doesFileExist path
      if exists then pure path else go rest

importModuleName :: Import -> Name
importModuleName (ImportModule n) = n
importModuleName (ImportAlias n _) = n
importModuleName (ImportOnly n _) = n

toFilePath :: FilePath -> Name -> FilePath
toFilePath base =
  (base </>) . (<.> "solc") . foldr step "" . show
  where
    step c ac
      | c == '.' = pathSeparator : ac
      | otherwise = c : ac

topDeclsFrom :: CompUnit -> [TopDecl]
topDeclsFrom (CompUnit _ ds) = ds

cycleError :: FilePath -> [FilePath] -> String
cycleError path stack =
  let prefix = takeWhile (/= path) stack
      cycleChain = path : reverse prefix ++ [path]
   in unlines
        [ "Import cycle detected:",
          "  " ++ foldr1 (\a b -> a ++ " -> " ++ b) cycleChain
        ]

flattenModuleCompUnit :: ModuleGraph -> FilePath -> Either String CompUnit
flattenModuleCompUnit graph modulePath = do
  unit <-
    maybe
      (Left ("Internal error: module not loaded: " ++ modulePath))
      Right
      (Map.lookup modulePath (modules graph))
  ensureNoAmbiguousSelectedImports unit
  let depSet = dependencyClosure graph modulePath
      depOrder = filter (`Set.member` depSet) (moduleOrder graph)
      directDeps = Map.findWithDefault [] modulePath (dependencies graph)
      directImportMap = Map.fromList (zip directDeps (imports unit))
      importedDecls =
        concatMap
          (importedDeclsFor graph directImportMap)
          depOrder
      qualifiedDecls = concatMap (qualifiedImportDecls graph) (zip (imports unit) directDeps)
  pure (CompUnit (imports unit) (qualifiedDecls ++ importedDecls ++ topDeclsFrom unit))

dependencyClosure :: ModuleGraph -> FilePath -> Set FilePath
dependencyClosure graph start = go Set.empty (Map.findWithDefault [] start (dependencies graph))
  where
    go :: Set FilePath -> [FilePath] -> Set FilePath
    go seen [] = seen
    go seen (p : ps)
      | p `Set.member` seen = go seen ps
      | otherwise =
          let next = Map.findWithDefault [] p (dependencies graph)
           in go (Set.insert p seen) (next ++ ps)

qualifiedImportDecls :: ModuleGraph -> (Import, FilePath) -> [TopDecl]
qualifiedImportDecls graph (imp, modulePath) =
  case imp of
    ImportOnly _ _ -> []
    ImportModule n -> qualifyFunctions n
    ImportAlias _ n -> qualifyFunctions n
  where
    qualifyFunctions qualifier =
      case Map.lookup modulePath (modules graph) of
        Nothing -> []
        Just cunit ->
          [ TFunDef (qualifyFunction qualifier fd)
            | TFunDef fd <- topDeclsFrom cunit
          ]

qualifyFunction :: Name -> FunDef -> FunDef
qualifyFunction qualifier (FunDef sig body) =
  FunDef
    (sig {sigName = QualName qualifier (show (sigName sig))})
    body

importedDeclsFor :: ModuleGraph -> Map FilePath Import -> FilePath -> [TopDecl]
importedDeclsFor graph directImportMap modulePath =
  case Map.lookup modulePath directImportMap of
    Just imp ->
      filter (isVisibleFromImport imp) topDecls
    Nothing ->
      topDecls
  where
    topDecls =
      topDeclsFrom (modules graph Map.! modulePath)

isVisibleFromImport :: Import -> TopDecl -> Bool
isVisibleFromImport (ImportOnly _ names) (TFunDef (FunDef sig _)) =
  sigName sig `elem` names
isVisibleFromImport _ _ = True

ensureNoAmbiguousSelectedImports :: CompUnit -> Either String ()
ensureNoAmbiguousSelectedImports (CompUnit imps _) =
  case ambiguous of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Ambiguous selected imports:",
            unlines (map formatAmbiguous xs)
          ]
  where
    selections :: Map Name [Name]
    selections =
      Map.fromListWith (++)
        [ (item, [modName])
          | ImportOnly modName items <- imps,
            item <- items
        ]
    ambiguous :: [(Name, [Name])]
    ambiguous =
      [ (item, mods)
        | (item, mods) <- Map.toList selections,
          length mods > 1
      ]

formatAmbiguous :: (Name, [Name]) -> String
formatAmbiguous (item, mods) =
  "  "
    ++ show item
    ++ " imported from "
    ++ intercalate ", " (map show mods)
