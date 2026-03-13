module Solcore.Frontend.Module.Loader
  ( ModuleGraph (..),
    LoadedModule (..),
    loadModuleGraph,
    flattenModuleValidationCompUnit,
    flattenModuleStrictCompileCompUnit,
    flattenModuleStrictCompileCompUnitWithImportedStart,
    flattenModuleStrictValidationCompUnit,
    loadCompUnit,
    moduleSourcePath,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.List (isPrefixOf, intercalate)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import Data.Set qualified as Set
import Solcore.Frontend.Module.Identity qualified as Mod
import Solcore.Frontend.Parser.SolcoreParser (parseCompUnit)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree
import System.Directory (doesFileExist, makeAbsolute)
import System.FilePath

data LoadedModule
  = LoadedModule
  { loadedSourcePath :: FilePath,
    loadedCompUnit :: CompUnit,
    loadedModuleRefs :: Map ModulePath Mod.ModuleId
  }
  deriving (Eq, Show)

data LoaderConfig
  = LoaderConfig
  { mainRoot :: FilePath,
    stdRoot :: Maybe FilePath
  }

data LoadState
  = LoadState
  { loadedModules :: Map Mod.ModuleId LoadedModule,
    moduleDeps :: Map Mod.ModuleId [Mod.ModuleId],
    loadOrder :: [Mod.ModuleId]
  }

emptyLoadState :: LoadState
emptyLoadState = LoadState Map.empty Map.empty []

data ModuleGraph
  = ModuleGraph
  { entryModule :: Mod.ModuleId,
    modules :: Map Mod.ModuleId LoadedModule,
    dependencies :: Map Mod.ModuleId [Mod.ModuleId],
    moduleOrder :: [Mod.ModuleId],
    graphMainRoot :: FilePath,
    graphStdRoot :: Maybe FilePath
  }
  deriving (Eq, Show)

loadModuleGraph :: [FilePath] -> FilePath -> IO (Either String ModuleGraph)
loadModuleGraph roots entryFile = runExceptT do
  entryAbsolute <- liftIO $ makeAbsolute entryFile
  cfg <- liftIO $ mkLoaderConfig roots entryFile
  entryId <- moduleIdForPath Mod.MainLibrary (mainRoot cfg) entryAbsolute
  st <- execStateT (visit cfg [] entryId entryAbsolute) emptyLoadState
  pure
    ( ModuleGraph
        { entryModule = entryId,
          modules = loadedModules st,
          dependencies = moduleDeps st,
          moduleOrder = reverse (loadOrder st),
          graphMainRoot = mainRoot cfg,
          graphStdRoot = stdRoot cfg
        }
    )

-- Loads the entry file and all recursive imports, and keeps the old
-- flattened-declaration behavior for compatibility.
loadCompUnit :: [FilePath] -> FilePath -> IO (Either String CompUnit)
loadCompUnit roots entryFile = runExceptT do
  graph <- ExceptT $ loadModuleGraph roots entryFile
  ExceptT $ pure (flattenModuleValidationCompUnit graph (entryModule graph))

mkLoaderConfig :: [FilePath] -> FilePath -> IO LoaderConfig
mkLoaderConfig roots entryFile = do
  let defaultMainRoot = takeDirectory entryFile
      requestedMainRoot = case roots of
        [] -> defaultMainRoot
        x : _ -> x
      requestedStdRoot = case roots of
        [] -> Nothing
        _ : xs -> case xs of
          [] -> Nothing
          y : _ -> Just y
  mainRoot' <- makeAbsolute requestedMainRoot
  stdRoot' <- traverse makeAbsolute requestedStdRoot
  pure
    LoaderConfig
      { mainRoot = mainRoot',
        stdRoot = stdRoot'
      }

visit ::
  LoaderConfig ->
  [Mod.ModuleId] ->
  Mod.ModuleId ->
  FilePath ->
  StateT LoadState (ExceptT String IO) ()
visit cfg stack moduleId sourcePath = do
  alreadyLoaded <- gets (Map.member moduleId . loadedModules)
  unless alreadyLoaded do
    when (moduleId `elem` stack) $
      throwError (cycleError moduleId stack)
    content <- liftIO (readFile sourcePath)
    parsed <- liftIO (parseCompUnit content)
    cunit <- either throwError pure parsed
    importedModules <- mapM (resolveImportPath cfg moduleId) (imports cunit)
    exportedModules <-
      mapM (resolveModuleReference cfg moduleId "export") (exportModulePaths cunit)
    let moduleRefs =
          Map.fromList $
            [(importModule imp, importId) | (imp, (importId, _)) <- zip (imports cunit) importedModules]
              ++ [(path, exportId) | (path, exportId, _) <- exportedModules]
        referencedModules =
          uniqueResolvedModules
            (importedModules ++ [(exportId, exportPath) | (_, exportId, exportPath) <- exportedModules])
    mapM_
      (\(targetId, targetPath) -> visit cfg (moduleId : stack) targetId targetPath)
      referencedModules
    modify
      ( \st ->
          st
            { loadedModules = Map.insert moduleId (LoadedModule sourcePath cunit moduleRefs) (loadedModules st),
              moduleDeps = Map.insert moduleId (map fst importedModules) (moduleDeps st),
              loadOrder = moduleId : loadOrder st
            }
      )

resolveImportPath ::
  LoaderConfig ->
  Mod.ModuleId ->
  Import ->
  StateT LoadState (ExceptT String IO) (Mod.ModuleId, FilePath)
resolveImportPath cfg currentModule imp =
  fmap (\(_, targetId, targetPath) -> (targetId, targetPath)) $
    resolveModuleReference cfg currentModule "import" (importModule imp)

resolveModuleReference ::
  LoaderConfig ->
  Mod.ModuleId ->
  String ->
  ModulePath ->
  StateT LoadState (ExceptT String IO) (ModulePath, Mod.ModuleId, FilePath)
resolveModuleReference cfg currentModule refKind modulePath = do
  candidates <- either throwError pure (resolveModuleImportCandidates cfg currentModule modulePath)
  resolved <- liftIO $ firstExisting candidates
  case resolved of
    Just (targetId, targetPath) -> pure (modulePath, targetId, targetPath)
    Nothing ->
      throwError $
        refKind
          ++ " "
          ++ Mod.modulePathDisplay modulePath
          ++ ": file not found"

importModuleName :: Import -> Name
importModuleName = Mod.modulePathName . importModule

toFilePath :: FilePath -> Name -> FilePath
toFilePath base = (base </>) . Mod.moduleFilePath

resolveModuleImportCandidates ::
  LoaderConfig ->
  Mod.ModuleId ->
  ModulePath ->
  Either String [(Mod.ModuleId, FilePath)]
resolveModuleImportCandidates cfg currentModule path =
  case path of
    RelativePath relName
      | isStdSpecial relName,
        Just root <- stdRoot cfg ->
          pure [(Mod.ModuleId Mod.StdLibrary relName, toFilePath root relName)]
      | otherwise ->
          pure (resolveRelativeCandidates currentLibrary resolvedName relName)
      where
        currentLibrary = Mod.moduleLibrary currentModule
        resolvedName = Mod.appendRelativeModulePath (Mod.moduleName currentModule) relName
    LibraryPath absName ->
      pure [resolveWithinLibrary (Mod.moduleLibrary currentModule) absName]
    ExternalPath libName _ ->
      Left ("unsupported external library import: @" ++ show libName)
  where
    resolveWithinLibrary libId modName =
      (Mod.ModuleId libId modName, toFilePath (rootForLibrary cfg libId) modName)

    resolveRelativeCandidates libId resolvedName relName =
      case stdRoot cfg of
        Just root
          | libId == Mod.MainLibrary ->
              [ resolveWithinLibrary libId resolvedName,
                (Mod.ModuleId Mod.StdLibrary relName, toFilePath root relName)
              ]
        _ ->
          [resolveWithinLibrary libId resolvedName]

firstExisting :: [(Mod.ModuleId, FilePath)] -> IO (Maybe (Mod.ModuleId, FilePath))
firstExisting [] = pure Nothing
firstExisting (candidate@(_, path) : rest) = do
  exists <- doesFileExist path
  if exists then pure (Just candidate) else firstExisting rest

isStdSpecial :: Name -> Bool
isStdSpecial (Name "std") = True
isStdSpecial (QualName (Name "std") _) = True
isStdSpecial _ = False

rootForLibrary :: LoaderConfig -> Mod.LibraryId -> FilePath
rootForLibrary cfg Mod.MainLibrary = mainRoot cfg
rootForLibrary cfg Mod.StdLibrary =
  maybe (mainRoot cfg </> "std") id (stdRoot cfg)
rootForLibrary _ (Mod.ExternalLibrary libName) =
  error ("external library root is not configured: " ++ show libName)

moduleIdForPath :: Mod.LibraryId -> FilePath -> FilePath -> ExceptT String IO Mod.ModuleId
moduleIdForPath libId root filePath =
  case makeRelativeToRoot root filePath of
    Nothing ->
      throwError $
        "source file is outside library root:\n  "
          ++ filePath
          ++ "\n  root: "
          ++ root
    Just relPath ->
      case splitDirectories (dropExtension relPath) of
        [] ->
          throwError ("invalid module path for source file: " ++ filePath)
        parts ->
          pure (Mod.ModuleId libId (Mod.joinQualifiedName parts))

makeRelativeToRoot :: FilePath -> FilePath -> Maybe FilePath
makeRelativeToRoot root filePath
  | rootDir `isPrefixOf` fileDir = Just (makeRelative root filePath)
  | otherwise = Nothing
  where
    rootDir = addTrailingPathSeparator (normalise root)
    fileDir = normalise filePath

topDeclsFrom :: CompUnit -> [TopDecl]
topDeclsFrom (CompUnit _ ds) = ds

cycleError :: Mod.ModuleId -> [Mod.ModuleId] -> String
cycleError path stack =
  let prefix = takeWhile (/= path) stack
      cycleChain = path : reverse prefix ++ [path]
   in unlines
        [ "Import cycle detected:",
          "  " ++ foldr1 (\a b -> a ++ " -> " ++ b) (map Mod.moduleIdDisplay cycleChain)
        ]

lookupLoadedModule :: ModuleGraph -> Mod.ModuleId -> Either String CompUnit
lookupLoadedModule graph modulePath =
  maybe
    (Left ("Internal error: module not loaded: " ++ Mod.moduleIdDisplay modulePath))
    (Right . loadedCompUnit)
    (Map.lookup modulePath (modules graph))

lookupModuleReference :: ModuleGraph -> Mod.ModuleId -> ModulePath -> Either String Mod.ModuleId
lookupModuleReference graph modulePath refPath = do
  loadedModule <- lookupLoadedModuleEntry graph modulePath
  maybe
    (Left ("Internal error: unresolved module reference: " ++ Mod.modulePathDisplay refPath))
    Right
    (Map.lookup refPath (loadedModuleRefs loadedModule))

lookupLoadedModuleEntry :: ModuleGraph -> Mod.ModuleId -> Either String LoadedModule
lookupLoadedModuleEntry graph modulePath =
  maybe
    (Left ("Internal error: module not loaded: " ++ Mod.moduleIdDisplay modulePath))
    Right
    (Map.lookup modulePath (modules graph))

exportModulePaths :: CompUnit -> [ModulePath]
exportModulePaths =
  uniqueModulePaths . concatMap topDeclExportModulePaths . topDeclsFrom

topDeclExportModulePaths :: TopDecl -> [ModulePath]
topDeclExportModulePaths (TExportDecl exportDecl) =
  exportDeclModulePaths exportDecl
topDeclExportModulePaths _ =
  []

exportDeclModulePaths :: Export -> [ModulePath]
exportDeclModulePaths (ExportList specs) =
  [path | ExportModuleAll path <- specs]
exportDeclModulePaths (ExportModule path) =
  [path]
exportDeclModulePaths (ExportModuleAs path _) =
  [path]
exportDeclModulePaths (ExportItemsFrom path _) =
  [path]

uniqueResolvedModules :: [(Mod.ModuleId, FilePath)] -> [(Mod.ModuleId, FilePath)]
uniqueResolvedModules =
  reverse . fst . foldl step ([], Set.empty)
  where
    step (acc, seen) pair@(moduleId, _)
      | moduleId `Set.member` seen = (acc, seen)
      | otherwise = (pair : acc, Set.insert moduleId seen)

moduleSourcePath :: ModuleGraph -> Mod.ModuleId -> Either String FilePath
moduleSourcePath graph modulePath =
  maybe
    (Left ("Internal error: module not loaded: " ++ Mod.moduleIdDisplay modulePath))
    (Right . loadedSourcePath)
    (Map.lookup modulePath (modules graph))

moduleImportPairsFor :: ModuleGraph -> Mod.ModuleId -> CompUnit -> [(Import, Mod.ModuleId)]
moduleImportPairsFor graph modulePath unit =
  zip (imports unit) (Map.findWithDefault [] modulePath (dependencies graph))

data ExportedItemRef
  = ExportedLocalItem Name
  | ExportedRemoteItem Mod.ModuleId Name
  deriving (Eq, Show)

data ExportedModuleBinding
  = ExportedModuleBinding
  { exportedModuleName :: Name,
    exportedModuleTarget :: Mod.ModuleId
  }
  deriving (Eq, Show)

data ModulePublicInterface
  = ModulePublicInterface
  { publicItemRefs :: [ExportedItemRef],
    publicModuleBindings :: [ExportedModuleBinding]
  }
  deriving (Eq, Show)

emptyPublicInterface :: ModulePublicInterface
emptyPublicInterface =
  ModulePublicInterface
    { publicItemRefs = [],
      publicModuleBindings = []
    }

prepareFlattenContext :: ModuleGraph -> Mod.ModuleId -> Either String (CompUnit, FilePath, [(Import, Mod.ModuleId)])
prepareFlattenContext graph modulePath = do
  unit <- lookupLoadedModule graph modulePath
  sourcePath <- moduleSourcePath graph modulePath
  _ <- publicModuleInterface graph modulePath
  let importPairs = moduleImportPairsFor graph modulePath unit
  ensureNoAmbiguousSelectedImports graph importPairs
  ensureNoDuplicateModuleQualifiers unit
  ensureNoDuplicateSelectedItems unit
  ensureImportItemsExist graph importPairs
  pure (unit, sourcePath, importPairs)

flattenModuleValidationCompUnit :: ModuleGraph -> Mod.ModuleId -> Either String CompUnit
flattenModuleValidationCompUnit graph modulePath = do
  (unit, _sourcePath, importPairs) <- prepareFlattenContext graph modulePath
  collidingTypeNames <- collidingImportedTypeNames graph importPairs
  importedDecls <- concat <$> mapM (importedDeclsFor graph) importPairs
  qualifiedDecls <- concat <$> mapM (qualifiedImportDecls collidingTypeNames graph) importPairs
  pure (CompUnit (imports unit) (qualifiedDecls ++ importedDecls ++ topDeclsFrom unit))

flattenModuleStrictCompileCompUnit :: ModuleGraph -> Mod.ModuleId -> Either String CompUnit
flattenModuleStrictCompileCompUnit graph modulePath =
  fst <$> flattenModuleStrictCompileCompUnitWithImportedStart graph modulePath

flattenModuleStrictCompileCompUnitWithImportedStart ::
  ModuleGraph ->
  Mod.ModuleId ->
  Either String (CompUnit, Int)
flattenModuleStrictCompileCompUnitWithImportedStart graph modulePath = do
  (unit, _sourcePath, importPairs) <- prepareFlattenContext graph modulePath
  collidingTypeNames <- collidingImportedTypeNames graph importPairs
  importedDecls <- concat <$> mapM (strictCompileImportedDecls collidingTypeNames graph) importPairs
  qualifiedDecls <- concat <$> mapM (qualifiedImportDecls collidingTypeNames graph) importPairs
  let localDecls = topDeclsFrom unit
      visibleImportedDecls = shadowImportedDecls localDecls importedDecls
      importedStart = length qualifiedDecls + length localDecls
  pure
    ( CompUnit (imports unit) (qualifiedDecls ++ localDecls ++ visibleImportedDecls),
      importedStart
    )

flattenModuleStrictValidationCompUnit :: ModuleGraph -> Mod.ModuleId -> Either String CompUnit
flattenModuleStrictValidationCompUnit graph modulePath = do
  (unit, _sourcePath, importPairs) <- prepareFlattenContext graph modulePath
  importedDecls <- concat <$> mapM (strictValidationImportedDecls graph) importPairs
  qualifiedDecls <- concat <$> mapM (qualifiedImportStubDecls graph) importPairs
  let localDecls = topDeclsFrom unit
      visibleImportedDecls = shadowImportedDecls localDecls importedDecls
  pure (CompUnit (imports unit) (qualifiedDecls ++ localDecls ++ visibleImportedDecls))

ensureImportItemsExist :: ModuleGraph -> [(Import, Mod.ModuleId)] -> Either String ()
ensureImportItemsExist graph importPairs = do
  unknownGroups <- mapM unknowns importPairs
  case concat unknownGroups of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Unknown selected imports:",
            unlines xs
          ]
  where
    unknowns (ImportOnly importPath items, modulePath) = do
      selected <- resolveSelectedImportItems graph importPath modulePath items
      available <- importableNamesForModule graph modulePath
      let missing = filter (`notElem` available) selected
      pure [formatMissing importPath n | n <- missing]
    unknowns _ = pure []

formatMissing :: ModulePath -> Name -> String
formatMissing importPath itemName =
  "  " ++ Mod.modulePathDisplay importPath ++ "." ++ show itemName

resolveSelectedImportItems :: ModuleGraph -> ModulePath -> Mod.ModuleId -> ItemSelector -> Either String [Name]
resolveSelectedImportItems graph _moduleName modulePath SelectAll =
  importableNamesForModule graph modulePath
resolveSelectedImportItems _graph _moduleName _modulePath (SelectOnly items) =
  Right items

importableNamesForModule :: ModuleGraph -> Mod.ModuleId -> Either String [Name]
importableNamesForModule graph modulePath = do
  publicDecls <- publicItemDeclsForModule graph modulePath
  pure (uniqueNames (concatMap topDeclNames publicDecls))

publicItemDeclsForModule :: ModuleGraph -> Mod.ModuleId -> Either String [TopDecl]
publicItemDeclsForModule graph modulePath = do
  publicInterface <- publicModuleInterface graph modulePath
  unit <- lookupLoadedModule graph modulePath
  let localDecls =
        selectPublicItemDecls
          [name | ExportedLocalItem name <- publicItemRefs publicInterface]
          (topDeclsFrom unit)
  remoteDecls <- concat <$> mapM materializeRemoteDecls (groupRemoteItemRefs (publicItemRefs publicInterface))
  pure (localDecls ++ remoteDecls)
  where
    materializeRemoteDecls (targetModule, names) = do
      remoteDecls <- publicItemDeclsForModule graph targetModule
      pure (selectPublicItemDecls names remoteDecls)

publicTopDeclsForModule :: ModuleGraph -> Mod.ModuleId -> Either String [TopDecl]
publicTopDeclsForModule graph modulePath = do
  publicDecls <- publicItemDeclsForModule graph modulePath
  unit <- lookupLoadedModule graph modulePath
  pure (publicDecls ++ [decl | decl@(TInstDef _) <- topDeclsFrom unit])

publicModuleInterface :: ModuleGraph -> Mod.ModuleId -> Either String ModulePublicInterface
publicModuleInterface graph modulePath = do
  unit <- lookupLoadedModule graph modulePath
  sourcePath <- moduleSourcePath graph modulePath
  expandedDecls <-
    mapM
      (expandExportDecl graph modulePath sourcePath unit)
      [exportDecl | TExportDecl exportDecl <- topDeclsFrom unit]
  let publicInterface =
        ModulePublicInterface
          { publicItemRefs = concatMap publicItemRefs expandedDecls,
            publicModuleBindings = concatMap publicModuleBindings expandedDecls
          }
  ensureNoDuplicateExportedItems sourcePath (publicItemRefs publicInterface)
  ensureNoDuplicateExportedModules sourcePath (publicModuleBindings publicInterface)
  pure publicInterface

publicModuleBindingsForModule :: ModuleGraph -> Mod.ModuleId -> Either String [ExportedModuleBinding]
publicModuleBindingsForModule graph modulePath =
  publicModuleBindings <$> publicModuleInterface graph modulePath

expandExportDecl ::
  ModuleGraph ->
  Mod.ModuleId ->
  FilePath ->
  CompUnit ->
  Export ->
  Either String ModulePublicInterface
expandExportDecl graph currentModule sourcePath unit (ExportList specs) = do
  expandedSpecs <- mapM (expandExportSpec graph currentModule sourcePath unit) specs
  pure
    ModulePublicInterface
      { publicItemRefs = concatMap publicItemRefs expandedSpecs,
        publicModuleBindings = concatMap publicModuleBindings expandedSpecs
      }
expandExportDecl graph currentModule _sourcePath _unit (ExportModule path) = do
  targetModule <- lookupModuleReference graph currentModule path
  pure
    emptyPublicInterface
      { publicModuleBindings =
          [ExportedModuleBinding (defaultModuleBindingName path) targetModule]
      }
expandExportDecl graph currentModule _sourcePath _unit (ExportModuleAs path aliasName) = do
  targetModule <- lookupModuleReference graph currentModule path
  pure
    emptyPublicInterface
      { publicModuleBindings = [ExportedModuleBinding aliasName targetModule]
      }
expandExportDecl graph currentModule sourcePath _unit (ExportItemsFrom path selector) = do
  itemRefs <- resolveRemoteExportItems graph currentModule sourcePath path selector
  pure emptyPublicInterface {publicItemRefs = itemRefs}

expandExportSpec ::
  ModuleGraph ->
  Mod.ModuleId ->
  FilePath ->
  CompUnit ->
  ExportSpec ->
  Either String ModulePublicInterface
expandExportSpec _graph _currentModule sourcePath unit (ExportName name) = do
  ensureLocalExportExists sourcePath (topDeclsFrom unit) name
  pure emptyPublicInterface {publicItemRefs = [ExportedLocalItem name]}
expandExportSpec _graph _currentModule _sourcePath unit ExportAll =
  pure
    emptyPublicInterface
      { publicItemRefs = map ExportedLocalItem (availableExportNames (topDeclsFrom unit))
      }
expandExportSpec graph currentModule sourcePath _unit (ExportModuleAll path) = do
  itemRefs <- resolveRemoteExportItems graph currentModule sourcePath path SelectAll
  pure emptyPublicInterface {publicItemRefs = itemRefs}

resolveRemoteExportItems ::
  ModuleGraph ->
  Mod.ModuleId ->
  FilePath ->
  ModulePath ->
  ItemSelector ->
  Either String [ExportedItemRef]
resolveRemoteExportItems graph currentModule sourcePath exportPath selector = do
  targetModule <- lookupModuleReference graph currentModule exportPath
  case selector of
    SelectAll -> do
      exportedNames <- importableNamesForModule graph targetModule
      pure [ExportedRemoteItem targetModule name | name <- exportedNames]
    SelectOnly names -> do
      availableNames <- importableNamesForModule graph targetModule
      ensureRemoteExportsExist sourcePath exportPath names availableNames
      pure [ExportedRemoteItem targetModule name | name <- names]

availableExportNames :: [TopDecl] -> [Name]
availableExportNames ds =
  uniqueNames (concatMap topDeclNames (filter isImportableTopDecl ds))

ensureNoDuplicateExportedItems :: FilePath -> [ExportedItemRef] -> Either String ()
ensureNoDuplicateExportedItems modulePath itemRefs =
  case duplicates of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Duplicate exported item names:",
            "  " ++ modulePath,
            unlines (map (\n -> "  " ++ show n) xs)
          ]
  where
    duplicates = duplicateNames (map exportedItemName itemRefs)

ensureNoDuplicateExportedModules :: FilePath -> [ExportedModuleBinding] -> Either String ()
ensureNoDuplicateExportedModules modulePath moduleBindings =
  case duplicates of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Duplicate exported module names:",
            "  " ++ modulePath,
            unlines (map (\n -> "  " ++ show n) xs)
          ]
  where
    duplicates = duplicateNames [exportedModuleName binding | binding <- moduleBindings]

ensureLocalExportExists :: FilePath -> [TopDecl] -> Name -> Either String ()
ensureLocalExportExists sourcePath ds itemName
  | itemName `elem` availableExportNames ds = Right ()
  | otherwise =
      Left $
        unlines
          [ "Unknown export:",
            "  " ++ sourcePath,
            "  " ++ show itemName
          ]

ensureRemoteExportsExist :: FilePath -> ModulePath -> [Name] -> [Name] -> Either String ()
ensureRemoteExportsExist sourcePath exportPath names availableNames =
  case missing of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Unknown re-exported names:",
            "  " ++ sourcePath,
            unlines [formatMissing exportPath name | name <- xs]
          ]
  where
    missing = filter (`notElem` availableNames) names

exportedItemName :: ExportedItemRef -> Name
exportedItemName (ExportedLocalItem name) = name
exportedItemName (ExportedRemoteItem _ name) = name

groupRemoteItemRefs :: [ExportedItemRef] -> [(Mod.ModuleId, [Name])]
groupRemoteItemRefs =
  reverse . fst . foldl step ([], Map.empty)
  where
    step (acc, seen) (ExportedLocalItem _) = (acc, seen)
    step (acc, seen) (ExportedRemoteItem moduleId itemName) =
      case Map.lookup moduleId seen of
        Just names ->
          ( replaceAssoc moduleId (names ++ [itemName]) acc,
            Map.insert moduleId (names ++ [itemName]) seen
          )
        Nothing ->
          ( (moduleId, [itemName]) : acc,
            Map.insert moduleId [itemName] seen
          )

    replaceAssoc moduleId names =
      map (\(currentModule, currentNames) -> if currentModule == moduleId then (currentModule, names) else (currentModule, currentNames))

defaultModuleBindingName :: ModulePath -> Name
defaultModuleBindingName =
  moduleLeafName . Mod.modulePathName

moduleLeafName :: Name -> Name
moduleLeafName (Name n) = Name n
moduleLeafName (QualName _ n) = Name n

importModuleQualifiers :: ModulePath -> [Name]
importModuleQualifiers importPath =
  uniqueNames [defaultModuleBindingName importPath, Mod.modulePathName importPath]

selectPublicItemDecls :: [Name] -> [TopDecl] -> [TopDecl]
selectPublicItemDecls names =
  mapMaybe (selectTopDecl names) . filter isPublicItemTopDecl

isPublicItemTopDecl :: TopDecl -> Bool
isPublicItemTopDecl (TInstDef _) = False
isPublicItemTopDecl d = isImportableTopDecl d

isImportableTopDecl :: TopDecl -> Bool
isImportableTopDecl (TPragmaDecl _) = False
isImportableTopDecl (TExportDecl _) = False
isImportableTopDecl _ = True

topDeclNames :: TopDecl -> [Name]
topDeclNames (TFunDef (FunDef sig _)) = [sigName sig]
topDeclNames (TSym (TySym n _ _)) = [n]
topDeclNames (TClassDef (Class _ _ n _ _ _)) = [n]
topDeclNames (TContr (Contract n _ _)) = [n]
topDeclNames (TDataDef (DataTy n _ cs)) = n : map constrName cs
topDeclNames (TInstDef _) = []
topDeclNames (TExportDecl _) = []
topDeclNames (TPragmaDecl _) = []

qualifiedImportDecls :: Set Name -> ModuleGraph -> (Import, Mod.ModuleId) -> Either String [TopDecl]
qualifiedImportDecls collidingTypeNames graph (imp, modulePath) =
  case imp of
    ImportOnly _ _ -> Right []
    ImportModule importPath ->
      concat <$> mapM (`qualifyDecls` modulePath) (importModuleQualifiers importPath)
    ImportAlias _ qualifier ->
      qualifyDecls qualifier modulePath
  where
    qualifyDecls qualifier targetModule = do
      moduleBindings <- publicModuleBindingsForModule graph targetModule
      nestedDecls <- concat <$> mapM (qualifyNestedModule qualifier) moduleBindings
      publicDecls <- publicTopDeclsForModule graph targetModule
      allDecls <- importableTopDeclsForCompiledModule graph targetModule
      let typeRenameMap = importedTypeRenameMap collidingTypeNames qualifier publicDecls
          publicUnit = CompUnit [] publicDecls
          allUnit = CompUnit [] allDecls
      pure $
        qualifiedFunctionDecls typeRenameMap qualifier publicUnit allUnit
          ++ qualifiedTypeAliasDecls typeRenameMap qualifier publicUnit
          ++ nestedDecls

    qualifyNestedModule qualifier (ExportedModuleBinding bindingName targetModule) =
      qualifyDecls (QualName qualifier (show bindingName)) targetModule

importableTopDeclsForCompiledModule :: ModuleGraph -> Mod.ModuleId -> Either String [TopDecl]
importableTopDeclsForCompiledModule graph modulePath = do
  cunit <- flattenModuleStrictCompileCompUnit graph modulePath
  publicInterface <- publicModuleInterface graph modulePath
  let localDecls = filter isImportableTopDecl (topDeclsFrom cunit)
  remoteDecls <- concat <$> mapM (compileSupportForRemoteItemGroup graph) (groupRemoteItemRefs (publicItemRefs publicInterface))
  pure (localDecls ++ shadowImportedDecls localDecls remoteDecls)

compileSupportForRemoteItemGroup :: ModuleGraph -> (Mod.ModuleId, [Name]) -> Either String [TopDecl]
compileSupportForRemoteItemGroup graph (targetModule, names) = do
  publicDecls <- publicTopDeclsForModule graph targetModule
  allDecls <- importableTopDeclsForCompiledModule graph targetModule
  let selectedPublicDecls = mapMaybe (selectTopDecl names) publicDecls
      allFunctionDecls = [fd | TFunDef fd <- allDecls]
      supportNonFunctionDecls = filter (not . isFunctionTopDecl) allDecls
      allFunctionNames = Set.fromList [sigName (funSignature fd) | fd <- allFunctionDecls]
      depMap =
        Map.fromList
          [ ( sigName (funSignature fd),
              filter (`Set.member` allFunctionNames) (funDefFunctionRefs fd)
            )
            | fd <- allFunctionDecls
          ]
      selectedFunctionNames =
        [ sigName (funSignature fd)
          | TFunDef fd <- selectedPublicDecls
        ]
      requiredFunctionNames = Set.fromList (functionDependencyClosure depMap selectedFunctionNames)
      requiredFunctionDecls =
        [ TFunDef fd
          | fd <- allFunctionDecls,
            sigName (funSignature fd) `Set.member` requiredFunctionNames
        ]
  pure (requiredFunctionDecls ++ shadowImportedDecls requiredFunctionDecls supportNonFunctionDecls)

qualifiedImportStubDecls :: ModuleGraph -> (Import, Mod.ModuleId) -> Either String [TopDecl]
qualifiedImportStubDecls graph (imp, modulePath) =
  case imp of
    ImportOnly _ _ -> Right []
    ImportModule importPath ->
      concat <$> mapM (`stubDecls` modulePath) (importModuleQualifiers importPath)
    ImportAlias _ qualifier ->
      stubDecls qualifier modulePath
  where
    stubDecls qualifier targetModule = do
      moduleBindings <- publicModuleBindingsForModule graph targetModule
      nestedDecls <- concat <$> mapM (stubNestedModule qualifier) moduleBindings
      publicDecls <- publicTopDeclsForModule graph targetModule
      let cunit = CompUnit [] publicDecls
      pure $
        qualifiedFunctionStubDecls qualifier cunit
          ++ qualifiedTypeStubDecls qualifier cunit
          ++ nestedDecls

    stubNestedModule qualifier (ExportedModuleBinding bindingName targetModule) =
      stubDecls (QualName qualifier (show bindingName)) targetModule

qualifyFunctionWrapper :: Name -> FunDef -> FunDef
qualifyFunctionWrapper qualifier (FunDef sig _body) =
  FunDef
    (sig {sigName = qualifiedName})
    wrapperBody
  where
    originalName = sigName sig
    qualifiedName = QualName qualifier (show originalName)
    implName = hiddenFunctionName qualifier originalName
    wrapperBody = forwardingWrapperBody sig implName

qualifyRevertFunction :: Name -> FunDef -> FunDef
qualifyRevertFunction qualifier (FunDef sig body) =
  FunDef
    (sig {sigName = QualName qualifier (show (sigName sig))})
    body

qualifyFunctionImpl :: Map Name Name -> Name -> FunDef -> FunDef
qualifyFunctionImpl renameMap qualifier (FunDef sig body) =
  FunDef
    (sig {sigName = hiddenFunctionName qualifier (sigName sig)})
    (renameBodyFunctionCalls renameMap body)

hiddenFunctionName :: Name -> Name -> Name
hiddenFunctionName qualifier originalName =
  QualName qualifier ("$impl$" ++ flattenName originalName)

flattenName :: Name -> String
flattenName (Name n) = n
flattenName (QualName q n) = flattenName q ++ "_" ++ n

sigParamName :: Param -> Name
sigParamName (Typed n _) = n
sigParamName (Untyped n) = n

forwardingWrapperBody :: Signature -> Name -> Body
forwardingWrapperBody sig targetName =
  [Return (ExpName Nothing targetName (map (ExpVar Nothing) argNames))]
  where
    argNames = map sigParamName (sigParams sig)

qualifiedFunctionDecls :: Map Name Name -> Name -> CompUnit -> CompUnit -> [TopDecl]
qualifiedFunctionDecls typeRenameMap qualifier publicUnit allUnit =
  concatMap qualifyImpl allFds ++ concatMap qualifyWrapper exportedFds
  where
    allFds = [fd | TFunDef fd <- topDeclsFrom allUnit]
    exportedNames = [sigName (funSignature fd) | TFunDef fd <- topDeclsFrom publicUnit]
    exportedFds =
      [ fd
        | fd <- allFds,
          sigName (funSignature fd) `elem` exportedNames
      ]
    renameMap =
      Map.fromList
        [ ( sigName (funSignature fd),
            hiddenFunctionName qualifier (sigName (funSignature fd))
          )
          | fd <- allFds,
            sigName (funSignature fd) /= Name "revert"
        ]
    qualifyImpl fd
      | sigName (funSignature fd') == Name "revert" =
          [TFunDef (qualifyRevertFunction qualifier fd')]
      | otherwise =
          [TFunDef (qualifyFunctionImpl renameMap qualifier fd')]
      where
        fd' = renameFunDefTypeRefs typeRenameMap fd
    qualifyWrapper fd
      | sigName (funSignature fd') == Name "revert" =
          []
      | otherwise =
          [TFunDef (qualifyFunctionWrapper qualifier fd')]
      where
        fd' = renameFunDefTypeRefs typeRenameMap fd

qualifiedFunctionStubDecls :: Name -> CompUnit -> [TopDecl]
qualifiedFunctionStubDecls qualifier cunit =
  [ TFunDef (stubFunction (QualName qualifier (show (sigName (funSignature fd)))))
    | TFunDef fd <- topDeclsFrom cunit
  ]

renameBodyFunctionCalls :: Map Name Name -> Body -> Body
renameBodyFunctionCalls renameMap =
  map (renameStmtFunctionCalls renameMap)

renameStmtFunctionCalls :: Map Name Name -> Stmt -> Stmt
renameStmtFunctionCalls renameMap (Assign lhs rhs) =
  Assign (renameExpFunctionCalls renameMap lhs) (renameExpFunctionCalls renameMap rhs)
renameStmtFunctionCalls renameMap (StmtPlusEq e1 e2) =
  StmtPlusEq (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameStmtFunctionCalls renameMap (StmtMinusEq e1 e2) =
  StmtMinusEq (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameStmtFunctionCalls renameMap (Let n mt me) =
  Let n mt (renameExpFunctionCalls renameMap <$> me)
renameStmtFunctionCalls renameMap (StmtExp e) =
  StmtExp (renameExpFunctionCalls renameMap e)
renameStmtFunctionCalls renameMap (Return e) =
  Return (renameExpFunctionCalls renameMap e)
renameStmtFunctionCalls renameMap (Match es eqns) =
  Match
    (map (renameExpFunctionCalls renameMap) es)
    (map (renameEquationFunctionCalls renameMap) eqns)
renameStmtFunctionCalls _ stmt@(Asm _) = stmt
renameStmtFunctionCalls renameMap (If e blk1 blk2) =
  If
    (renameExpFunctionCalls renameMap e)
    (renameBodyFunctionCalls renameMap blk1)
    (renameBodyFunctionCalls renameMap blk2)

renameEquationFunctionCalls :: Map Name Name -> Equation -> Equation
renameEquationFunctionCalls renameMap (ps, body) =
  (ps, renameBodyFunctionCalls renameMap body)

renameExpFunctionCalls :: Map Name Name -> Exp -> Exp
renameExpFunctionCalls _ litExp@(Lit _) = litExp
renameExpFunctionCalls renameMap (ExpName me n es) =
  case qualifiedMemberName me n of
    Just qn ->
      case Map.lookup qn renameMap of
        Just renamedName -> ExpName Nothing renamedName es'
        Nothing -> ExpName me' n es'
    Nothing ->
      ExpName me' n' es'
  where
    me' = fmap (renameExpFunctionCalls renameMap) me
    n'
      | me == Nothing = Map.findWithDefault n n renameMap
      | otherwise = n
    es' = map (renameExpFunctionCalls renameMap) es
renameExpFunctionCalls renameMap (ExpVar me n) =
  case qualifiedMemberName me n of
    Just qn ->
      case Map.lookup qn renameMap of
        Just renamedName -> ExpVar Nothing renamedName
        Nothing -> ExpVar me' n
    Nothing ->
      ExpVar me' n
  where
    me' = fmap (renameExpFunctionCalls renameMap) me
renameExpFunctionCalls renameMap (ExpDotName n es) =
  ExpDotName n (map (renameExpFunctionCalls renameMap) es)
renameExpFunctionCalls renameMap (Lam ps bd mt) =
  Lam ps (renameBodyFunctionCalls renameMap bd) mt
renameExpFunctionCalls renameMap (TyExp e ty) =
  TyExp (renameExpFunctionCalls renameMap e) ty
renameExpFunctionCalls renameMap (ExpIndexed e1 e2) =
  ExpIndexed (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameExpFunctionCalls renameMap (ExpPlus e1 e2) =
  ExpPlus (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameExpFunctionCalls renameMap (ExpMinus e1 e2) =
  ExpMinus (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameExpFunctionCalls renameMap (ExpTimes e1 e2) =
  ExpTimes (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameExpFunctionCalls renameMap (ExpDivide e1 e2) =
  ExpDivide (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameExpFunctionCalls renameMap (ExpModulo e1 e2) =
  ExpModulo (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameExpFunctionCalls renameMap (ExpLT e1 e2) =
  ExpLT (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameExpFunctionCalls renameMap (ExpGT e1 e2) =
  ExpGT (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameExpFunctionCalls renameMap (ExpLE e1 e2) =
  ExpLE (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameExpFunctionCalls renameMap (ExpGE e1 e2) =
  ExpGE (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameExpFunctionCalls renameMap (ExpEE e1 e2) =
  ExpEE (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameExpFunctionCalls renameMap (ExpNE e1 e2) =
  ExpNE (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameExpFunctionCalls renameMap (ExpLAnd e1 e2) =
  ExpLAnd (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameExpFunctionCalls renameMap (ExpLOr e1 e2) =
  ExpLOr (renameExpFunctionCalls renameMap e1) (renameExpFunctionCalls renameMap e2)
renameExpFunctionCalls renameMap (ExpLNot e) =
  ExpLNot (renameExpFunctionCalls renameMap e)
renameExpFunctionCalls renameMap (ExpCond e1 e2 e3) =
  ExpCond
    (renameExpFunctionCalls renameMap e1)
    (renameExpFunctionCalls renameMap e2)
    (renameExpFunctionCalls renameMap e3)

qualifiedMemberName :: Maybe Exp -> Name -> Maybe Name
qualifiedMemberName me n =
  QualName <$> (me >>= qualifierFromExpVarChain) <*> pure (show n)

qualifierFromExpVarChain :: Exp -> Maybe Name
qualifierFromExpVarChain (ExpVar Nothing n) =
  Just n
qualifierFromExpVarChain (ExpVar (Just e) n) = do
  q <- qualifierFromExpVarChain e
  pure (QualName q (show n))
qualifierFromExpVarChain _ =
  Nothing

renameTopDeclTypeRefs :: Map Name Name -> TopDecl -> TopDecl
renameTopDeclTypeRefs renameMap (TFunDef fd) =
  TFunDef (renameFunDefTypeRefs renameMap fd)
renameTopDeclTypeRefs renameMap (TClassDef c) =
  TClassDef (renameClassTypeRefs renameMap c)
renameTopDeclTypeRefs renameMap (TInstDef i) =
  TInstDef (renameInstanceTypeRefs renameMap i)
renameTopDeclTypeRefs renameMap (TContr c) =
  TContr (renameContractTypeRefs renameMap c)
renameTopDeclTypeRefs renameMap (TDataDef d) =
  TDataDef (renameDataTyTypeRefs renameMap d)
renameTopDeclTypeRefs renameMap (TSym s) =
  TSym (renameTySymTypeRefs renameMap s)
renameTopDeclTypeRefs _ d = d

renameFunDefTypeRefs :: Map Name Name -> FunDef -> FunDef
renameFunDefTypeRefs renameMap (FunDef sig body) =
  FunDef
    (renameSignatureTypeRefs renameMap sig)
    (renameBodyTypeRefs renameMap body)

renameSignatureTypeRefs :: Map Name Name -> Signature -> Signature
renameSignatureTypeRefs renameMap sig =
  sig
    { sigVars = map (renameTyTypeRefs renameMap) (sigVars sig),
      sigContext = map (renamePredTypeRefs renameMap) (sigContext sig),
      sigParams = map (renameParamTypeRefs renameMap) (sigParams sig),
      sigReturn = renameTyTypeRefs renameMap <$> sigReturn sig
    }

renameParamTypeRefs :: Map Name Name -> Param -> Param
renameParamTypeRefs renameMap (Typed n ty) =
  Typed n (renameTyTypeRefs renameMap ty)
renameParamTypeRefs _ p@(Untyped _) = p

renameBodyTypeRefs :: Map Name Name -> Body -> Body
renameBodyTypeRefs renameMap =
  map (renameStmtTypeRefs renameMap)

renameStmtTypeRefs :: Map Name Name -> Stmt -> Stmt
renameStmtTypeRefs renameMap (Assign lhs rhs) =
  Assign (renameExpTypeRefs renameMap lhs) (renameExpTypeRefs renameMap rhs)
renameStmtTypeRefs renameMap (StmtPlusEq e1 e2) =
  StmtPlusEq (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameStmtTypeRefs renameMap (StmtMinusEq e1 e2) =
  StmtMinusEq (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameStmtTypeRefs renameMap (Let n mt me) =
  Let n (renameTyTypeRefs renameMap <$> mt) (renameExpTypeRefs renameMap <$> me)
renameStmtTypeRefs renameMap (StmtExp e) =
  StmtExp (renameExpTypeRefs renameMap e)
renameStmtTypeRefs renameMap (Return e) =
  Return (renameExpTypeRefs renameMap e)
renameStmtTypeRefs renameMap (Match es eqns) =
  Match
    (map (renameExpTypeRefs renameMap) es)
    (map (renameEquationTypeRefs renameMap) eqns)
renameStmtTypeRefs _ stmt@(Asm _) = stmt
renameStmtTypeRefs renameMap (If e blk1 blk2) =
  If
    (renameExpTypeRefs renameMap e)
    (renameBodyTypeRefs renameMap blk1)
    (renameBodyTypeRefs renameMap blk2)

renameEquationTypeRefs :: Map Name Name -> Equation -> Equation
renameEquationTypeRefs renameMap (ps, body) =
  (map (renamePatTypeRefs renameMap) ps, renameBodyTypeRefs renameMap body)

renamePatTypeRefs :: Map Name Name -> Pat -> Pat
renamePatTypeRefs renameMap (Pat n ps) =
  Pat (renamePatNameTypeRefs renameMap n) (map (renamePatTypeRefs renameMap) ps)
renamePatTypeRefs renameMap (PatDot n ps) =
  PatDot n (map (renamePatTypeRefs renameMap) ps)
renamePatTypeRefs _ p@(PWildcard) = p
renamePatTypeRefs _ p@(PLit _) = p

renamePatNameTypeRefs :: Map Name Name -> Name -> Name
renamePatNameTypeRefs renameMap (QualName q n) =
  QualName (renameTypeName renameMap q) n
renamePatNameTypeRefs renameMap n =
  case Map.lookup n renameMap of
    Just qn -> QualName qn (show n)
    Nothing -> n

renameExpTypeRefs :: Map Name Name -> Exp -> Exp
renameExpTypeRefs _ litExp@(Lit _) = litExp
renameExpTypeRefs renameMap (ExpName Nothing n es) =
  ExpName
    (sameNameConstructorQualifier renameMap n)
    n
    (map (renameExpTypeRefs renameMap) es)
renameExpTypeRefs renameMap (ExpName me n es) =
  ExpName
    (renameMemberQualifierTypeRefs renameMap <$> me)
    n
    (map (renameExpTypeRefs renameMap) es)
renameExpTypeRefs renameMap (ExpVar Nothing n) =
  ExpVar
    (sameNameConstructorQualifier renameMap n)
    n
renameExpTypeRefs renameMap (ExpVar me n) =
  ExpVar
    (renameMemberQualifierTypeRefs renameMap <$> me)
    n
renameExpTypeRefs renameMap (ExpDotName n es) =
  ExpDotName n (map (renameExpTypeRefs renameMap) es)
renameExpTypeRefs renameMap (Lam ps bd mt) =
  Lam
    (map (renameParamTypeRefs renameMap) ps)
    (renameBodyTypeRefs renameMap bd)
    (renameTyTypeRefs renameMap <$> mt)
renameExpTypeRefs renameMap (TyExp e ty) =
  TyExp (renameExpTypeRefs renameMap e) (renameTyTypeRefs renameMap ty)
renameExpTypeRefs renameMap (ExpIndexed e1 e2) =
  ExpIndexed (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameExpTypeRefs renameMap (ExpPlus e1 e2) =
  ExpPlus (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameExpTypeRefs renameMap (ExpMinus e1 e2) =
  ExpMinus (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameExpTypeRefs renameMap (ExpTimes e1 e2) =
  ExpTimes (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameExpTypeRefs renameMap (ExpDivide e1 e2) =
  ExpDivide (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameExpTypeRefs renameMap (ExpModulo e1 e2) =
  ExpModulo (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameExpTypeRefs renameMap (ExpLT e1 e2) =
  ExpLT (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameExpTypeRefs renameMap (ExpGT e1 e2) =
  ExpGT (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameExpTypeRefs renameMap (ExpLE e1 e2) =
  ExpLE (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameExpTypeRefs renameMap (ExpGE e1 e2) =
  ExpGE (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameExpTypeRefs renameMap (ExpEE e1 e2) =
  ExpEE (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameExpTypeRefs renameMap (ExpNE e1 e2) =
  ExpNE (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameExpTypeRefs renameMap (ExpLAnd e1 e2) =
  ExpLAnd (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameExpTypeRefs renameMap (ExpLOr e1 e2) =
  ExpLOr (renameExpTypeRefs renameMap e1) (renameExpTypeRefs renameMap e2)
renameExpTypeRefs renameMap (ExpLNot e) =
  ExpLNot (renameExpTypeRefs renameMap e)
renameExpTypeRefs renameMap (ExpCond e1 e2 e3) =
  ExpCond
    (renameExpTypeRefs renameMap e1)
    (renameExpTypeRefs renameMap e2)
    (renameExpTypeRefs renameMap e3)

renameMemberQualifierTypeRefs :: Map Name Name -> Exp -> Exp
renameMemberQualifierTypeRefs renameMap e =
  case qualifierFromExpVarChain e of
    Just q ->
      let q' = renameTypeName renameMap q
       in if q' == q
            then renameExpTypeRefs renameMap e
            else qualifierNameToExp q'
    Nothing ->
      renameExpTypeRefs renameMap e

sameNameConstructorQualifier :: Map Name Name -> Name -> Maybe Exp
sameNameConstructorQualifier renameMap n =
  qualifierNameToExp <$> Map.lookup n renameMap

qualifierNameToExp :: Name -> Exp
qualifierNameToExp (Name n) =
  ExpVar Nothing (Name n)
qualifierNameToExp (QualName q n) =
  ExpVar (Just (qualifierNameToExp q)) (Name n)

renameContractTypeRefs :: Map Name Name -> Contract -> Contract
renameContractTypeRefs renameMap (Contract n ts ds) =
  Contract
    n
    (map (renameTyTypeRefs renameMap) ts)
    (map (renameContractDeclTypeRefs renameMap) ds)

renameContractDeclTypeRefs :: Map Name Name -> ContractDecl -> ContractDecl
renameContractDeclTypeRefs renameMap (CDataDecl d) =
  CDataDecl (renameDataTyTypeRefs renameMap d)
renameContractDeclTypeRefs renameMap (CFieldDecl (Field n ty me)) =
  CFieldDecl
    (Field n (renameTyTypeRefs renameMap ty) (renameExpTypeRefs renameMap <$> me))
renameContractDeclTypeRefs renameMap (CFunDecl fd) =
  CFunDecl (renameFunDefTypeRefs renameMap fd)
renameContractDeclTypeRefs renameMap (CConstrDecl (Constructor ps body)) =
  CConstrDecl
    ( Constructor
        (map (renameParamTypeRefs renameMap) ps)
        (renameBodyTypeRefs renameMap body)
    )

renameClassTypeRefs :: Map Name Name -> Class -> Class
renameClassTypeRefs renameMap (Class bvs ctx n pvs mv sigs) =
  Class
    (map (renameTyTypeRefs renameMap) bvs)
    (map (renamePredTypeRefs renameMap) ctx)
    n
    (map (renameTyTypeRefs renameMap) pvs)
    (renameTyTypeRefs renameMap mv)
    (map (renameSignatureTypeRefs renameMap) sigs)

renameInstanceTypeRefs :: Map Name Name -> Instance -> Instance
renameInstanceTypeRefs renameMap (Instance d vs ctx n pts mt fns) =
  Instance
    d
    (map (renameTyTypeRefs renameMap) vs)
    (map (renamePredTypeRefs renameMap) ctx)
    n
    (map (renameTyTypeRefs renameMap) pts)
    (renameTyTypeRefs renameMap mt)
    (map (renameFunDefTypeRefs renameMap) fns)

renameDataTyTypeRefs :: Map Name Name -> DataTy -> DataTy
renameDataTyTypeRefs renameMap (DataTy n vs cs) =
  DataTy
    (renameTypeName renameMap n)
    (map (renameTyTypeRefs renameMap) vs)
    (map (renameConstrTypeRefs renameMap) cs)

renameConstrTypeRefs :: Map Name Name -> Constr -> Constr
renameConstrTypeRefs renameMap (Constr n tys) =
  Constr (renameConstrNameTypeRefs renameMap n) (map (renameTyTypeRefs renameMap) tys)

renameConstrNameTypeRefs :: Map Name Name -> Name -> Name
renameConstrNameTypeRefs renameMap (QualName q n) =
  QualName (renameTypeName renameMap q) n
renameConstrNameTypeRefs _ n = n

renameTySymTypeRefs :: Map Name Name -> TySym -> TySym
renameTySymTypeRefs renameMap (TySym n vs ty) =
  TySym
    (renameTypeName renameMap n)
    (map (renameTyTypeRefs renameMap) vs)
    (renameTyTypeRefs renameMap ty)

renamePredTypeRefs :: Map Name Name -> Pred -> Pred
renamePredTypeRefs renameMap (InCls n mt pts) =
  InCls
    n
    (renameTyTypeRefs renameMap mt)
    (map (renameTyTypeRefs renameMap) pts)

renameTyTypeRefs :: Map Name Name -> Ty -> Ty
renameTyTypeRefs renameMap (TyCon n tys) =
  TyCon (renameTypeName renameMap n) (map (renameTyTypeRefs renameMap) tys)

renameTypeName :: Map Name Name -> Name -> Name
renameTypeName renameMap n =
  case Map.lookup n renameMap of
    Just n' -> n'
    Nothing ->
      case n of
        QualName q x -> QualName (renameTypeName renameMap q) x
        _ -> n

qualifiedTypeAliasDecls :: Map Name Name -> Name -> CompUnit -> [TopDecl]
qualifiedTypeAliasDecls typeRenameMap qualifier cunit =
  dataAliases ++ symAliases
  where
    dataAliases =
      [ TSym (qualifyTyCon qualifier n vs)
        | TDataDef (DataTy n vs _) <- topDeclsFrom cunit,
          not (Map.member n typeRenameMap)
      ]
    symAliases =
      [ TSym (qualifyTyCon qualifier n vs)
        | TSym (TySym n vs _) <- topDeclsFrom cunit,
          not (Map.member n typeRenameMap)
      ]

qualifiedTypeStubDecls :: Name -> CompUnit -> [TopDecl]
qualifiedTypeStubDecls qualifier cunit =
  dataAliases ++ symAliases
  where
    dataAliases =
      [ TDataDef
          ( DataTy
              (QualName qualifier (show n))
              []
              [Constr (constructorLeafName (constrName c)) [] | c <- cs]
          )
        | TDataDef (DataTy n _ cs) <- topDeclsFrom cunit
      ]
    symAliases =
      [ TSym (stubType (QualName qualifier (show n)))
        | TSym (TySym n _ _) <- topDeclsFrom cunit
      ]

constructorLeafName :: Name -> Name
constructorLeafName (QualName _ n) = Name n
constructorLeafName n = n

qualifyTyCon :: Name -> Name -> [Ty] -> TySym
qualifyTyCon qualifier unqualName tyVars =
  TySym
    { symName = QualName qualifier (show unqualName),
      symVars = tyVars,
      symType = TyCon unqualName tyVars
    }

stubType :: Name -> TySym
stubType n =
  TySym
    { symName = n,
      symVars = [],
      symType = TyCon (Name "word") []
    }

stubFunction :: Name -> FunDef
stubFunction n =
  FunDef
    (Signature [] [] n [] Nothing)
    []

importedDeclsFor :: ModuleGraph -> (Import, Mod.ModuleId) -> Either String [TopDecl]
importedDeclsFor graph (imp, modulePath) =
  applyImportVisibility imp <$> publicTopDeclsForModule graph modulePath

strictValidationImportedDecls :: ModuleGraph -> (Import, Mod.ModuleId) -> Either String [TopDecl]
strictValidationImportedDecls graph (imp, modulePath) =
  case imp of
    ImportOnly _ SelectAll ->
      mapMaybe toValidationImportStub <$> publicTopDeclsForModule graph modulePath
    ImportOnly _ (SelectOnly names) ->
      mapMaybe toValidationImportStub . mapMaybe (selectTopDecl names)
        <$> publicTopDeclsForModule graph modulePath
    ImportModule _ ->
      Right []
    ImportAlias _ _ ->
      Right []

toValidationImportStub :: TopDecl -> Maybe TopDecl
toValidationImportStub (TFunDef (FunDef sig _)) =
  Just (TFunDef (stubFunction (sigName sig)))
toValidationImportStub (TSym (TySym n _ _)) =
  Just (TSym (stubType n))
toValidationImportStub d@(TClassDef _) =
  Just d
toValidationImportStub (TContr (Contract n _ _)) =
  Just (TContr (Contract n [] []))
toValidationImportStub (TDataDef (DataTy n _ cs)) =
  Just (TDataDef (DataTy n [] [Constr (constrName c) [] | c <- cs]))
toValidationImportStub (TInstDef _) = Nothing
toValidationImportStub (TExportDecl _) = Nothing
toValidationImportStub (TPragmaDecl _) = Nothing

strictCompileImportedDecls :: Set Name -> ModuleGraph -> (Import, Mod.ModuleId) -> Either String [TopDecl]
strictCompileImportedDecls collidingTypeNames graph (imp, modulePath) =
  case imp of
    ImportOnly moduleName selector ->
      importOnlyCompileDecls (Mod.modulePathName moduleName) selector
    ImportModule moduleName ->
      moduleImportCompileDecls (Mod.modulePathName moduleName) modulePath
    ImportAlias _ qualifier ->
      moduleImportCompileDecls qualifier modulePath
  where
    importOnlyCompileDecls moduleName selector = do
      publicDecls <- publicTopDeclsForModule graph modulePath
      allDecls <- importableTopDeclsForCompiledModule graph modulePath
      let allFunctionDecls = [fd | TFunDef fd <- allDecls]
          supportNonFunctionDecls = filter (not . isFunctionTopDecl) allDecls
          allFunctionNames = Set.fromList [sigName (funSignature fd) | fd <- allFunctionDecls]
          renameMap = importedFunctionRenameMap moduleName allDecls
          typeRenameMap = importedTypeRenameMap collidingTypeNames moduleName publicDecls
          depMap =
            Map.fromList
              [ ( sigName (funSignature fd),
                  filter (`Set.member` allFunctionNames) (funDefFunctionRefs fd)
                )
                | fd <- allFunctionDecls
              ]
      let selectedPublicDecls =
            case selector of
              SelectAll -> publicDecls
              SelectOnly names -> mapMaybe (selectTopDecl names) publicDecls
          selectedFunctionNames =
            [ sigName (funSignature fd)
              | TFunDef fd <- selectedPublicDecls
            ]
          requiredFunctionNames = Set.fromList (functionDependencyClosure depMap selectedFunctionNames)
          requiredFunctionDecls =
            [ TFunDef fd
              | fd <- allFunctionDecls,
                sigName (funSignature fd) `Set.member` requiredFunctionNames
            ]
          renamedSupportNonFunctionDecls =
            map
              (renameTopDeclTypeRefs typeRenameMap . renameTopDeclFunctionCalls renameMap)
              supportNonFunctionDecls
          functionDecls =
            importOnlyFunctionDecls moduleName selectedFunctionNames requiredFunctionDecls
      pure
        ( functionDecls
            ++ shadowImportedDecls functionDecls renamedSupportNonFunctionDecls
        )

    moduleImportCompileDecls qualifier targetModule = do
      moduleBindings <- publicModuleBindingsForModule graph targetModule
      publicDecls <- publicTopDeclsForModule graph targetModule
      allDecls <- importableTopDeclsForCompiledModule graph targetModule
      let renameMap = importedFunctionRenameMap qualifier allDecls
          typeRenameMap = importedTypeRenameMap collidingTypeNames qualifier publicDecls
          localSupportDecls =
            map
              (renameTopDeclTypeRefs typeRenameMap . renameTopDeclFunctionCalls renameMap)
              (filter (not . isFunctionTopDecl) publicDecls)
      nestedSupportDecls <- concat <$> mapM (nestedModuleImportCompileDecls qualifier) moduleBindings
      pure (localSupportDecls ++ nestedSupportDecls)

    nestedModuleImportCompileDecls qualifier (ExportedModuleBinding bindingName targetModule) =
      moduleImportCompileDecls (QualName qualifier (show bindingName)) targetModule

importedFunctionRenameMap :: Name -> [TopDecl] -> Map Name Name
importedFunctionRenameMap qualifier ds =
  Map.fromList
    [ (sigName (funSignature fd), hiddenFunctionName qualifier (sigName (funSignature fd)))
      | TFunDef fd <- ds,
        sigName (funSignature fd) /= Name "revert"
    ]

importedTypeRenameMap :: Set Name -> Name -> [TopDecl] -> Map Name Name
importedTypeRenameMap collidingTypeNames qualifier ds =
  Map.fromList
    [ (n, QualName qualifier (show n))
      | d <- ds,
        n <- topDeclImportedTypeNames d,
        n `Set.member` collidingTypeNames
    ]

topDeclImportedTypeNames :: TopDecl -> [Name]
topDeclImportedTypeNames (TDataDef (DataTy n _ _)) = [n]
topDeclImportedTypeNames (TSym (TySym n _ _)) = [n]
topDeclImportedTypeNames _ = []

collidingImportedTypeNames :: ModuleGraph -> [(Import, Mod.ModuleId)] -> Either String (Set Name)
collidingImportedTypeNames graph importPairs = do
  importedTypeNames <- concat <$> mapM namesFromImport importPairs
  let counts =
        Map.fromListWith (+) [(n, 1 :: Int) | n <- importedTypeNames]
  pure $
    Set.fromList
      [ n
        | (n, count) <- Map.toList counts,
          count > 1
      ]
  where
    namesFromImport (ImportModule _, modulePath) =
      topDeclTypeNamesForModule modulePath
    namesFromImport (ImportAlias _ _, modulePath) =
      topDeclTypeNamesForModule modulePath
    namesFromImport (ImportOnly _ _, _) =
      Right []

    topDeclTypeNamesForModule modulePath = do
      publicDecls <- publicTopDeclsForModule graph modulePath
      pure (concatMap topDeclImportedTypeNames publicDecls)

importOnlyFunctionDecls :: Name -> [Name] -> [TopDecl] -> [TopDecl]
importOnlyFunctionDecls qualifier selectedNames allDecls =
  concatMap qualifyImpl allFds ++ concatMap wrapSelected selectedFds
  where
    allFds = [fd | TFunDef fd <- allDecls]
    selectedFds =
      [ fd
        | fd <- allFds,
          sigName (funSignature fd) `elem` selectedNames
      ]
    renameMap = importedFunctionRenameMap qualifier allDecls
    qualifyImpl fd
      | sigName (funSignature fd) == Name "revert" =
          [TFunDef fd]
      | otherwise =
          [TFunDef (qualifyFunctionImpl renameMap qualifier fd)]
    wrapSelected fd
      | sigName (funSignature fd) == Name "revert" =
          []
      | otherwise =
          [TFunDef (importOnlyFunctionWrapper qualifier fd)]

importOnlyFunctionWrapper :: Name -> FunDef -> FunDef
importOnlyFunctionWrapper qualifier (FunDef sig _body) =
  FunDef
    sig
    (forwardingWrapperBody sig implName)
  where
    originalName = sigName sig
    implName = hiddenFunctionName qualifier originalName

functionDependencyClosure :: Map Name [Name] -> [Name] -> [Name]
functionDependencyClosure depMap seeds = reverse (go Set.empty seeds [])
  where
    go :: Set Name -> [Name] -> [Name] -> [Name]
    go _ [] acc = acc
    go seen (n : pending) acc
      | n `Set.member` seen = go seen pending acc
      | otherwise =
          let deps = Map.findWithDefault [] n depMap
           in go (Set.insert n seen) (deps ++ pending) (n : acc)

funDefFunctionRefs :: FunDef -> [Name]
funDefFunctionRefs (FunDef _ body) =
  bodyFunctionRefs body

bodyFunctionRefs :: Body -> [Name]
bodyFunctionRefs =
  concatMap stmtFunctionRefs

stmtFunctionRefs :: Stmt -> [Name]
stmtFunctionRefs (Assign lhs rhs) =
  expFunctionRefs lhs ++ expFunctionRefs rhs
stmtFunctionRefs (StmtPlusEq e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
stmtFunctionRefs (StmtMinusEq e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
stmtFunctionRefs (Let _ _ me) =
  maybe [] expFunctionRefs me
stmtFunctionRefs (StmtExp e) =
  expFunctionRefs e
stmtFunctionRefs (Return e) =
  expFunctionRefs e
stmtFunctionRefs (Match es eqns) =
  concatMap expFunctionRefs es ++ concatMap equationFunctionRefs eqns
stmtFunctionRefs (Asm _) =
  []
stmtFunctionRefs (If e blk1 blk2) =
  expFunctionRefs e ++ bodyFunctionRefs blk1 ++ bodyFunctionRefs blk2

equationFunctionRefs :: Equation -> [Name]
equationFunctionRefs (_pats, body) =
  bodyFunctionRefs body

expFunctionRefs :: Exp -> [Name]
expFunctionRefs (Lit _) = []
expFunctionRefs (ExpName me n es) =
  directRef ++ maybe [] expFunctionRefs me ++ concatMap expFunctionRefs es
  where
    directRef =
      case me of
        Nothing -> [n]
        _ -> maybe [] pure (qualifiedMemberName me n)
expFunctionRefs (ExpVar me n) =
  directRef ++ maybe [] expFunctionRefs me
  where
    directRef =
      case me of
        Nothing -> [n]
        _ -> maybe [] pure (qualifiedMemberName me n)
expFunctionRefs (ExpDotName _ es) =
  concatMap expFunctionRefs es
expFunctionRefs (Lam _ body _mt) =
  bodyFunctionRefs body
expFunctionRefs (TyExp e _ty) =
  expFunctionRefs e
expFunctionRefs (ExpIndexed e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
expFunctionRefs (ExpPlus e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
expFunctionRefs (ExpMinus e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
expFunctionRefs (ExpTimes e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
expFunctionRefs (ExpDivide e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
expFunctionRefs (ExpModulo e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
expFunctionRefs (ExpLT e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
expFunctionRefs (ExpGT e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
expFunctionRefs (ExpLE e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
expFunctionRefs (ExpGE e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
expFunctionRefs (ExpEE e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
expFunctionRefs (ExpNE e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
expFunctionRefs (ExpLAnd e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
expFunctionRefs (ExpLOr e1 e2) =
  expFunctionRefs e1 ++ expFunctionRefs e2
expFunctionRefs (ExpLNot e) =
  expFunctionRefs e
expFunctionRefs (ExpCond e1 e2 e3) =
  expFunctionRefs e1 ++ expFunctionRefs e2 ++ expFunctionRefs e3

renameTopDeclFunctionCalls :: Map Name Name -> TopDecl -> TopDecl
renameTopDeclFunctionCalls renameMap (TInstDef inst) =
  TInstDef
    ( inst
        { instFunctions =
            map (renameFunDefFunctionCalls renameMap) (instFunctions inst)
        }
    )
renameTopDeclFunctionCalls renameMap (TContr c) =
  TContr (renameContractFunctionCalls renameMap c)
renameTopDeclFunctionCalls _ d = d

renameFunDefFunctionCalls :: Map Name Name -> FunDef -> FunDef
renameFunDefFunctionCalls renameMap (FunDef sig body) =
  FunDef sig (renameBodyFunctionCalls renameMap body)

renameContractFunctionCalls :: Map Name Name -> Contract -> Contract
renameContractFunctionCalls renameMap (Contract n ts ds) =
  Contract n ts (map (renameContractDeclFunctionCalls renameMap) ds)

renameContractDeclFunctionCalls :: Map Name Name -> ContractDecl -> ContractDecl
renameContractDeclFunctionCalls renameMap (CFunDecl fd) =
  CFunDecl (renameFunDefFunctionCalls renameMap fd)
renameContractDeclFunctionCalls renameMap (CFieldDecl (Field n ty me)) =
  CFieldDecl (Field n ty (renameExpFunctionCalls renameMap <$> me))
renameContractDeclFunctionCalls renameMap (CConstrDecl (Constructor ps body)) =
  CConstrDecl (Constructor ps (renameBodyFunctionCalls renameMap body))
renameContractDeclFunctionCalls _ d = d

isFunctionTopDecl :: TopDecl -> Bool
isFunctionTopDecl (TFunDef _) = True
isFunctionTopDecl _ = False

shadowImportedDecls :: [TopDecl] -> [TopDecl] -> [TopDecl]
shadowImportedDecls localDecls =
  reverse . snd . foldl step (initialSeen, [])
  where
    initialSeen =
      ( concatMap topDeclTermNames localDecls,
        concatMap topDeclTypeNames localDecls,
        concatMap topDeclClassNames localDecls,
        [inst | TInstDef inst <- localDecls]
      )

    step (seen, acc) decl =
      case filterDecl seen decl of
        (seen', Just decl') -> (seen', decl' : acc)
        (seen', Nothing) -> (seen', acc)

    filterDecl (termNames, typeNames, classNames, instDecls) d@(TFunDef (FunDef sig _))
      | sigName sig `elem` termNames = ((termNames, typeNames, classNames, instDecls), Nothing)
      | otherwise =
          ( (sigName sig : termNames, typeNames, classNames, instDecls),
            Just d
          )
    filterDecl (termNames, typeNames, classNames, instDecls) d@(TSym (TySym n _ _))
      | n `elem` typeNames = ((termNames, typeNames, classNames, instDecls), Nothing)
      | otherwise =
          ( (termNames, n : typeNames, classNames, instDecls),
            Just d
          )
    filterDecl (termNames, typeNames, classNames, instDecls) d@(TClassDef (Class _ _ n _ _ _))
      | n `elem` classNames = ((termNames, typeNames, classNames, instDecls), Nothing)
      | otherwise =
          ( (termNames, typeNames, n : classNames, instDecls),
            Just d
          )
    filterDecl (termNames, typeNames, classNames, instDecls) d@(TContr (Contract n _ _))
      | n `elem` typeNames = ((termNames, typeNames, classNames, instDecls), Nothing)
      | otherwise =
          ( (termNames, n : typeNames, classNames, instDecls),
            Just d
          )
    filterDecl (termNames, typeNames, classNames, instDecls) (TDataDef (DataTy n ts cs))
      | n `elem` typeNames = ((termNames, typeNames, classNames, instDecls), Nothing)
      | otherwise =
          ( (termNames, n : typeNames, classNames, instDecls),
            Just (TDataDef (DataTy n ts cs))
          )
    filterDecl (termNames, typeNames, classNames, instDecls) d@(TInstDef inst)
      | inst `elem` instDecls = ((termNames, typeNames, classNames, instDecls), Nothing)
      | otherwise =
          ( (termNames, typeNames, classNames, inst : instDecls),
            Just d
          )
    filterDecl seen (TExportDecl _) = (seen, Nothing)
    filterDecl seen (TPragmaDecl _) = (seen, Nothing)

topDeclTermNames :: TopDecl -> [Name]
topDeclTermNames (TFunDef (FunDef sig _)) = [sigName sig]
topDeclTermNames (TDataDef (DataTy _ _ cs)) = map constrName cs
topDeclTermNames _ = []

topDeclTypeNames :: TopDecl -> [Name]
topDeclTypeNames (TSym (TySym n _ _)) = [n]
topDeclTypeNames (TContr (Contract n _ _)) = [n]
topDeclTypeNames (TDataDef (DataTy n _ _)) = [n]
topDeclTypeNames _ = []

topDeclClassNames :: TopDecl -> [Name]
topDeclClassNames (TClassDef (Class _ _ n _ _ _)) = [n]
topDeclClassNames _ = []

applyImportVisibility :: Import -> [TopDecl] -> [TopDecl]
applyImportVisibility (ImportOnly _ SelectAll) =
  id
applyImportVisibility (ImportOnly _ (SelectOnly names)) =
  mapMaybe (selectTopDecl names)
applyImportVisibility (ImportModule _) =
  const []
applyImportVisibility (ImportAlias _ _) =
  const []

selectTopDecl :: [Name] -> TopDecl -> Maybe TopDecl
selectTopDecl names d@(TFunDef (FunDef sig _))
  | sigName sig `elem` names = Just d
  | otherwise = Nothing
selectTopDecl names d@(TSym (TySym n _ _))
  | n `elem` names = Just d
  | otherwise = Nothing
selectTopDecl names d@(TClassDef (Class _ _ n _ _ _))
  | n `elem` names = Just d
  | otherwise = Nothing
selectTopDecl names d@(TContr (Contract n _ _))
  | n `elem` names = Just d
  | otherwise = Nothing
selectTopDecl names (TDataDef (DataTy n ts cs))
  | n `elem` names = Just (TDataDef (DataTy n ts cs))
  | otherwise =
      case filter (\c -> constrName c `elem` names) cs of
        [] -> Nothing
        cs' -> Just (TDataDef (DataTy n ts cs'))
selectTopDecl _ d@(TInstDef _) =
  Just d
selectTopDecl _ (TExportDecl _) =
  Nothing
selectTopDecl _ (TPragmaDecl _) =
  Nothing

ensureNoAmbiguousSelectedImports :: ModuleGraph -> [(Import, Mod.ModuleId)] -> Either String ()
ensureNoAmbiguousSelectedImports graph importPairs = do
  selectedPairs <- concat <$> mapM selectedFromImport importPairs
  case ambiguous selectedPairs of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Ambiguous selected imports:",
            unlines (map formatAmbiguous xs)
          ]
  where
    selectedFromImport (ImportOnly modName selector, modulePath) = do
      names <- resolveSelectedImportItems graph modName modulePath selector
      pure [(item, modName) | item <- uniqueNames names]
    selectedFromImport _ = pure []

    ambiguous selectedPairs =
      [ (item, uniqueModulePaths mods)
        | (item, mods) <- Map.toList selections,
          length (uniqueModulePaths mods) > 1
      ]
      where
        selections :: Map Name [ModulePath]
        selections = Map.fromListWith (++) [(item, [modName]) | (item, modName) <- selectedPairs]

formatAmbiguous :: (Name, [ModulePath]) -> String
formatAmbiguous (item, mods) =
  "  "
    ++ show item
    ++ " imported from "
    ++ intercalate ", " (map Mod.modulePathDisplay mods)

uniqueNames :: [Name] -> [Name]
uniqueNames = reverse . fst . foldl step ([], Map.empty)
  where
    step (acc, seen) n
      | Map.member n seen = (acc, seen)
      | otherwise = (n : acc, Map.insert n () seen)

uniqueModulePaths :: [ModulePath] -> [ModulePath]
uniqueModulePaths = reverse . fst . foldl step ([], Map.empty)
  where
    step (acc, seen) n
      | Map.member n seen = (acc, seen)
      | otherwise = (n : acc, Map.insert n () seen)

duplicateNames :: [Name] -> [Name]
duplicateNames names =
  [ n
    | (n, count) <- Map.toList counts,
      count > 1
  ]
  where
    counts = Map.fromListWith (+) [(n, 1 :: Int) | n <- names]

ensureNoDuplicateModuleQualifiers :: CompUnit -> Either String ()
ensureNoDuplicateModuleQualifiers (CompUnit imps _) =
  case duplicates of
    [] -> Right ()
    qs ->
      Left $
        unlines
          [ "Duplicate import qualifiers:",
            unlines (map (\q -> "  " ++ show q) qs)
          ]
  where
    duplicates = duplicateNames (mapMaybe moduleQualifier imps)

moduleQualifier :: Import -> Maybe Name
moduleQualifier (ImportModule n) = Just (defaultModuleBindingName n)
moduleQualifier (ImportAlias _ n) = Just n
moduleQualifier (ImportOnly _ _) = Nothing

ensureNoDuplicateSelectedItems :: CompUnit -> Either String ()
ensureNoDuplicateSelectedItems (CompUnit imps _) =
  case concatMap duplicateItems imps of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Duplicate names in selective import:",
            unlines xs
          ]
  where
    duplicateItems (ImportOnly moduleName (SelectOnly items)) =
      [ "  " ++ Mod.modulePathDisplay moduleName ++ "." ++ show item
        | item <- duplicateNames items
      ]
    duplicateItems (ImportOnly _ SelectAll) = []
    duplicateItems _ = []
