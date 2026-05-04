module Solcore.Frontend.Module.Loader
  ( ModuleGraph (..),
    LoadedModule (..),
    loadModuleGraph,
    flattenModuleValidationCompUnit,
    flattenModuleStrictCompileCompUnit,
    flattenModuleStrictCompileCompUnitWithImportedStart,
    flattenModuleStrictCompileCompUnitWithMetadata,
    flattenModuleStrictValidationCompUnit,
    loadCompUnit,
    moduleSourcePath,
  )
where

import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Data.Graph (SCC (..), stronglyConnComp)
import Data.List (find, intercalate, isPrefixOf, sortOn)
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Maybe (fromMaybe, isJust, mapMaybe)
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
    stdRoot :: Maybe FilePath,
    externalRoots :: Map Name FilePath
  }

data LoadState
  = LoadState
  { loadedModules :: Map Mod.ModuleId LoadedModule,
    moduleDeps :: Map Mod.ModuleId [Mod.ModuleId],
    moduleRefDeps :: Map Mod.ModuleId [Mod.ModuleId],
    loadingModules :: Set Mod.ModuleId,
    loadOrder :: [Mod.ModuleId]
  }

emptyLoadState :: LoadState
emptyLoadState = LoadState Map.empty Map.empty Map.empty Set.empty []

data ModuleGraph
  = ModuleGraph
  { entryModule :: Mod.ModuleId,
    modules :: Map Mod.ModuleId LoadedModule,
    dependencies :: Map Mod.ModuleId [Mod.ModuleId],
    referenceDependencies :: Map Mod.ModuleId [Mod.ModuleId],
    importGroups :: Map Mod.ModuleId [Mod.ModuleId],
    referenceGroups :: Map Mod.ModuleId [Mod.ModuleId],
    moduleOrder :: [Mod.ModuleId]
  }
  deriving (Eq, Show)

loadModuleGraph :: FilePath -> Maybe FilePath -> [(Name, FilePath)] -> FilePath -> IO (Either String ModuleGraph)
loadModuleGraph mainRootPath stdRootPath externalLibs entryFile = runExceptT do
  entryAbsolute <- liftIO $ makeAbsolute entryFile
  cfg <- liftIO $ mkLoaderConfig mainRootPath stdRootPath externalLibs entryFile
  entryId <- moduleIdForPath Mod.MainLibrary (mainRoot cfg) entryAbsolute
  st <- execStateT (visit cfg entryId entryAbsolute) emptyLoadState
  let loaded = loadedModules st
      importDeps = moduleDeps st
      refDeps = moduleRefDeps st
  pure
    ( ModuleGraph
        { entryModule = entryId,
          modules = loaded,
          dependencies = importDeps,
          referenceDependencies = refDeps,
          importGroups = buildGroupMap loaded importDeps,
          referenceGroups = buildGroupMap loaded refDeps,
          moduleOrder = reverse (loadOrder st)
        }
    )

-- Loads the entry file and all recursive imports, and keeps the old
-- flattened-declaration behavior for compatibility.
loadCompUnit :: [FilePath] -> FilePath -> IO (Either String CompUnit)
loadCompUnit roots entryFile = runExceptT do
  let defaultMainRoot = takeDirectory entryFile
      mainRootPath = case roots of
        [] -> defaultMainRoot
        x : _ -> x
      stdRootPath = case roots of
        _ : y : _ -> Just y
        _ -> Nothing
  graph <- ExceptT $ loadModuleGraph mainRootPath stdRootPath [] entryFile
  ExceptT $ pure (flattenModuleValidationCompUnit graph (entryModule graph))

mkLoaderConfig :: FilePath -> Maybe FilePath -> [(Name, FilePath)] -> FilePath -> IO LoaderConfig
mkLoaderConfig mainRootPath stdRootPath externalLibs _entryFile = do
  mainRoot' <- makeAbsolute mainRootPath
  stdRoot' <- traverse makeAbsolute stdRootPath
  externalRoots' <-
    Map.fromList
      <$> mapM
        ( \(libName, libRoot) -> do
            absRoot <- makeAbsolute libRoot
            pure (libName, absRoot)
        )
        externalLibs
  pure
    LoaderConfig
      { mainRoot = mainRoot',
        stdRoot = stdRoot',
        externalRoots = externalRoots'
      }

visit ::
  LoaderConfig ->
  Mod.ModuleId ->
  FilePath ->
  StateT LoadState (ExceptT String IO) ()
visit cfg moduleId sourcePath = do
  alreadyLoaded <- gets (Map.member moduleId . loadedModules)
  loading <- gets (Set.member moduleId . loadingModules)
  unless (alreadyLoaded || loading) do
    modify (\st -> st {loadingModules = Set.insert moduleId (loadingModules st)})
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
      (\(targetId, targetPath) -> visit cfg targetId targetPath)
      referencedModules
    modify
      ( \st ->
          st
            { loadedModules = Map.insert moduleId (LoadedModule sourcePath cunit moduleRefs) (loadedModules st),
              moduleDeps = Map.insert moduleId (map fst importedModules) (moduleDeps st),
              moduleRefDeps = Map.insert moduleId (map fst referencedModules) (moduleRefDeps st),
              loadingModules = Set.delete moduleId (loadingModules st),
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
          pure [resolveStdModule root relName]
      | otherwise ->
          (: []) <$> resolveWithinLibrary currentLibrary resolvedName
      where
        currentLibrary = Mod.moduleLibrary currentModule
        resolvedName = Mod.appendRelativeModulePath (Mod.moduleName currentModule) relName
    LibraryPath absName ->
      (: []) <$> resolveWithinLibrary (Mod.moduleLibrary currentModule) absName
    ExternalPath libName modName ->
      case Map.lookup libName (externalRoots cfg) of
        Just root ->
          pure [(Mod.ModuleId (Mod.ExternalLibrary libName) modName, toFilePath root modName)]
        Nothing ->
          Left ("external library root is not configured: @" ++ show libName)
  where
    resolveWithinLibrary libId modName = do
      root <- rootForLibrary cfg libId
      pure (Mod.ModuleId libId modName, toFilePath root modName)
    resolveStdModule root modName =
      let stdName = normalizeStdModuleName modName
       in (Mod.ModuleId Mod.StdLibrary stdName, toFilePath root stdName)

firstExisting :: [(Mod.ModuleId, FilePath)] -> IO (Maybe (Mod.ModuleId, FilePath))
firstExisting [] = pure Nothing
firstExisting (candidate@(_, path) : rest) = do
  exists <- doesFileExist path
  if exists then pure (Just candidate) else firstExisting rest

isStdSpecial :: Name -> Bool
isStdSpecial (Name "std") = True
isStdSpecial (QualName (Name "std") _) = True
isStdSpecial _ = False

normalizeStdModuleName :: Name -> Name
normalizeStdModuleName (Name "std") = Name "std"
normalizeStdModuleName (QualName (Name "std") suffix) = Name suffix
normalizeStdModuleName (QualName prefix suffix) =
  QualName (normalizeStdModuleName prefix) suffix
normalizeStdModuleName moduleName = moduleName

rootForLibrary :: LoaderConfig -> Mod.LibraryId -> Either String FilePath
rootForLibrary cfg Mod.MainLibrary = Right (mainRoot cfg)
rootForLibrary cfg Mod.StdLibrary =
  Right (maybe (mainRoot cfg </> "std") id (stdRoot cfg))
rootForLibrary cfg (Mod.ExternalLibrary libName) =
  case Map.lookup libName (externalRoots cfg) of
    Just root -> Right root
    Nothing ->
      Left ("external library root is not configured: @" ++ show libName)

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

buildGroupMap :: Map Mod.ModuleId LoadedModule -> Map Mod.ModuleId [Mod.ModuleId] -> Map Mod.ModuleId [Mod.ModuleId]
buildGroupMap loaded depMap =
  Map.fromList
    [ (moduleId, group)
      | group <- groups,
        moduleId <- group
    ]
  where
    groups =
      map flattenScc $
        stronglyConnComp
          [ (moduleId, moduleId, Map.findWithDefault [] moduleId depMap)
            | moduleId <- Map.keys loaded
          ]

    flattenScc (AcyclicSCC moduleId) = [moduleId]
    flattenScc (CyclicSCC moduleIds) = moduleIds

moduleSourcePath :: ModuleGraph -> Mod.ModuleId -> Either String FilePath
moduleSourcePath graph modulePath =
  maybe
    (Left ("Internal error: module not loaded: " ++ Mod.moduleIdDisplay modulePath))
    (Right . loadedSourcePath)
    (Map.lookup modulePath (modules graph))

moduleImportPairsFor :: ModuleGraph -> Mod.ModuleId -> CompUnit -> [(Import, Mod.ModuleId)]
moduleImportPairsFor graph modulePath unit =
  zip (imports unit) (Map.findWithDefault [] modulePath (dependencies graph))

importGroupFor :: ModuleGraph -> Mod.ModuleId -> [Mod.ModuleId]
importGroupFor graph modulePath =
  Map.findWithDefault [modulePath] modulePath (importGroups graph)

referenceGroupFor :: ModuleGraph -> Mod.ModuleId -> [Mod.ModuleId]
referenceGroupFor graph modulePath =
  Map.findWithDefault [modulePath] modulePath (referenceGroups graph)

isRecursiveImportGroup :: ModuleGraph -> Mod.ModuleId -> Bool
isRecursiveImportGroup graph modulePath =
  case importGroupFor graph modulePath of
    [] -> False
    [_] ->
      modulePath `elem` Map.findWithDefault [] modulePath (dependencies graph)
    _ -> True

data ExportedItemRef
  = ExportedItemRef
  { exportedItemOrigin :: Mod.ModuleId,
    exportedItemName :: Name,
    exportedItemConstructors :: Maybe [Name]
  }
  deriving (Eq, Ord, Show)

data ExportedModuleBinding
  = ExportedModuleBinding
  { exportedModuleName :: Name,
    exportedModuleTarget :: Mod.ModuleId
  }
  deriving (Eq, Ord, Show)

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

normalizePublicInterface :: ModulePublicInterface -> ModulePublicInterface
normalizePublicInterface publicInterface =
  ModulePublicInterface
    { publicItemRefs = normalizeItemRefs (publicItemRefs publicInterface),
      publicModuleBindings = normalizeModuleBindings (publicModuleBindings publicInterface)
    }
  where
    normalizeItemRefs refs =
      concatMap (\itemName -> Map.findWithDefault [] itemName chosenRefs) (sortOn show orderedNames)
      where
        (orderedNames, chosenRefs) = foldl step ([], Map.empty) refs

        step (names, chosen) ref =
          let itemName = exportedItemName ref
           in case Map.lookup itemName chosen of
                Nothing ->
                  (names ++ [itemName], Map.insert itemName [ref] chosen)
                Just existingRefs ->
                  (names, Map.insert itemName (mergeWith existingRefs ref) chosen)

        mergeWith existingRefs ref =
          case break ((== refOriginKey ref) . refOriginKey) existingRefs of
            (before, matched : after) ->
              before ++ [mergeRefs matched ref] ++ after
            _ ->
              existingRefs ++ [ref]

        refOriginKey existingRef = (exportedItemOrigin existingRef, isJust (exportedItemConstructors existingRef))

        mergeRefs existingRef newRef =
          existingRef {exportedItemConstructors = mergeConstructors (exportedItemConstructors existingRef) (exportedItemConstructors newRef)}

        mergeConstructors Nothing _ = Nothing
        mergeConstructors _ Nothing = Nothing
        mergeConstructors (Just xs) (Just ys) = Just (uniqueNames (xs ++ ys))

    normalizeModuleBindings bindings =
      [ chosen Map.! bindingName
        | bindingName <- sortOn show orderedNames
      ]
      where
        (orderedNames, chosen) = foldl step ([], Map.empty) bindings

        step (names, current) binding =
          let bindingName = exportedModuleName binding
           in case Map.lookup bindingName current of
                Nothing -> (names ++ [bindingName], Map.insert bindingName binding current)
                Just _ -> (names, current)

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
  ensureNoModuleLookupConflicts graph unit importPairs
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
  (\(cunit, _, _, _) -> cunit) <$> flattenModuleStrictCompileCompUnitWithMetadata graph modulePath

flattenModuleStrictCompileCompUnitWithImportedStart ::
  ModuleGraph ->
  Mod.ModuleId ->
  Either String (CompUnit, Int)
flattenModuleStrictCompileCompUnitWithImportedStart graph modulePath
  | otherwise = do
      (cunit, _, importedStart, _) <- flattenModuleStrictCompileCompUnitWithMetadata graph modulePath
      pure (cunit, importedStart)

flattenModuleStrictCompileCompUnitWithMetadata ::
  ModuleGraph ->
  Mod.ModuleId ->
  Either String (CompUnit, Int, Int, [(Name, [Name])])
flattenModuleStrictCompileCompUnitWithMetadata graph modulePath
  | isRecursiveImportGroup graph modulePath = do
      compileSurfaces <- compileSurfacesForGroup graph (importGroupFor graph modulePath)
      flattenModuleStrictCompileCompUnitWithSurfaces compileSurfaces graph modulePath
flattenModuleStrictCompileCompUnitWithMetadata graph modulePath = do
  (unit, _sourcePath, importPairs) <- prepareFlattenContext graph modulePath
  collidingTypeNames <- collidingImportedTypeNames graph importPairs
  importedDecls <- dedupeImportedInstanceDecls . concat <$> mapM (strictCompileImportedDecls collidingTypeNames graph) importPairs
  partialImportedTypes <- concat <$> mapM (strictCompileImportedPartialTypes collidingTypeNames graph) importPairs
  qualifiedDecls <- concat <$> mapM (qualifiedImportDecls collidingTypeNames graph) importPairs
  let localDecls = topDeclsFrom unit
      visibleImportedDecls = uniqueTopDecls (filterImportedInstanceConflicts localDecls importedDecls)
      localStart = length qualifiedDecls
      importedStart = length qualifiedDecls + length localDecls
  pure
    ( CompUnit (imports unit) (qualifiedDecls ++ localDecls ++ visibleImportedDecls),
      localStart,
      importedStart,
      normalizePartialImportedTypes partialImportedTypes
    )

flattenModuleStrictCompileCompUnitWithSurfaces ::
  Map Mod.ModuleId [TopDecl] ->
  ModuleGraph ->
  Mod.ModuleId ->
  Either String (CompUnit, Int, Int, [(Name, [Name])])
flattenModuleStrictCompileCompUnitWithSurfaces compileSurfaces graph modulePath = do
  (unit, _sourcePath, importPairs) <- prepareFlattenContext graph modulePath
  collidingTypeNames <- collidingImportedTypeNames graph importPairs
  importedDecls <-
    dedupeImportedInstanceDecls
      . concat
      <$> mapM (strictCompileImportedDeclsWithSurfaces compileSurfaces collidingTypeNames graph) importPairs
  partialImportedTypes <-
    concat
      <$> mapM
        (strictCompileImportedPartialTypesWithSurfaces compileSurfaces collidingTypeNames graph)
        importPairs
  qualifiedDecls <-
    concat <$> mapM (qualifiedImportDeclsWithSurfaces compileSurfaces collidingTypeNames graph) importPairs
  let localDecls = topDeclsFrom unit
      visibleImportedDecls = uniqueTopDecls (filterImportedInstanceConflicts localDecls importedDecls)
      localStart = length qualifiedDecls
      importedStart = length qualifiedDecls + length localDecls
  pure
    ( CompUnit (imports unit) (qualifiedDecls ++ localDecls ++ visibleImportedDecls),
      localStart,
      importedStart,
      normalizePartialImportedTypes partialImportedTypes
    )

flattenModuleStrictValidationCompUnit :: ModuleGraph -> Mod.ModuleId -> Either String CompUnit
flattenModuleStrictValidationCompUnit graph modulePath = do
  (unit, _sourcePath, importPairs) <- prepareFlattenContext graph modulePath
  importedDecls <- concat <$> mapM (strictValidationImportedDecls graph) importPairs
  qualifiedDecls <- concat <$> mapM (qualifiedImportStubDecls graph) importPairs
  let localDecls = topDeclsFrom unit
      visibleImportedDecls = uniqueTopDecls (filterImportedInstanceConflicts localDecls importedDecls)
  pure (CompUnit (imports unit) (qualifiedDecls ++ localDecls ++ visibleImportedDecls))

ensureImportItemsExist :: ModuleGraph -> [(Import, Mod.ModuleId)] -> Either String ()
ensureImportItemsExist graph importPairs = do
  (unknownSelectedGroups, unknownHiddenGroups) <- unzip <$> mapM unknowns importPairs
  case (concat unknownSelectedGroups, concat unknownHiddenGroups) of
    ([], []) -> Right ()
    (selectedXs, hiddenXs) ->
      Left $
        unlines
          ( (if null selectedXs then [] else ["Unknown selected imports:", unlines selectedXs])
              ++ (if null hiddenXs then [] else ["Unknown hidden imports:", unlines hiddenXs])
          )
  where
    unknowns (ImportOnly importPath items, modulePath) = do
      available <- importableNamesForModule graph modulePath
      let missingSelected = filter (`notElem` available) (explicitSelectorNames items)
          missingHidden = filter (`notElem` available) (explicitHiddenNames items)
      pure
        ( [formatMissing importPath n | n <- missingSelected],
          [formatMissing importPath n | n <- missingHidden]
        )
    unknowns _ = pure ([], [])

formatMissing :: ModulePath -> Name -> String
formatMissing importPath itemName =
  "  " ++ Mod.modulePathDisplay importPath ++ "." ++ show itemName

resolveSelectedImportItems :: ModuleGraph -> ModulePath -> Mod.ModuleId -> ItemSelector -> Either String [Name]
resolveSelectedImportItems graph _moduleName modulePath selector = do
  available <- importableNamesForModule graph modulePath
  pure (selectedNamesFromAvailable available selector)

selectedNamesFromAvailable :: [Name] -> ItemSelector -> [Name]
selectedNamesFromAvailable available (SelectItems items hidden) =
  filter (`notElem` hiddenNames) (uniqueNames (concatMap expand items))
  where
    hiddenNames = uniqueNames hidden
    expand SelectAllItems = available
    expand (SelectItem itemName) = [itemName]

importableNamesForModule :: ModuleGraph -> Mod.ModuleId -> Either String [Name]
importableNamesForModule graph modulePath = do
  publicDecls <- publicItemDeclsForModule graph modulePath
  pure (uniqueNames (concatMap topDeclNames publicDecls))

publicItemDeclsForModule :: ModuleGraph -> Mod.ModuleId -> Either String [TopDecl]
publicItemDeclsForModule graph modulePath =
  publicItemDeclsForModuleSeen graph Set.empty modulePath

publicItemDeclsForModuleSeen :: ModuleGraph -> Set ExportedItemRef -> Mod.ModuleId -> Either String [TopDecl]
publicItemDeclsForModuleSeen graph seen modulePath = do
  publicInterface <- publicModuleInterface graph modulePath
  unit <- lookupLoadedModule graph modulePath
  let localDecls =
        selectPublicItemDecls
          [ itemRef
            | itemRef <- publicItemRefs publicInterface,
              exportedItemOrigin itemRef == modulePath
          ]
          (topDeclsFrom unit)
  remoteDecls <- concat <$> mapM materializeRemoteRef (publicItemRefs publicInterface)
  pure (localDecls ++ shadowImportedDecls localDecls remoteDecls)
  where
    materializeRemoteRef itemRef
      | exportedItemOrigin itemRef == modulePath =
          pure []
      | itemRef `Set.member` seen =
          pure []
      | otherwise = do
          remoteDecls <-
            publicItemDeclsForModuleSeen
              graph
              (Set.insert itemRef seen)
              (exportedItemOrigin itemRef)
          pure (selectPublicItemDecls [itemRef] remoteDecls)

publicTopDeclsForModule :: ModuleGraph -> Mod.ModuleId -> Either String [TopDecl]
publicTopDeclsForModule graph modulePath = do
  publicDecls <- publicItemDeclsForModule graph modulePath
  unit <- lookupLoadedModule graph modulePath
  pure (publicDecls ++ [decl | decl@(TInstDef _) <- topDeclsFrom unit])

publicModuleInterface :: ModuleGraph -> Mod.ModuleId -> Either String ModulePublicInterface
publicModuleInterface graph modulePath = do
  interfaces <- publicInterfacesForGroup graph (referenceGroupFor graph modulePath)
  maybe
    (Left ("Internal error: missing public interface for " ++ Mod.moduleIdDisplay modulePath))
    Right
    (Map.lookup modulePath interfaces)

publicInterfacesForGroup :: ModuleGraph -> [Mod.ModuleId] -> Either String (Map Mod.ModuleId ModulePublicInterface)
publicInterfacesForGroup graph groupModules =
  go (0 :: Int) initialInterfaces
  where
    initialInterfaces =
      Map.fromList [(moduleId, emptyPublicInterface) | moduleId <- groupModules]

    maxIterations =
      max 8 (length groupModules * 8)

    go iterations currentInterfaces
      | iterations > maxIterations =
          Left $
            "Module interface fixed point did not stabilize for recursive group:\n  "
              ++ intercalate ", " (map Mod.moduleIdDisplay groupModules)
      | otherwise = do
          nextInterfaces <-
            Map.fromList <$> mapM (stepInterface currentInterfaces) groupModules
          if nextInterfaces == currentInterfaces
            then do
              validatePublicInterfaces graph groupModules nextInterfaces
              pure nextInterfaces
            else go (iterations + 1) nextInterfaces

    stepInterface currentInterfaces moduleId = do
      unit <- lookupLoadedModule graph moduleId
      sourcePath <- moduleSourcePath graph moduleId
      expandedDecls <-
        mapM
          (expandExportDeclFixed graph groupModules currentInterfaces moduleId sourcePath unit)
          [exportDecl | TExportDecl exportDecl <- topDeclsFrom unit]
      pure
        ( moduleId,
          normalizePublicInterface $
            ModulePublicInterface
              { publicItemRefs = concatMap publicItemRefs expandedDecls,
                publicModuleBindings = concatMap publicModuleBindings expandedDecls
              }
        )

publicModuleBindingsForModule :: ModuleGraph -> Mod.ModuleId -> Either String [ExportedModuleBinding]
publicModuleBindingsForModule graph modulePath =
  publicModuleBindings <$> publicModuleInterface graph modulePath

validatePublicInterfaces ::
  ModuleGraph ->
  [Mod.ModuleId] ->
  Map Mod.ModuleId ModulePublicInterface ->
  Either String ()
validatePublicInterfaces graph groupModules interfaces =
  mapM_ validateModule groupModules
  where
    validateModule moduleId = do
      unit <- lookupLoadedModule graph moduleId
      sourcePath <- moduleSourcePath graph moduleId
      mapM_
        (validateExportDecl sourcePath moduleId)
        [exportDecl | TExportDecl exportDecl <- topDeclsFrom unit]
      expandedDecls <-
        mapM
          (expandExportDeclFixed graph groupModules interfaces moduleId sourcePath unit)
          [exportDecl | TExportDecl exportDecl <- topDeclsFrom unit]
      let rawPublicInterface =
            ModulePublicInterface
              { publicItemRefs = concatMap publicItemRefs expandedDecls,
                publicModuleBindings = concatMap publicModuleBindings expandedDecls
              }
      ensureNoDuplicateExportedItems sourcePath (publicItemRefs rawPublicInterface)
      ensureNoDuplicateExportedModules sourcePath (publicModuleBindings rawPublicInterface)

    validateExportDecl sourcePath moduleId exportDecl =
      case exportDecl of
        ExportList specs ->
          mapM_ (validateExportSpec sourcePath moduleId) specs
        ExportModule _ ->
          pure ()
        ExportModuleAs _ _ ->
          pure ()
        ExportItemsFrom path selector -> do
          targetModule <- lookupModuleReference graph moduleId path
          let names = explicitExportSelectorNames selector
          availableNames <- interfaceNamesForModule targetModule
          when (hasExportSelectAll selector) (ensureRemoteModuleVisible moduleId path)
          ensureRemoteExportsExist sourcePath path names availableNames

    validateExportSpec sourcePath moduleId spec =
      case spec of
        ExportName itemName -> do
          unit <- lookupLoadedModule graph moduleId
          ensureLocalExportExists sourcePath (topDeclsFrom unit) itemName
        ExportNameWithConstructors typeName constructorSelector -> do
          unit <- lookupLoadedModule graph moduleId
          ensureLocalConstructorExportExists sourcePath (topDeclsFrom unit) typeName constructorSelector
        ExportAll ->
          pure ()
        ExportModuleAll path ->
          ensureRemoteModuleVisible moduleId path

    ensureRemoteModuleVisible moduleId path = do
      _ <- lookupModuleReference graph moduleId path
      pure ()

    interfaceNamesForModule targetModule
      | targetModule `elem` groupModules =
          pure $
            maybe [] (uniqueNames . map exportedItemName . publicItemRefs) (Map.lookup targetModule interfaces)
      | otherwise =
          importableNamesForModule graph targetModule

expandExportDeclFixed ::
  ModuleGraph ->
  [Mod.ModuleId] ->
  Map Mod.ModuleId ModulePublicInterface ->
  Mod.ModuleId ->
  FilePath ->
  CompUnit ->
  Export ->
  Either String ModulePublicInterface
expandExportDeclFixed graph groupModules currentInterfaces currentModule sourcePath unit (ExportList specs) = do
  expandedSpecs <- mapM (expandExportSpecFixed graph groupModules currentInterfaces currentModule sourcePath unit) specs
  pure
    ModulePublicInterface
      { publicItemRefs = concatMap publicItemRefs expandedSpecs,
        publicModuleBindings = concatMap publicModuleBindings expandedSpecs
      }
expandExportDeclFixed graph _groupModules _currentInterfaces currentModule _sourcePath _unit (ExportModule path) = do
  targetModule <- lookupModuleReference graph currentModule path
  pure
    emptyPublicInterface
      { publicModuleBindings =
          [ExportedModuleBinding (defaultModuleBindingName path) targetModule]
      }
expandExportDeclFixed graph _groupModules _currentInterfaces currentModule _sourcePath _unit (ExportModuleAs path aliasName) = do
  targetModule <- lookupModuleReference graph currentModule path
  pure
    emptyPublicInterface
      { publicModuleBindings = [ExportedModuleBinding aliasName targetModule]
      }
expandExportDeclFixed graph groupModules currentInterfaces currentModule sourcePath _unit (ExportItemsFrom path selector) = do
  itemRefs <- resolveRemoteExportItemsFixed graph groupModules currentInterfaces currentModule sourcePath path selector
  pure emptyPublicInterface {publicItemRefs = itemRefs}

expandExportSpecFixed ::
  ModuleGraph ->
  [Mod.ModuleId] ->
  Map Mod.ModuleId ModulePublicInterface ->
  Mod.ModuleId ->
  FilePath ->
  CompUnit ->
  ExportSpec ->
  Either String ModulePublicInterface
expandExportSpecFixed _graph _groupModules _currentInterfaces currentModule sourcePath unit (ExportName itemName) = do
  ensureLocalExportExists sourcePath (topDeclsFrom unit) itemName
  pure emptyPublicInterface {publicItemRefs = localExportRefsForName currentModule itemName (topDeclsFrom unit)}
expandExportSpecFixed _graph _groupModules _currentInterfaces currentModule sourcePath unit (ExportNameWithConstructors typeName constructorSelector) = do
  ensureLocalConstructorExportExists sourcePath (topDeclsFrom unit) typeName constructorSelector
  pure emptyPublicInterface {publicItemRefs = [localDataExportRef currentModule typeName (resolveLocalConstructorSelection typeName constructorSelector (topDeclsFrom unit))]}
expandExportSpecFixed _graph _groupModules _currentInterfaces currentModule _sourcePath unit ExportAll =
  pure
    emptyPublicInterface
      { publicItemRefs = availableExportRefs currentModule (topDeclsFrom unit)
      }
expandExportSpecFixed graph groupModules currentInterfaces currentModule sourcePath _unit (ExportModuleAll path) = do
  itemRefs <-
    resolveRemoteExportItemsFixed
      graph
      groupModules
      currentInterfaces
      currentModule
      sourcePath
      path
      (SelectExportItems [SelectExportAllItems])
  pure emptyPublicInterface {publicItemRefs = itemRefs}

resolveRemoteExportItemsFixed ::
  ModuleGraph ->
  [Mod.ModuleId] ->
  Map Mod.ModuleId ModulePublicInterface ->
  Mod.ModuleId ->
  FilePath ->
  ModulePath ->
  ExportSelector ->
  Either String [ExportedItemRef]
resolveRemoteExportItemsFixed graph groupModules currentInterfaces currentModule sourcePath exportPath selector = do
  targetModule <- lookupModuleReference graph currentModule exportPath
  if targetModule `elem` groupModules
    then resolveWithinGroup targetModule
    else resolveOutsideGroup targetModule
  where
    resolveWithinGroup targetModule =
      do
        let availableRefs = currentInterfaceRefs targetModule
        selectRemoteExportRefs sourcePath exportPath selector availableRefs False

    resolveOutsideGroup targetModule =
      do
        availableRefs <- publicItemRefs <$> publicModuleInterface graph targetModule
        selectRemoteExportRefs sourcePath exportPath selector availableRefs True

    currentInterfaceRefs targetModule =
      maybe [] publicItemRefs (Map.lookup targetModule currentInterfaces)

selectExportedItemRefs :: [Name] -> [ExportedItemRef] -> [ExportedItemRef]
selectExportedItemRefs names refs =
  concatMap pick names
  where
    pick itemName =
      [ ref
        | ref <- refs,
          exportedItemName ref == itemName
      ]

selectRemoteExportRefs ::
  FilePath ->
  ModulePath ->
  ExportSelector ->
  [ExportedItemRef] ->
  Bool ->
  Either String [ExportedItemRef]
selectRemoteExportRefs sourcePath exportPath (SelectExportItems items) availableRefs shouldValidate =
  concat <$> mapM selectEntry items
  where
    selectEntry SelectExportAllItems =
      pure availableRefs
    selectEntry (SelectExportItem itemName) = do
      let matchingRefs = selectExportedItemRefs [itemName] availableRefs
      when shouldValidate $
        ensureRemoteExportsExist sourcePath exportPath [itemName] (uniqueNames (map exportedItemName availableRefs))
      pure (map stripConstructorVisibility matchingRefs)
    selectEntry (SelectExportConstructors typeName constructorSelector) =
      case selectVisibleConstructors availableRefs typeName constructorSelector of
        Nothing
          | shouldValidate ->
              Left $
                unlines
                  [ "Unknown re-exported constructors:",
                    "  " ++ sourcePath,
                    "  " ++ Mod.modulePathDisplay exportPath ++ "." ++ show typeName
                  ]
          | otherwise ->
              pure []
        Just ref
          | shouldValidate,
            missingVisibleConstructors constructorSelector ref /= [] ->
              Left $
                unlines
                  [ "Unknown re-exported constructors:",
                    "  " ++ sourcePath,
                    unlines
                      [ "  " ++ Mod.modulePathDisplay exportPath ++ "." ++ show typeName ++ "." ++ show constructorName
                        | constructorName <- missingVisibleConstructors constructorSelector ref
                      ]
                  ]
          | otherwise ->
              pure [ref]

stripConstructorVisibility :: ExportedItemRef -> ExportedItemRef
stripConstructorVisibility itemRef =
  case exportedItemConstructors itemRef of
    Just _ ->
      itemRef {exportedItemConstructors = Just []}
    Nothing ->
      itemRef

selectVisibleConstructors :: [ExportedItemRef] -> Name -> ConstructorSelector -> Maybe ExportedItemRef
selectVisibleConstructors availableRefs typeName constructorSelector = do
  dataRef <- findVisibleDataRef availableRefs typeName
  let visibleConstructorNames = fromMaybe [] (exportedItemConstructors dataRef)
      selectedConstructors = selectConstructorSubset constructorSelector visibleConstructorNames
  pure (dataRef {exportedItemConstructors = Just selectedConstructors})

findVisibleDataRef :: [ExportedItemRef] -> Name -> Maybe ExportedItemRef
findVisibleDataRef availableRefs typeName =
  find (\itemRef -> exportedItemName itemRef == typeName && isJust (exportedItemConstructors itemRef)) availableRefs

selectConstructorSubset :: ConstructorSelector -> [Name] -> [Name]
selectConstructorSubset SelectAllConstructors visibleConstructorNames =
  visibleConstructorNames
selectConstructorSubset (SelectConstructors constructorNames) visibleConstructorNames =
  [ constructorName
    | constructorName <- uniqueNames constructorNames,
      constructorName `elem` visibleConstructorNames
  ]

missingVisibleConstructors :: ConstructorSelector -> ExportedItemRef -> [Name]
missingVisibleConstructors SelectAllConstructors _ =
  []
missingVisibleConstructors (SelectConstructors constructorNames) itemRef =
  [ constructorName
    | constructorName <- uniqueNames constructorNames,
      constructorName `notElem` fromMaybe [] (exportedItemConstructors itemRef)
  ]

availableExportNames :: [TopDecl] -> [Name]
availableExportNames ds =
  uniqueNames (concatMap topDeclNames (filter isImportableTopDecl ds))

availableExportRefs :: Mod.ModuleId -> [TopDecl] -> [ExportedItemRef]
availableExportRefs currentModule =
  concatMap (localExportRefsForDecl currentModule) . filter isImportableTopDecl

localExportRefsForName :: Mod.ModuleId -> Name -> [TopDecl] -> [ExportedItemRef]
localExportRefsForName currentModule itemName topLevelDecls =
  concatMap (localExportRefsForMatchingName currentModule itemName) (filter isImportableTopDecl topLevelDecls)

localExportRefsForMatchingName :: Mod.ModuleId -> Name -> TopDecl -> [ExportedItemRef]
localExportRefsForMatchingName currentModule itemName (TDataDef (DataTy n _ _))
  | itemName == n =
      [localDataExportRef currentModule n []]
  | otherwise =
      []
localExportRefsForMatchingName currentModule itemName decl
  | itemName `elem` topDeclNames decl =
      localExportRefsForDecl currentModule decl
  | otherwise =
      []

localExportRefsForDecl :: Mod.ModuleId -> TopDecl -> [ExportedItemRef]
localExportRefsForDecl currentModule decl =
  case decl of
    TDataDef (DataTy n _ _) ->
      [localDataExportRef currentModule n []]
    _ ->
      [ ExportedItemRef currentModule itemName Nothing
        | itemName <- topDeclNames decl
      ]

localDataExportRef :: Mod.ModuleId -> Name -> [Name] -> ExportedItemRef
localDataExportRef currentModule typeName visibleConstructors =
  ExportedItemRef currentModule typeName (Just (uniqueNames visibleConstructors))

ensureNoDuplicateExportedItems :: FilePath -> [ExportedItemRef] -> Either String ()
ensureNoDuplicateExportedItems modulePath itemRefs =
  case conflicts of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Duplicate exported item names:",
            "  " ++ modulePath,
            unlines (map (\n -> "  " ++ show n) xs)
          ]
  where
    conflicts =
      [ itemName
        | (itemName, refs) <- Map.toList groupedRefs,
          Set.size (Set.fromList [exportedItemOrigin ref | ref <- refs]) > 1
      ]
    groupedRefs = Map.fromListWith (++) [(exportedItemName ref, [ref]) | ref <- itemRefs]

ensureNoDuplicateExportedModules :: FilePath -> [ExportedModuleBinding] -> Either String ()
ensureNoDuplicateExportedModules modulePath moduleBindings =
  case conflicts of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Duplicate exported module names:",
            "  " ++ modulePath,
            unlines (map (\n -> "  " ++ show n) xs)
          ]
  where
    conflicts =
      [ bindingName
        | (bindingName, bindings) <- Map.toList groupedBindings,
          Set.size (Set.fromList [exportedModuleTarget binding | binding <- bindings]) > 1
      ]
    groupedBindings = Map.fromListWith (++) [(exportedModuleName binding, [binding]) | binding <- moduleBindings]

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

ensureLocalConstructorExportExists :: FilePath -> [TopDecl] -> Name -> ConstructorSelector -> Either String ()
ensureLocalConstructorExportExists sourcePath topLevelDecls typeName constructorSelector =
  case findLocalDataType typeName topLevelDecls of
    Nothing ->
      Left $
        unlines
          [ "Unknown export:",
            "  " ++ sourcePath,
            "  " ++ show typeName
          ]
    Just (DataTy _ _ constrs) ->
      ensureConstructorSelectorExists sourcePath typeName constructorSelector constrs

findLocalDataType :: Name -> [TopDecl] -> Maybe DataTy
findLocalDataType typeName =
  foldr
    ( \decl acc ->
        case decl of
          TDataDef dataTy | dataName dataTy == typeName -> Just dataTy
          _ -> acc
    )
    Nothing

ensureConstructorSelectorExists :: FilePath -> Name -> ConstructorSelector -> [Constr] -> Either String ()
ensureConstructorSelectorExists _sourcePath _typeName SelectAllConstructors _ =
  Right ()
ensureConstructorSelectorExists sourcePath typeName (SelectConstructors constructorNames) constrs =
  case missing of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Unknown exported constructors:",
            "  " ++ sourcePath,
            unlines ["  " ++ show typeName ++ "." ++ show constructorName | constructorName <- xs]
          ]
  where
    availableNames = uniqueNames (map (constructorLeafName . constrName) constrs)
    missing = filter (`notElem` availableNames) constructorNames

resolveLocalConstructorSelection :: Name -> ConstructorSelector -> [TopDecl] -> [Name]
resolveLocalConstructorSelection typeName constructorSelector topLevelDecls =
  case findLocalDataType typeName topLevelDecls of
    Just (DataTy _ _ constrs) -> resolveConstructorSelection constructorSelector constrs
    Nothing -> []

resolveConstructorSelection :: ConstructorSelector -> [Constr] -> [Name]
resolveConstructorSelection SelectAllConstructors constrs =
  uniqueNames (map (constructorLeafName . constrName) constrs)
resolveConstructorSelection (SelectConstructors constructorNames) _ =
  uniqueNames constructorNames

ensureRemoteExportsExist :: FilePath -> ModulePath -> [Name] -> [Name] -> Either String ()
ensureRemoteExportsExist sourcePath exportPath names availableNames =
  case missing of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Unknown re-exported names:",
            "  " ++ sourcePath,
            unlines [formatMissing exportPath missingName | missingName <- xs]
          ]
  where
    missing = filter (`notElem` availableNames) names

groupRemoteItemRefs :: Mod.ModuleId -> [ExportedItemRef] -> [(Mod.ModuleId, [Name])]
groupRemoteItemRefs currentModule =
  reverse . fst . foldl step ([], Map.empty)
  where
    step (acc, seen) itemRef
      | exportedItemOrigin itemRef == currentModule = (acc, seen)
      | otherwise =
          case Map.lookup (exportedItemOrigin itemRef) seen of
            Just names ->
              ( replaceAssoc (exportedItemOrigin itemRef) (names ++ [exportedItemName itemRef]) acc,
                Map.insert (exportedItemOrigin itemRef) (names ++ [exportedItemName itemRef]) seen
              )
            Nothing ->
              ( (exportedItemOrigin itemRef, [exportedItemName itemRef]) : acc,
                Map.insert (exportedItemOrigin itemRef) [exportedItemName itemRef] seen
              )

    replaceAssoc moduleId names =
      map (\(loadedModule, currentNames) -> if loadedModule == moduleId then (loadedModule, names) else (loadedModule, currentNames))

defaultModuleBindingName :: ModulePath -> Name
defaultModuleBindingName =
  moduleLeafName . Mod.modulePathName

moduleLeafName :: Name -> Name
moduleLeafName (Name n) = Name n
moduleLeafName (QualName _ n) = Name n

importModuleQualifiers :: ModulePath -> [Name]
importModuleQualifiers importPath =
  uniqueNames [defaultModuleBindingName importPath, Mod.modulePathName importPath]

selectPublicItemDecls :: [ExportedItemRef] -> [TopDecl] -> [TopDecl]
selectPublicItemDecls itemRefs topLevelDecls =
  uniqueTopDecls $
    concatMap
      (\itemRef -> mapMaybe (selectTopDeclForExportRef itemRef) filteredDecls)
      itemRefs
  where
    filteredDecls = filter isPublicItemTopDecl topLevelDecls

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
topDeclNames (TDataDef (DataTy n _ _)) = [n]
topDeclNames (TInstDef _) = []
topDeclNames (TExportDecl _) = []
topDeclNames (TPragmaDecl _) = []

qualifiedImportDecls :: Set Name -> ModuleGraph -> (Import, Mod.ModuleId) -> Either String [TopDecl]
qualifiedImportDecls =
  qualifiedImportDeclsWithSurfaces Map.empty

qualifiedImportDeclsWithSurfaces ::
  Map Mod.ModuleId [TopDecl] ->
  Set Name ->
  ModuleGraph ->
  (Import, Mod.ModuleId) ->
  Either String [TopDecl]
qualifiedImportDeclsWithSurfaces compileSurfaces collidingTypeNames graph (imp, modulePath) =
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
      allDecls <- compileTargetTopDecls compileSurfaces graph targetModule
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
importableTopDeclsForCompiledModule graph modulePath
  | isRecursiveImportGroup graph modulePath = do
      compileSurfaces <- compileSurfacesForGroup graph (importGroupFor graph modulePath)
      maybe
        (Left ("Internal error: missing compile surface for " ++ Mod.moduleIdDisplay modulePath))
        Right
        (Map.lookup modulePath compileSurfaces)
importableTopDeclsForCompiledModule graph modulePath = do
  cunit <- flattenModuleStrictCompileCompUnit graph modulePath
  publicInterface <- publicModuleInterface graph modulePath
  let localDecls = filter isImportableTopDecl (topDeclsFrom cunit)
  remoteDecls <- concat <$> mapM (compileSupportForRemoteItemGroup graph) (groupRemoteItemRefs modulePath (publicItemRefs publicInterface))
  pure (localDecls ++ shadowImportedDecls localDecls remoteDecls)

compileSupportForRemoteItemGroup :: ModuleGraph -> (Mod.ModuleId, [Name]) -> Either String [TopDecl]
compileSupportForRemoteItemGroup =
  compileSupportForRemoteItemGroupWithSurfaces Map.empty

compileSupportForRemoteItemGroupWithSurfaces ::
  Map Mod.ModuleId [TopDecl] ->
  ModuleGraph ->
  (Mod.ModuleId, [Name]) ->
  Either String [TopDecl]
compileSupportForRemoteItemGroupWithSurfaces compileSurfaces graph (targetModule, names) = do
  publicDecls <- publicTopDeclsForModule graph targetModule
  allDecls <- compileTargetTopDecls compileSurfaces graph targetModule
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
      seedFunctionNames =
        selectedFunctionNames ++ concatMap topDeclFunctionRefs supportNonFunctionDecls
      requiredFunctionNames = Set.fromList (functionDependencyClosure depMap seedFunctionNames)
      requiredFunctionDecls =
        [ TFunDef fd
          | fd <- allFunctionDecls,
            sigName (funSignature fd) `Set.member` requiredFunctionNames
        ]
  pure (requiredFunctionDecls ++ shadowImportedDecls requiredFunctionDecls supportNonFunctionDecls)

compileTargetTopDecls ::
  Map Mod.ModuleId [TopDecl] ->
  ModuleGraph ->
  Mod.ModuleId ->
  Either String [TopDecl]
compileTargetTopDecls compileSurfaces graph targetModule =
  maybe
    (importableTopDeclsForCompiledModule graph targetModule)
    Right
    (Map.lookup targetModule compileSurfaces)

compileSurfacesForGroup ::
  ModuleGraph ->
  [Mod.ModuleId] ->
  Either String (Map Mod.ModuleId [TopDecl])
compileSurfacesForGroup graph groupModules =
  go (0 :: Int) =<< initialSurfaces
  where
    maxIterations =
      max 8 (length groupModules * 12)

    initialSurfaces =
      Map.fromList
        <$> mapM
          ( \moduleId -> do
              unit <- lookupLoadedModule graph moduleId
              pure (moduleId, filter isImportableTopDecl (topDeclsFrom unit))
          )
          groupModules

    go iterations currentSurfaces
      | iterations > maxIterations =
          Left $
            "Module compile surface fixed point did not stabilize for recursive group:\n  "
              ++ intercalate ", " (map Mod.moduleIdDisplay groupModules)
      | otherwise = do
          nextSurfaces <- Map.fromList <$> mapM (stepSurface currentSurfaces) groupModules
          if nextSurfaces == currentSurfaces
            then pure nextSurfaces
            else go (iterations + 1) nextSurfaces

    stepSurface currentSurfaces moduleId = do
      (cunit, _, _, _) <- flattenModuleStrictCompileCompUnitWithSurfaces currentSurfaces graph moduleId
      publicInterface <- publicModuleInterface graph moduleId
      let localDecls = filter isImportableTopDecl (topDeclsFrom cunit)
      remoteDecls <-
        concat
          <$> mapM
            (compileSupportForRemoteItemGroupWithSurfaces currentSurfaces graph)
            (groupRemoteItemRefs moduleId (publicItemRefs publicInterface))
      pure (moduleId, localDecls ++ shadowImportedDecls localDecls remoteDecls)

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
    wrapperBody
      | isBuiltinPassthroughFunctionName originalName =
          forwardingWrapperBody sig originalName
      | otherwise =
          forwardingWrapperBody sig (hiddenFunctionName qualifier originalName)

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
  concatMap qualifyImpl requiredFds ++ concatMap qualifyWrapper exportedFds
  where
    allFds = [fd | TFunDef fd <- topDeclsFrom allUnit]
    allFunctionNames = Set.fromList [sigName (funSignature fd) | fd <- allFds]
    exportedNames = [sigName (funSignature fd) | TFunDef fd <- topDeclsFrom publicUnit]
    exportedFds =
      [ fd
        | fd <- allFds,
          sigName (funSignature fd) `elem` exportedNames
      ]
    depMap =
      Map.fromList
        [ ( sigName (funSignature fd),
            filter (`Set.member` allFunctionNames) (funDefFunctionRefs fd)
          )
          | fd <- allFds
        ]
    requiredFunctionNames =
      Set.fromList (functionDependencyClosure depMap exportedNames)
    requiredFds =
      [ fd
        | fd <- allFds,
          sigName (funSignature fd) `Set.member` requiredFunctionNames
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
      | isBuiltinPassthroughFunctionName (sigName (funSignature fd')) =
          []
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
renameStmtFunctionCalls renameMap (For initStmt cond postStmt body) =
  For
    (renameStmtFunctionCalls renameMap initStmt)
    (renameExpFunctionCalls renameMap cond)
    (renameStmtFunctionCalls renameMap postStmt)
    (renameBodyFunctionCalls renameMap body)

renameEquationFunctionCalls :: Map Name Name -> Equation -> Equation
renameEquationFunctionCalls renameMap (ps, body) =
  (ps, renameBodyFunctionCalls renameMap body)

renameExpFunctionCalls :: Map Name Name -> Exp -> Exp
renameExpFunctionCalls _ litExp@(Lit _) = litExp
renameExpFunctionCalls _ atExp@(ExpAt _) = atExp
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
renameStmtTypeRefs renameMap (For initStmt cond postStmt body) =
  For
    (renameStmtTypeRefs renameMap initStmt)
    (renameExpTypeRefs renameMap cond)
    (renameStmtTypeRefs renameMap postStmt)
    (renameBodyTypeRefs renameMap body)

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
renameExpTypeRefs _ atExp@(ExpAt _) = atExp
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
    ImportOnly _ selector -> do
      publicDecls <- publicTopDeclsForModule graph modulePath
      let names = selectedNamesFromAvailable (uniqueNames (concatMap topDeclNames publicDecls)) selector
      pure (mapMaybe toValidationImportStub (mapMaybe (selectTopDecl names) publicDecls))
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
strictCompileImportedDecls =
  strictCompileImportedDeclsWithSurfaces Map.empty

strictCompileImportedPartialTypes :: Set Name -> ModuleGraph -> (Import, Mod.ModuleId) -> Either String [(Name, [Name])]
strictCompileImportedPartialTypes =
  strictCompileImportedPartialTypesWithSurfaces Map.empty

strictCompileImportedDeclsWithSurfaces ::
  Map Mod.ModuleId [TopDecl] ->
  Set Name ->
  ModuleGraph ->
  (Import, Mod.ModuleId) ->
  Either String [TopDecl]
strictCompileImportedDeclsWithSurfaces compileSurfaces collidingTypeNames graph (imp, modulePath) =
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
      allDecls <- compileTargetTopDecls compileSurfaces graph modulePath
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
            let names = selectedNamesFromAvailable (uniqueNames (concatMap topDeclNames publicDecls)) selector
             in mapMaybe (selectTopDecl names) publicDecls
          selectedFunctionNames =
            [ sigName (funSignature fd)
              | TFunDef fd <- selectedPublicDecls
            ]
          seedFunctionNames =
            selectedFunctionNames ++ concatMap topDeclFunctionRefs supportNonFunctionDecls
          requiredFunctionNames = Set.fromList (functionDependencyClosure depMap seedFunctionNames)
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
            ++ shadowImportedDecls
              functionDecls
              renamedSupportNonFunctionDecls
        )

    moduleImportCompileDecls qualifier targetModule = do
      moduleBindings <- publicModuleBindingsForModule graph targetModule
      publicDecls <- publicTopDeclsForModule graph targetModule
      allDecls <- compileTargetTopDecls compileSurfaces graph targetModule
      let renameMap = importedFunctionRenameMap qualifier allDecls
          typeRenameMap = importedTypeRenameMap collidingTypeNames qualifier publicDecls
          allFunctionDecls = [fd | TFunDef fd <- allDecls]
          allFunctionNames = Set.fromList [sigName (funSignature fd) | fd <- allFunctionDecls]
          depMap =
            Map.fromList
              [ ( sigName (funSignature fd),
                  filter (`Set.member` allFunctionNames) (funDefFunctionRefs fd)
                )
                | fd <- allFunctionDecls
              ]
          publicFunctionNames =
            [ sigName (funSignature fd)
              | TFunDef fd <- publicDecls
            ]
          supportNonFunctionDecls = filter (not . isFunctionTopDecl) allDecls
          exportedFunctionNames =
            Set.fromList (functionDependencyClosure depMap publicFunctionNames)
          supportFunctionNames =
            Set.fromList
              ( functionDependencyClosure
                  depMap
                  (concatMap topDeclFunctionRefs supportNonFunctionDecls)
              )
          extraSupportFunctions =
            [ fd
              | fd <- allFunctionDecls,
                sigName (funSignature fd) `Set.member` Set.difference supportFunctionNames exportedFunctionNames
            ]
          localSupportDecls =
            map
              (renameTopDeclTypeRefs typeRenameMap . renameTopDeclFunctionCalls renameMap)
              supportNonFunctionDecls
          localSupportFunctionDecls =
            concatMap (qualifySupportImpl renameMap typeRenameMap qualifier) extraSupportFunctions
          requiredBuiltinSupportNames =
            Set.filter
              isBuiltinPassthroughFunctionName
              (Set.union exportedFunctionNames supportFunctionNames)
          builtinSupportDecls =
            [ TFunDef fd
              | fd <- allFunctionDecls,
                sigName (funSignature fd) `Set.member` requiredBuiltinSupportNames
            ]
      nestedSupportDecls <- concat <$> mapM (nestedModuleImportCompileDecls qualifier) moduleBindings
      pure
        ( builtinSupportDecls
            ++ localSupportFunctionDecls
            ++ localSupportDecls
            ++ shadowImportedDecls (localSupportFunctionDecls ++ localSupportDecls) nestedSupportDecls
        )

    nestedModuleImportCompileDecls qualifier (ExportedModuleBinding bindingName targetModule) =
      moduleImportCompileDecls (QualName qualifier (show bindingName)) targetModule

strictCompileImportedPartialTypesWithSurfaces ::
  Map Mod.ModuleId [TopDecl] ->
  Set Name ->
  ModuleGraph ->
  (Import, Mod.ModuleId) ->
  Either String [(Name, [Name])]
strictCompileImportedPartialTypesWithSurfaces _compileSurfaces collidingTypeNames graph (imp, modulePath) =
  case imp of
    ImportOnly moduleName selector ->
      importOnlyTypes (Mod.modulePathName moduleName) selector
    ImportModule moduleName ->
      moduleImportTypes (Mod.modulePathName moduleName) modulePath
    ImportAlias _ qualifier ->
      moduleImportTypes qualifier modulePath
  where
    importOnlyTypes qualifier selector = do
      publicInterface <- publicModuleInterface graph modulePath
      publicDecls <- publicTopDeclsForModule graph modulePath
      let names = selectedNamesFromAvailable (uniqueNames (concatMap topDeclNames publicDecls)) selector
          selectedRefs = selectExportedItemRefs names (publicItemRefs publicInterface)
          typeRenameMap = importedTypeRenameMap collidingTypeNames qualifier publicDecls
      partialVisibleImportedTypes typeRenameMap selectedRefs

    moduleImportTypes qualifier targetModule = do
      publicInterface <- publicModuleInterface graph targetModule
      publicDecls <- publicTopDeclsForModule graph targetModule
      let typeRenameMap = importedTypeRenameMap collidingTypeNames qualifier publicDecls
      partialVisibleImportedTypes typeRenameMap (publicItemRefs publicInterface)

    partialVisibleImportedTypes typeRenameMap itemRefs =
      concat <$> mapM (partialTypeInfo typeRenameMap) itemRefs

    partialTypeInfo typeRenameMap itemRef =
      case exportedItemConstructors itemRef of
        Nothing ->
          pure []
        Just visibleConstructors -> do
          fullConstructors <- fullConstructorNamesForRef graph itemRef
          let visibleSet = Set.fromList visibleConstructors
              fullSet = Set.fromList fullConstructors
              renamedTypeName = Map.findWithDefault (exportedItemName itemRef) (exportedItemName itemRef) typeRenameMap
          pure [(renamedTypeName, uniqueNames visibleConstructors) | visibleSet /= fullSet]

normalizePartialImportedTypes :: [(Name, [Name])] -> [(Name, [Name])]
normalizePartialImportedTypes partialTypes =
  [ (typeName, constructorNames)
    | (typeName, constructorNames) <- Map.toAscList merged
  ]
  where
    merged =
      Map.fromListWith
        (\xs ys -> uniqueNames (xs ++ ys))
        [(typeName, uniqueNames constructorNames) | (typeName, constructorNames) <- partialTypes]

fullConstructorNamesForRef :: ModuleGraph -> ExportedItemRef -> Either String [Name]
fullConstructorNamesForRef graph itemRef = do
  originUnit <- lookupLoadedModule graph (exportedItemOrigin itemRef)
  case findLocalDataType (exportedItemName itemRef) (topDeclsFrom originUnit) of
    Just (DataTy _ _ constrs) ->
      pure (uniqueNames (map (constructorLeafName . constrName) constrs))
    Nothing ->
      Left $
        "Internal error: exported data type not found: "
          ++ Mod.moduleIdDisplay (exportedItemOrigin itemRef)
          ++ "."
          ++ show (exportedItemName itemRef)

importedFunctionRenameMap :: Name -> [TopDecl] -> Map Name Name
importedFunctionRenameMap qualifier ds =
  Map.fromList
    [ (sigName (funSignature fd), hiddenFunctionName qualifier (sigName (funSignature fd)))
      | TFunDef fd <- ds,
        not (isBuiltinPassthroughFunctionName (sigName (funSignature fd)))
    ]

isBuiltinPassthroughFunctionName :: Name -> Bool
isBuiltinPassthroughFunctionName functionName =
  functionName `elem` passthroughNames
  where
    passthroughNames =
      [ Name "revert",
        Name "concatLit",
        Name "strlenLit",
        Name "keccakLit"
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
      | isBuiltinPassthroughFunctionName (sigName (funSignature fd)) =
          [TFunDef fd]
      | otherwise =
          [TFunDef (qualifyFunctionImpl renameMap qualifier fd)]
    wrapSelected fd
      | isBuiltinPassthroughFunctionName (sigName (funSignature fd)) =
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

qualifySupportImpl :: Map Name Name -> Map Name Name -> Name -> FunDef -> [TopDecl]
qualifySupportImpl renameMap typeRenameMap qualifier fd
  | isBuiltinPassthroughFunctionName (sigName (funSignature fd')) =
      []
  | otherwise =
      [TFunDef (qualifyFunctionImpl renameMap qualifier fd')]
  where
    fd' = renameFunDefTypeRefs typeRenameMap fd

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

topDeclFunctionRefs :: TopDecl -> [Name]
topDeclFunctionRefs (TFunDef fd) =
  funDefFunctionRefs fd
topDeclFunctionRefs (TInstDef inst) =
  concatMap funDefFunctionRefs (instFunctions inst)
topDeclFunctionRefs (TContr (Contract _ _ contractDecls)) =
  concatMap contractDeclFunctionRefs contractDecls
topDeclFunctionRefs _ =
  []

contractDeclFunctionRefs :: ContractDecl -> [Name]
contractDeclFunctionRefs (CFunDecl fd) =
  funDefFunctionRefs fd
contractDeclFunctionRefs (CFieldDecl (Field _ _ me)) =
  maybe [] expFunctionRefs me
contractDeclFunctionRefs (CConstrDecl (Constructor _ body)) =
  bodyFunctionRefs body
contractDeclFunctionRefs (CDataDecl _) =
  []

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
stmtFunctionRefs (For initStmt cond postStmt body) =
  stmtFunctionRefs initStmt
    ++ expFunctionRefs cond
    ++ stmtFunctionRefs postStmt
    ++ bodyFunctionRefs body

equationFunctionRefs :: Equation -> [Name]
equationFunctionRefs (_pats, body) =
  bodyFunctionRefs body

expFunctionRefs :: Exp -> [Name]
expFunctionRefs (Lit _) = []
expFunctionRefs (ExpAt _) = []
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
    localClassNames = concatMap topDeclClassNames localDecls
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
      | instName inst `elem` localClassNames = ((termNames, typeNames, classNames, instDecls), Nothing)
      | inst `elem` instDecls = ((termNames, typeNames, classNames, instDecls), Nothing)
      | otherwise =
          ( (termNames, typeNames, classNames, inst : instDecls),
            Just d
          )
    filterDecl seen (TExportDecl _) = (seen, Nothing)
    filterDecl seen (TPragmaDecl _) = (seen, Nothing)

filterImportedInstanceConflicts :: [TopDecl] -> [TopDecl] -> [TopDecl]
filterImportedInstanceConflicts localDecls =
  mapMaybe keepImportedDecl
  where
    localClassNames = concatMap topDeclClassNames localDecls

    keepImportedDecl d@(TInstDef inst)
      | instName inst `elem` localClassNames = Nothing
      | otherwise = Just d
    keepImportedDecl d = Just d

dedupeImportedInstanceDecls :: [TopDecl] -> [TopDecl]
dedupeImportedInstanceDecls =
  reverse . snd . foldl step ([], [])
  where
    step (seenHeads, acc) d@(TInstDef inst)
      | instanceDeclHeadKey inst `elem` seenHeads = (seenHeads, acc)
      | otherwise = (instanceDeclHeadKey inst : seenHeads, d : acc)
    step (seenHeads, acc) d = (seenHeads, d : acc)

instanceDeclHeadKey :: Instance -> (Bool, Name, [Ty], Ty)
instanceDeclHeadKey inst =
  (instDefault inst, instName inst, paramsTy inst, mainTy inst)

topDeclTermNames :: TopDecl -> [Name]
topDeclTermNames (TFunDef (FunDef sig _)) = [sigName sig]
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
applyImportVisibility (ImportOnly _ selector) =
  \topLevelDecls ->
    let names = selectedNamesFromAvailable (uniqueNames (concatMap topDeclNames topLevelDecls)) selector
     in mapMaybe (selectTopDecl names) topLevelDecls
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
  | otherwise = Nothing
selectTopDecl _ d@(TInstDef _) =
  Just d
selectTopDecl _ (TExportDecl _) =
  Nothing
selectTopDecl _ (TPragmaDecl _) =
  Nothing

selectTopDeclForExportRef :: ExportedItemRef -> TopDecl -> Maybe TopDecl
selectTopDeclForExportRef itemRef d@(TFunDef (FunDef sig _))
  | exportedItemName itemRef == sigName sig,
    exportedItemConstructors itemRef == Nothing =
      Just d
  | otherwise =
      Nothing
selectTopDeclForExportRef itemRef d@(TSym (TySym n _ _))
  | exportedItemName itemRef == n,
    exportedItemConstructors itemRef == Nothing =
      Just d
  | otherwise =
      Nothing
selectTopDeclForExportRef itemRef d@(TClassDef (Class _ _ n _ _ _))
  | exportedItemName itemRef == n,
    exportedItemConstructors itemRef == Nothing =
      Just d
  | otherwise =
      Nothing
selectTopDeclForExportRef itemRef d@(TContr (Contract n _ _))
  | exportedItemName itemRef == n,
    exportedItemConstructors itemRef == Nothing =
      Just d
  | otherwise =
      Nothing
selectTopDeclForExportRef itemRef (TDataDef (DataTy n ts cs))
  | exportedItemName itemRef /= n =
      Nothing
  | otherwise =
      case exportedItemConstructors itemRef of
        Just visibleConstructors ->
          Just (TDataDef (DataTy n ts (filterVisibleConstructors visibleConstructors cs)))
        Nothing ->
          Nothing
selectTopDeclForExportRef _ (TInstDef _) = Nothing
selectTopDeclForExportRef _ (TExportDecl _) = Nothing
selectTopDeclForExportRef _ (TPragmaDecl _) = Nothing

filterVisibleConstructors :: [Name] -> [Constr] -> [Constr]
filterVisibleConstructors visibleConstructors =
  filter (\constr -> constructorLeafName (constrName constr) `elem` visibleConstructors)

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

ensureNoModuleLookupConflicts :: ModuleGraph -> CompUnit -> [(Import, Mod.ModuleId)] -> Either String ()
ensureNoModuleLookupConflicts graph unit importPairs =
  case conflicts of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Conflicting unqualified names:",
            unlines (map (\n -> "  " ++ show n) xs)
          ]
  where
    localTermNames =
      uniqueNames (concatMap topDeclTermNames (topDeclsFrom unit))

    visibleModuleNames =
      uniqueNames (concatMap importVisibleModuleNames (imports unit))

    importedTermNames =
      uniqueNames $
        concatMap snd $
          mapMaybe importTermPair importPairs

    conflicts =
      uniqueNames
        ( filter (`elem` visibleModuleNames) localTermNames
            ++ filter (`elem` visibleModuleNames) importedTermNames
        )

    importTermPair (ImportOnly importPath selector, modulePath) =
      Just
        ( importPath,
          either (const []) id (resolveSelectedImportTermNames graph modulePath selector)
        )
    importTermPair _ =
      Nothing

resolveSelectedImportTermNames :: ModuleGraph -> Mod.ModuleId -> ItemSelector -> Either String [Name]
resolveSelectedImportTermNames graph modulePath selector = do
  publicDecls <- publicTopDeclsForModule graph modulePath
  let names = selectedNamesFromAvailable (uniqueNames (concatMap topDeclNames publicDecls)) selector
  pure (uniqueNames (concatMap topDeclTermNames (mapMaybe (selectTopDecl names) publicDecls)))

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

uniqueTopDecls :: [TopDecl] -> [TopDecl]
uniqueTopDecls = reverse . fst . foldl step ([], Map.empty)
  where
    step (acc, seen) decl
      | Map.member decl seen = (acc, seen)
      | otherwise = (decl : acc, Map.insert decl () seen)

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

importVisibleModuleNames :: Import -> [Name]
importVisibleModuleNames (ImportModule importPath) =
  uniqueNames $
    concatMap modulePrefixesForQualifier (importModuleQualifiers importPath)
importVisibleModuleNames (ImportAlias _ qualifier) =
  [qualifier]
importVisibleModuleNames (ImportOnly _ _) =
  []

modulePrefixesForQualifier :: Name -> [Name]
modulePrefixesForQualifier n =
  reverse (go n)
  where
    go q@(QualName p _) = q : go p
    go x = [x]

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
    duplicateItems (ImportOnly moduleName selector) =
      [ "  " ++ Mod.modulePathDisplay moduleName ++ "." ++ show item
        | item <- duplicateNames (explicitSelectorNames selector)
      ]
        ++ [ "  " ++ Mod.modulePathDisplay moduleName ++ " hiding " ++ show item
             | item <- duplicateNames (explicitHiddenNames selector)
           ]
    duplicateItems _ = []

explicitSelectorNames :: ItemSelector -> [Name]
explicitSelectorNames (SelectItems items _) =
  [itemName | SelectItem itemName <- items]

explicitExportSelectorNames :: ExportSelector -> [Name]
explicitExportSelectorNames (SelectExportItems items) =
  [ itemName
    | item <- items,
      itemName <- case item of
        SelectExportItem exportItemName -> [exportItemName]
        SelectExportConstructors typeName _ -> [typeName]
        SelectExportAllItems -> []
  ]

explicitHiddenNames :: ItemSelector -> [Name]
explicitHiddenNames (SelectItems _ hidden) = hidden

hasExportSelectAll :: ExportSelector -> Bool
hasExportSelectAll (SelectExportItems items) =
  any isWildcard items
  where
    isWildcard SelectExportAllItems = True
    isWildcard _ = False
