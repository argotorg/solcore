module Solcore.Frontend.Module.Loader
  ( ModuleGraph (..),
    LoadedModule (..),
    ModuleTypeCheckSurface (..),
    loadModuleGraph,
    moduleValidationTopDeclSegments,
    moduleSourcePath,
    moduleLocalTypeCheckSurface,
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
    referenceGroups :: Map Mod.ModuleId [Mod.ModuleId],
    moduleOrder :: [Mod.ModuleId],
    publicInterfaceCache :: Map Mod.ModuleId ModulePublicInterface
  }
  deriving (Eq, Show)

data ModuleTypeCheckSurface
  = ModuleTypeCheckSurface
  { moduleSurfaceImports :: [Import],
    moduleSurfaceQualifiedDecls :: [TopDecl],
    moduleSurfaceLocalDecls :: [TopDecl],
    moduleSurfaceImportedDecls :: [TopDecl],
    moduleSurfacePartialImportedTypes :: [(Name, [Name])]
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
      graph =
        ModuleGraph
          { entryModule = entryId,
            modules = loaded,
            dependencies = importDeps,
            referenceDependencies = refDeps,
            referenceGroups = buildGroupMap loaded refDeps,
            moduleOrder = reverse (loadOrder st),
            publicInterfaceCache = Map.empty
          }
  interfaces <- ExceptT $ pure (buildPublicInterfaceCache graph)
  pure graph {publicInterfaceCache = interfaces}

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
      map sccGroupModuleIds $
        stronglyConnComp
          [ (moduleId, moduleId, Map.findWithDefault [] moduleId depMap)
            | moduleId <- Map.keys loaded
          ]

    sccGroupModuleIds (AcyclicSCC moduleId) = [moduleId]
    sccGroupModuleIds (CyclicSCC moduleIds) = moduleIds

moduleSourcePath :: ModuleGraph -> Mod.ModuleId -> Either String FilePath
moduleSourcePath graph modulePath =
  maybe
    (Left ("Internal error: module not loaded: " ++ Mod.moduleIdDisplay modulePath))
    (Right . loadedSourcePath)
    (Map.lookup modulePath (modules graph))

moduleImportPairsFor :: ModuleGraph -> Mod.ModuleId -> CompUnit -> [(Import, Mod.ModuleId)]
moduleImportPairsFor graph modulePath unit =
  zip (imports unit) (Map.findWithDefault [] modulePath (dependencies graph))

referenceGroupFor :: ModuleGraph -> Mod.ModuleId -> [Mod.ModuleId]
referenceGroupFor graph modulePath =
  Map.findWithDefault [modulePath] modulePath (referenceGroups graph)

data ExportedItemRef
  = ExportedItemRef
  { exportedItemOrigin :: Mod.ModuleId,
    exportedItemSourceName :: Name,
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

        refOriginKey existingRef =
          ( exportedItemOrigin existingRef,
            exportedItemSourceName existingRef,
            isJust (exportedItemConstructors existingRef)
          )

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

prepareModuleImportContext :: ModuleGraph -> Mod.ModuleId -> Either String (CompUnit, FilePath, [(Import, Mod.ModuleId)])
prepareModuleImportContext graph modulePath = do
  unit <- lookupLoadedModule graph modulePath
  sourcePath <- moduleSourcePath graph modulePath
  let importPairs = moduleImportPairsFor graph modulePath unit
  ensureNoDuplicateModuleQualifiers unit
  ensureNoDuplicateSelectedItems unit
  ensureImportItemsExist graph importPairs
  ensureNoAmbiguousSelectedImports graph importPairs
  ensureNoModuleLookupConflicts graph unit importPairs
  _ <- publicModuleInterface graph modulePath
  pure (unit, sourcePath, importPairs)

moduleLocalTypeCheckSurface ::
  ModuleGraph ->
  Mod.ModuleId ->
  Either String ModuleTypeCheckSurface
moduleLocalTypeCheckSurface graph modulePath = do
  (unit, _sourcePath, importPairs) <- prepareModuleImportContext graph modulePath
  collidingTypeNames <- collidingImportedTypeNames graph importPairs
  importedDecls <-
    dedupeImportedInstanceDecls
      . concat
      <$> mapM (typeCheckImportedDecls collidingTypeNames graph) importPairs
  partialImportedTypes <-
    concat <$> mapM (importedPartialTypes collidingTypeNames graph) importPairs
  qualifiedDecls <-
    concat <$> mapM (typeCheckQualifiedImportDecls collidingTypeNames graph) importPairs
  let localDecls = topDeclsFrom unit
      visibleImportedDecls = uniqueTopDecls (filterImportedInstanceConflicts localDecls importedDecls)
  pure
    ModuleTypeCheckSurface
      { moduleSurfaceImports = imports unit,
        moduleSurfaceQualifiedDecls = qualifiedDecls,
        moduleSurfaceLocalDecls = localDecls,
        moduleSurfaceImportedDecls = visibleImportedDecls,
        moduleSurfacePartialImportedTypes = normalizePartialImportedTypes partialImportedTypes
      }

stubTopDeclBody :: TopDecl -> TopDecl
stubTopDeclBody (TContr (Contract n vs contractDecls)) =
  TContr (Contract n vs (map stubContractDeclBody contractDecls))
stubTopDeclBody (TFunDef fd) =
  TFunDef (stubFunDefBody fd)
stubTopDeclBody (TInstDef (Instance d vs predCtx n ts t _funs)) =
  TInstDef (Instance d vs predCtx n ts t [])
stubTopDeclBody decl =
  decl

stubContractDeclBody :: ContractDecl -> ContractDecl
stubContractDeclBody (CFieldDecl (Field n ty _initExp)) =
  CFieldDecl (Field n ty Nothing)
stubContractDeclBody (CFunDecl fd) =
  CFunDecl (stubFunDefBody fd)
stubContractDeclBody (CConstrDecl (Constructor params _body)) =
  CConstrDecl (Constructor params [])
stubContractDeclBody decl =
  decl

stubFunDefBody :: FunDef -> FunDef
stubFunDefBody (FunDef sig _body) =
  FunDef sig []

moduleValidationTopDeclSegments :: ModuleGraph -> Mod.ModuleId -> Either String ([Import], [[TopDecl]])
moduleValidationTopDeclSegments graph modulePath = do
  (unit, _sourcePath, importPairs) <- prepareModuleImportContext graph modulePath
  importedDecls <- concat <$> mapM (validationImportedDecls graph) importPairs
  qualifiedDecls <- concat <$> mapM (qualifiedImportStubDecls graph) importPairs
  let localDecls = topDeclsFrom unit
      visibleImportedDecls = uniqueTopDecls (filterImportedInstanceConflicts localDecls importedDecls)
  pure (imports unit, [qualifiedDecls, localDecls, visibleImportedDecls])

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
  selectedImportLocalNamesFromAvailable available selector

selectedImportLocalNamesFromAvailable :: [Name] -> ItemSelector -> Either String [Name]
selectedImportLocalNamesFromAvailable available selector =
  uniqueNames . map snd <$> selectedImportBindingsFromAvailable available selector

selectedImportBindingsFromAvailable :: [Name] -> ItemSelector -> Either String [(Name, Name)]
selectedImportBindingsFromAvailable available (SelectItems items hidden) =
  pure (uniqueBindingsByLocal (filterVisible (concatMap expand items)))
  where
    hiddenNames = uniqueNames hidden
    filterVisible =
      filter (\(sourceName, _) -> sourceName `notElem` hiddenNames)
    expand SelectAllItems = [(itemName, itemName) | itemName <- available]
    expand (SelectItem itemName) = [(itemName, itemName)]
    expand (SelectItemAs itemName aliasName) = [(itemName, aliasName)]

uniqueBindingsByLocal :: [(Name, Name)] -> [(Name, Name)]
uniqueBindingsByLocal =
  reverse . fst . foldl step ([], Map.empty)
  where
    step (acc, seen) binding@(_, localName)
      | Map.member localName seen = (acc, seen)
      | otherwise = (binding : acc, Map.insert localName () seen)

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
publicModuleInterface graph modulePath =
  case Map.lookup modulePath (publicInterfaceCache graph) of
    Just publicInterface ->
      Right publicInterface
    Nothing -> do
      interfaces <- publicInterfacesForGroup graph (referenceGroupFor graph modulePath)
      maybe
        (Left ("Internal error: missing public interface for " ++ Mod.moduleIdDisplay modulePath))
        Right
        (Map.lookup modulePath interfaces)

buildPublicInterfaceCache :: ModuleGraph -> Either String (Map Mod.ModuleId ModulePublicInterface)
buildPublicInterfaceCache graph =
  foldM addGroup Map.empty (uniqueReferenceGroups graph)
  where
    addGroup cache groupModules
      | all (`Map.member` cache) groupModules =
          Right cache
      | otherwise = do
          interfaces <- publicInterfacesForGroup (graph {publicInterfaceCache = cache}) groupModules
          Right (Map.union interfaces cache)

uniqueReferenceGroups :: ModuleGraph -> [[Mod.ModuleId]]
uniqueReferenceGroups graph =
  reverse groups
  where
    (groups, _) = foldl step ([], Set.empty) (map (referenceGroupFor graph) (moduleOrder graph))

    step (acc, seen) groupModules =
      let groupKey = Set.fromList groupModules
       in if groupKey `Set.member` seen
            then (acc, seen)
            else (groupModules : acc, Set.insert groupKey seen)

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
        (validateExportDecl sourcePath moduleId unit)
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

    validateExportDecl sourcePath moduleId unit exportDecl =
      case exportDecl of
        ExportList specs ->
          mapM_ (validateExportSpec sourcePath moduleId unit) specs
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

    validateExportSpec sourcePath moduleId unit spec =
      case spec of
        ExportName itemName -> do
          refs <- visibleExportRefsForNameFixed graph groupModules interfaces moduleId unit itemName
          ensureVisibleExportExists sourcePath itemName refs
        ExportNameWithConstructors typeName constructorSelector -> do
          _ <- visibleConstructorExportRefFixed graph groupModules interfaces moduleId sourcePath unit typeName constructorSelector
          pure ()
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
expandExportSpecFixed graph groupModules currentInterfaces currentModule sourcePath unit (ExportName itemName) = do
  refs <- visibleExportRefsForNameFixed graph groupModules currentInterfaces currentModule unit itemName
  ensureVisibleExportExists sourcePath itemName refs
  pure emptyPublicInterface {publicItemRefs = map stripConstructorVisibility refs}
expandExportSpecFixed graph groupModules currentInterfaces currentModule sourcePath unit (ExportNameWithConstructors typeName constructorSelector) = do
  ref <- visibleConstructorExportRefFixed graph groupModules currentInterfaces currentModule sourcePath unit typeName constructorSelector
  pure emptyPublicInterface {publicItemRefs = [ref]}
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

visibleExportRefsForNameFixed ::
  ModuleGraph ->
  [Mod.ModuleId] ->
  Map Mod.ModuleId ModulePublicInterface ->
  Mod.ModuleId ->
  CompUnit ->
  Name ->
  Either String [ExportedItemRef]
visibleExportRefsForNameFixed graph groupModules currentInterfaces currentModule unit itemName = do
  importedRefs <- selectedImportedExportRefsFixed graph groupModules currentInterfaces currentModule unit
  let localRefs = localExportRefsForName currentModule itemName (topDeclsFrom unit)
      matchingImportedRefs =
        [ ref
          | ref <- importedRefs,
            exportedItemName ref == itemName
        ]
  pure (localRefs ++ matchingImportedRefs)

visibleConstructorExportRefFixed ::
  ModuleGraph ->
  [Mod.ModuleId] ->
  Map Mod.ModuleId ModulePublicInterface ->
  Mod.ModuleId ->
  FilePath ->
  CompUnit ->
  Name ->
  ConstructorSelector ->
  Either String ExportedItemRef
visibleConstructorExportRefFixed graph groupModules currentInterfaces currentModule sourcePath unit typeName constructorSelector =
  case findLocalDataType typeName (topDeclsFrom unit) of
    Just _ -> do
      ensureLocalConstructorExportExists sourcePath (topDeclsFrom unit) typeName constructorSelector
      pure (localDataExportRef currentModule typeName (resolveLocalConstructorSelection typeName constructorSelector (topDeclsFrom unit)))
    Nothing -> do
      importedRefs <- selectedImportedExportRefsFixed graph groupModules currentInterfaces currentModule unit
      case selectVisibleConstructors importedRefs typeName constructorSelector of
        Nothing ->
          Left $
            unlines
              [ "Unknown export:",
                "  " ++ sourcePath,
                "  " ++ show typeName
              ]
        Just ref
          | missingVisibleConstructors constructorSelector ref /= [] ->
              Left $
                unlines
                  [ "Unknown exported constructors:",
                    "  " ++ sourcePath,
                    unlines ["  " ++ show typeName ++ "." ++ show constructorName | constructorName <- missingVisibleConstructors constructorSelector ref]
                  ]
        Just ref ->
          pure ref

selectedImportedExportRefsFixed ::
  ModuleGraph ->
  [Mod.ModuleId] ->
  Map Mod.ModuleId ModulePublicInterface ->
  Mod.ModuleId ->
  CompUnit ->
  Either String [ExportedItemRef]
selectedImportedExportRefsFixed graph groupModules currentInterfaces currentModule unit =
  concat <$> mapM refsForImport (moduleImportPairsFor graph currentModule unit)
  where
    refsForImport (ImportOnly _ selector, targetModule) = do
      availableRefs <- itemRefsForModule targetModule
      bindings <- selectedImportBindingsFromAvailable (uniqueNames (map exportedItemName availableRefs)) selector
      pure (selectImportedItemRefs bindings availableRefs)
    refsForImport _ =
      pure []

    itemRefsForModule targetModule
      | targetModule `elem` groupModules =
          pure (maybe [] publicItemRefs (Map.lookup targetModule currentInterfaces))
      | otherwise =
          publicItemRefs <$> publicModuleInterface graph targetModule

ensureVisibleExportExists :: FilePath -> Name -> [ExportedItemRef] -> Either String ()
ensureVisibleExportExists sourcePath itemName refs
  | any ((== itemName) . exportedItemName) refs = Right ()
  | otherwise =
      Left $
        unlines
          [ "Unknown export:",
            "  " ++ sourcePath,
            "  " ++ show itemName
          ]

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

selectImportedItemRefs :: [(Name, Name)] -> [ExportedItemRef] -> [ExportedItemRef]
selectImportedItemRefs bindings refs =
  concatMap pick bindings
  where
    pick (sourceName, localName) =
      [ ref {exportedItemName = localName}
        | ref <- refs,
          exportedItemName ref == sourceName
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
      [ ExportedItemRef currentModule itemName itemName Nothing
        | itemName <- topDeclNames decl
      ]

localDataExportRef :: Mod.ModuleId -> Name -> [Name] -> ExportedItemRef
localDataExportRef currentModule typeName visibleConstructors =
  ExportedItemRef currentModule typeName typeName (Just (uniqueNames visibleConstructors))

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
          Set.size (Set.fromList [(exportedItemOrigin ref, exportedItemSourceName ref) | ref <- refs]) > 1
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

qualifyFunctionSignature :: Name -> FunDef -> FunDef
qualifyFunctionSignature qualifier (FunDef sig body) =
  FunDef
    (sig {sigName = QualName qualifier (show (sigName sig))})
    body

qualifiedFunctionStubDecls :: Name -> CompUnit -> [TopDecl]
qualifiedFunctionStubDecls qualifier cunit =
  [ TFunDef (stubFunction (QualName qualifier (show (sigName (funSignature fd)))))
    | TFunDef fd <- topDeclsFrom cunit
  ]

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
renameStmtTypeRefs renameMap (Block body) =
  Block (renameBodyTypeRefs renameMap body)
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

validationImportedDecls :: ModuleGraph -> (Import, Mod.ModuleId) -> Either String [TopDecl]
validationImportedDecls graph (imp, modulePath) =
  case imp of
    ImportOnly _ selector -> do
      publicDecls <- publicTopDeclsForModule graph modulePath
      bindings <- selectedImportBindingsFromAvailable (uniqueNames (concatMap topDeclNames publicDecls)) selector
      pure (mapMaybe toValidationImportStub (mapMaybe (selectImportedTopDecl bindings) publicDecls))
    ImportModule _ ->
      Right []
    ImportAlias _ _ ->
      Right []

toValidationImportStub :: TopDecl -> Maybe TopDecl
toValidationImportStub (TFunDef (FunDef sig _)) =
  Just (TFunDef (stubFunction (sigName sig)))
toValidationImportStub (TSym (TySym n _ _)) =
  Just (TSym (stubType n))
toValidationImportStub (TClassDef (Class bvs _ n pvs mv sigs)) =
  Just (TClassDef (Class bvs [] n pvs mv (map stripSigContext sigs)))
  where
    stripSigContext (Signature vs _ sn ps ret) = Signature vs [] sn ps ret
toValidationImportStub (TContr (Contract n _ _)) =
  Just (TContr (Contract n [] []))
toValidationImportStub (TDataDef (DataTy n _ cs)) =
  Just (TDataDef (DataTy n [] [Constr (constrName c) [] | c <- cs]))
toValidationImportStub (TInstDef _) = Nothing
toValidationImportStub (TExportDecl _) = Nothing
toValidationImportStub (TPragmaDecl _) = Nothing

typeCheckQualifiedImportDecls :: Set Name -> ModuleGraph -> (Import, Mod.ModuleId) -> Either String [TopDecl]
typeCheckQualifiedImportDecls collidingTypeNames graph (imp, modulePath) =
  case imp of
    ImportOnly _ _ -> Right []
    ImportModule importPath ->
      concat <$> mapM (`qualifyDecls` modulePath) (importModuleQualifiers importPath)
    ImportAlias _ qualifier ->
      qualifyDecls qualifier modulePath
  where
    qualifyDecls qualifier targetModule = do
      moduleBindings <- publicModuleBindingsForModule graph targetModule
      publicDecls <- publicTopDeclsForModule graph targetModule
      nestedDecls <- concat <$> mapM (qualifyNestedModule qualifier) moduleBindings
      let typeRenameMap = importedTypeRenameMap collidingTypeNames qualifier publicDecls
          publicUnit = CompUnit [] publicDecls
      pure $
        qualifiedFunctionSignatureDecls typeRenameMap qualifier publicUnit
          ++ qualifiedTypeAliasDecls typeRenameMap qualifier publicUnit
          ++ nestedDecls

    qualifyNestedModule qualifier (ExportedModuleBinding bindingName targetModule) =
      qualifyDecls (QualName qualifier (show bindingName)) targetModule

qualifiedFunctionSignatureDecls :: Map Name Name -> Name -> CompUnit -> [TopDecl]
qualifiedFunctionSignatureDecls typeRenameMap qualifier cunit =
  [ TFunDef (stubFunDefBody (qualifyFunctionSignature qualifier fd'))
    | TFunDef fd <- topDeclsFrom cunit,
      let fd' = renameFunDefTypeRefs typeRenameMap fd
  ]

typeCheckImportedDecls :: Set Name -> ModuleGraph -> (Import, Mod.ModuleId) -> Either String [TopDecl]
typeCheckImportedDecls collidingTypeNames graph (imp, modulePath) =
  case imp of
    ImportOnly moduleName selector ->
      importOnlyDecls (Mod.modulePathName moduleName) selector
    ImportModule moduleName ->
      moduleImportDecls (Mod.modulePathName moduleName) modulePath
    ImportAlias _ qualifier ->
      moduleImportDecls qualifier modulePath
  where
    importOnlyDecls qualifier selector = do
      publicDecls <- publicTopDeclsForModule graph modulePath
      supportDecls <- typeCheckSupportNonFunctionDecls graph modulePath
      bindings <- selectedImportBindingsFromAvailable (uniqueNames (concatMap topDeclNames publicDecls)) selector
      let selectedPublicDecls = mapMaybe (selectImportedTopDecl bindings) publicDecls
          typeRenameMap = importedTypeRenameMap collidingTypeNames qualifier publicDecls
          selectedFunctionDecls =
            [ TFunDef (stubFunDefBody (renameFunDefTypeRefs typeRenameMap fd))
              | TFunDef fd <- selectedPublicDecls
            ]
          supportNonFunctionDecls =
            map
              (stubTopDeclBody . renameTopDeclTypeRefs typeRenameMap)
              supportDecls
      pure (selectedFunctionDecls ++ shadowImportedDecls selectedFunctionDecls supportNonFunctionDecls)

    moduleImportDecls qualifier targetModule = do
      moduleBindings <- publicModuleBindingsForModule graph targetModule
      publicDecls <- publicTopDeclsForModule graph targetModule
      supportDecls <- typeCheckSupportNonFunctionDecls graph targetModule
      nestedSupportDecls <- concat <$> mapM (nestedModuleImportDecls qualifier) moduleBindings
      let typeRenameMap = importedTypeRenameMap collidingTypeNames qualifier publicDecls
          localSupportDecls =
            map
              (stubTopDeclBody . renameTopDeclTypeRefs typeRenameMap)
              supportDecls
      pure (localSupportDecls ++ shadowImportedDecls localSupportDecls nestedSupportDecls)

    nestedModuleImportDecls qualifier (ExportedModuleBinding bindingName targetModule) =
      moduleImportDecls (QualName qualifier (show bindingName)) targetModule

typeCheckSupportNonFunctionDecls :: ModuleGraph -> Mod.ModuleId -> Either String [TopDecl]
typeCheckSupportNonFunctionDecls graph =
  typeCheckSupportNonFunctionDeclsSeen graph Set.empty

typeCheckSupportNonFunctionDeclsSeen :: ModuleGraph -> Set Mod.ModuleId -> Mod.ModuleId -> Either String [TopDecl]
typeCheckSupportNonFunctionDeclsSeen graph seen modulePath
  | modulePath `Set.member` seen = Right []
  | otherwise = do
      unit <- lookupLoadedModule graph modulePath
      publicDecls <- publicTopDeclsForModule graph modulePath
      let importPairs = moduleImportPairsFor graph modulePath unit
          localSupport = filterSupport (topDeclsFrom unit)
          publicSupport = filterSupport publicDecls
          ownSupport = publicSupport ++ shadowImportedDecls publicSupport localSupport
      collidingTypeNames <- collidingImportedTypeNames graph importPairs
      importedSupport <-
        concat <$> mapM (supportFromImport collidingTypeNames (Set.insert modulePath seen)) importPairs
      pure (ownSupport ++ shadowImportedDecls ownSupport importedSupport)
  where
    filterSupport =
      filter (\decl -> isImportableTopDecl decl && not (isFunctionTopDecl decl))

    supportFromImport collidingTypeNames seen' (imp, targetModule) = do
      publicDecls <- publicTopDeclsForModule graph targetModule
      supportDecls <- typeCheckSupportNonFunctionDeclsSeen graph seen' targetModule
      let typeRenameMap = importedTypeRenameMap collidingTypeNames (importQualifier imp) publicDecls
      pure (map (renameTopDeclTypeRefs typeRenameMap) supportDecls)

    importQualifier (ImportOnly moduleName _) = Mod.modulePathName moduleName
    importQualifier (ImportModule moduleName) = Mod.modulePathName moduleName
    importQualifier (ImportAlias _ qualifier) = qualifier

importedPartialTypes ::
  Set Name ->
  ModuleGraph ->
  (Import, Mod.ModuleId) ->
  Either String [(Name, [Name])]
importedPartialTypes collidingTypeNames graph (imp, modulePath) =
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
      bindings <- selectedImportBindingsFromAvailable (uniqueNames (concatMap topDeclNames publicDecls)) selector
      let selectedRefs = selectImportedItemRefs bindings (publicItemRefs publicInterface)
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
  case findLocalDataType (exportedItemSourceName itemRef) (topDeclsFrom originUnit) of
    Just (DataTy _ _ constrs) ->
      pure (uniqueNames (map (constructorLeafName . constrName) constrs))
    Nothing ->
      Left $
        "Internal error: exported data type not found: "
          ++ Mod.moduleIdDisplay (exportedItemOrigin itemRef)
          ++ "."
          ++ show (exportedItemSourceName itemRef)

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

selectImportedTopDecl :: [(Name, Name)] -> TopDecl -> Maybe TopDecl
selectImportedTopDecl _ d@(TInstDef _) =
  Just d
selectImportedTopDecl bindings decl =
  case find (\(sourceName, _) -> sourceName `elem` topDeclNames decl) bindings of
    Just (sourceName, localName) ->
      Just (renameTopDeclName sourceName localName decl)
    Nothing ->
      Nothing

renameTopDeclName :: Name -> Name -> TopDecl -> TopDecl
renameTopDeclName oldName newName decl
  | oldName == newName = decl
  | otherwise =
      case decl of
        TFunDef (FunDef sig body)
          | sigName sig == oldName ->
              TFunDef (FunDef (sig {sigName = newName}) body)
        TSym sym@(TySym n _ _)
          | n == oldName ->
              TSym (sym {symName = newName})
        TClassDef (Class defaults vars n params var sigs)
          | n == oldName ->
              TClassDef (Class defaults vars newName params var sigs)
        TContr (Contract n params contractDecls)
          | n == oldName ->
              TContr (Contract newName params contractDecls)
        TDataDef (DataTy n params constrs)
          | n == oldName ->
              TDataDef (DataTy newName params constrs)
        _ ->
          decl

selectTopDeclForExportRef :: ExportedItemRef -> TopDecl -> Maybe TopDecl
selectTopDeclForExportRef itemRef d@(TFunDef (FunDef sig _))
  | exportedItemSourceName itemRef == sigName sig,
    exportedItemConstructors itemRef == Nothing =
      Just (renameTopDeclName (exportedItemSourceName itemRef) (exportedItemName itemRef) d)
  | otherwise =
      Nothing
selectTopDeclForExportRef itemRef d@(TSym (TySym n _ _))
  | exportedItemSourceName itemRef == n,
    exportedItemConstructors itemRef == Nothing =
      Just (renameTopDeclName (exportedItemSourceName itemRef) (exportedItemName itemRef) d)
  | otherwise =
      Nothing
selectTopDeclForExportRef itemRef d@(TClassDef (Class _ _ n _ _ _))
  | exportedItemSourceName itemRef == n,
    exportedItemConstructors itemRef == Nothing =
      Just (renameTopDeclName (exportedItemSourceName itemRef) (exportedItemName itemRef) d)
  | otherwise =
      Nothing
selectTopDeclForExportRef itemRef d@(TContr (Contract n _ _))
  | exportedItemSourceName itemRef == n,
    exportedItemConstructors itemRef == Nothing =
      Just (renameTopDeclName (exportedItemSourceName itemRef) (exportedItemName itemRef) d)
  | otherwise =
      Nothing
selectTopDeclForExportRef itemRef (TDataDef (DataTy n ts cs))
  | exportedItemSourceName itemRef /= n =
      Nothing
  | otherwise =
      case exportedItemConstructors itemRef of
        Just visibleConstructors ->
          Just (TDataDef (DataTy (exportedItemName itemRef) ts (filterVisibleConstructors visibleConstructors cs)))
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
  bindings <- selectedImportBindingsFromAvailable (uniqueNames (concatMap topDeclNames publicDecls)) selector
  pure (uniqueNames (concatMap topDeclTermNames (mapMaybe (selectImportedTopDecl bindings) publicDecls)))

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
        ++ [ "  " ++ Mod.modulePathDisplay moduleName ++ " as " ++ show item
             | item <- duplicateNames (explicitSelectorLocalNames selector)
           ]
        ++ [ "  " ++ Mod.modulePathDisplay moduleName ++ " hiding " ++ show item
             | item <- duplicateNames (explicitHiddenNames selector)
           ]
    duplicateItems _ = []

explicitSelectorNames :: ItemSelector -> [Name]
explicitSelectorNames (SelectItems items _) =
  [ itemName
    | item <- items,
      itemName <- case item of
        SelectItem itemName -> [itemName]
        SelectItemAs itemName _ -> [itemName]
        SelectAllItems -> []
  ]

explicitSelectorLocalNames :: ItemSelector -> [Name]
explicitSelectorLocalNames (SelectItems items _) =
  [ itemName
    | item <- items,
      itemName <- case item of
        SelectItem itemName -> [itemName]
        SelectItemAs _ aliasName -> [aliasName]
        SelectAllItems -> []
  ]

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
