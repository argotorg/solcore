module Solcore.Frontend.Module.Loader
  ( ModuleGraph (..),
    loadModuleGraph,
    flattenModuleValidationCompUnit,
    flattenModuleStrictCompileCompUnit,
    flattenModuleStrictValidationCompUnit,
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
import Data.Maybe (mapMaybe)
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
flattenModuleCompUnit graph modulePath =
  flattenModuleValidationCompUnit graph modulePath

flattenModuleValidationCompUnit :: ModuleGraph -> FilePath -> Either String CompUnit
flattenModuleValidationCompUnit graph modulePath = do
  unit <-
    maybe
      (Left ("Internal error: module not loaded: " ++ modulePath))
      Right
      (Map.lookup modulePath (modules graph))
  ensureNoAmbiguousSelectedImports unit
  ensureNoDuplicateModuleQualifiers unit
  ensureNoDuplicateSelectedItems unit
  let directDeps = Map.findWithDefault [] modulePath (dependencies graph)
      importPairs = zip (imports unit) directDeps
  ensureImportItemsExist graph importPairs
  let importedDecls = concatMap (importedDeclsFor graph) importPairs
      qualifiedDecls = concatMap (qualifiedImportDecls graph) importPairs
  pure (CompUnit (imports unit) (qualifiedDecls ++ importedDecls ++ topDeclsFrom unit))

flattenModuleStrictCompileCompUnit :: ModuleGraph -> FilePath -> Either String CompUnit
flattenModuleStrictCompileCompUnit graph modulePath = do
  unit <-
    maybe
      (Left ("Internal error: module not loaded: " ++ modulePath))
      Right
      (Map.lookup modulePath (modules graph))
  ensureNoAmbiguousSelectedImports unit
  ensureNoDuplicateModuleQualifiers unit
  ensureNoDuplicateSelectedItems unit
  let directDeps = Map.findWithDefault [] modulePath (dependencies graph)
      importPairs = zip (imports unit) directDeps
  ensureImportItemsExist graph importPairs
  let importedDecls = concatMap (importedDeclsFor graph) importPairs
      qualifiedDecls = concatMap (qualifiedImportDecls graph) importPairs
      localDecls = topDeclsFrom unit
      visibleImportedDecls = shadowImportedDecls localDecls importedDecls
  pure (CompUnit (imports unit) (qualifiedDecls ++ localDecls ++ visibleImportedDecls))

flattenModuleStrictValidationCompUnit :: ModuleGraph -> FilePath -> Either String CompUnit
flattenModuleStrictValidationCompUnit graph modulePath = do
  unit <-
    maybe
      (Left ("Internal error: module not loaded: " ++ modulePath))
      Right
      (Map.lookup modulePath (modules graph))
  ensureNoAmbiguousSelectedImports unit
  ensureNoDuplicateModuleQualifiers unit
  ensureNoDuplicateSelectedItems unit
  let directDeps = Map.findWithDefault [] modulePath (dependencies graph)
      importPairs = zip (imports unit) directDeps
  ensureImportItemsExist graph importPairs
  let importedDecls = concatMap (strictValidationImportedDecls graph) importPairs
      qualifiedDecls = concatMap (qualifiedImportStubDecls graph) importPairs
      localDecls = topDeclsFrom unit
      visibleImportedDecls = shadowImportedDecls localDecls importedDecls
  pure (CompUnit (imports unit) (qualifiedDecls ++ localDecls ++ visibleImportedDecls))

ensureImportItemsExist :: ModuleGraph -> [(Import, FilePath)] -> Either String ()
ensureImportItemsExist graph importPairs =
  case concatMap unknowns importPairs of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Unknown selected imports:",
            unlines xs
          ]
  where
    unknowns (ImportOnly moduleName names, modulePath) =
      let available =
            maybe [] importableNamesFromCompUnit (Map.lookup modulePath (modules graph))
          missing = filter (`notElem` available) names
       in [formatMissing moduleName n | n <- missing]
    unknowns _ = []

formatMissing :: Name -> Name -> String
formatMissing moduleName itemName =
  "  " ++ show moduleName ++ "." ++ show itemName

importableNamesFromCompUnit :: CompUnit -> [Name]
importableNamesFromCompUnit (CompUnit _ ds) =
  concatMap topDeclNames ds

topDeclNames :: TopDecl -> [Name]
topDeclNames (TFunDef (FunDef sig _)) = [sigName sig]
topDeclNames (TSym (TySym n _ _)) = [n]
topDeclNames (TClassDef (Class _ _ n _ _ _)) = [n]
topDeclNames (TContr (Contract n _ _)) = [n]
topDeclNames (TDataDef (DataTy n _ cs)) = n : map constrName cs
topDeclNames (TInstDef _) = []
topDeclNames (TPragmaDecl _) = []

qualifiedImportDecls :: ModuleGraph -> (Import, FilePath) -> [TopDecl]
qualifiedImportDecls graph (imp, modulePath) =
  case imp of
    ImportOnly _ _ -> []
    ImportModule n -> qualifyDecls n
    ImportAlias _ n -> qualifyDecls n
  where
    qualifyDecls qualifier =
      case Map.lookup modulePath (modules graph) of
        Nothing -> []
        Just cunit ->
          qualifiedFunctionDecls qualifier cunit
            ++ qualifiedTypeDecls qualifier cunit

qualifiedImportStubDecls :: ModuleGraph -> (Import, FilePath) -> [TopDecl]
qualifiedImportStubDecls graph (imp, modulePath) =
  case imp of
    ImportOnly _ _ -> []
    ImportModule n -> stubDecls n
    ImportAlias _ n -> stubDecls n
  where
    stubDecls qualifier =
      case Map.lookup modulePath (modules graph) of
        Nothing -> []
        Just cunit ->
          qualifiedFunctionStubDecls qualifier cunit
            ++ qualifiedTypeStubDecls qualifier cunit

qualifyFunction :: Name -> FunDef -> FunDef
qualifyFunction qualifier (FunDef sig body) =
  FunDef
    (sig {sigName = QualName qualifier (show (sigName sig))})
    body

qualifiedFunctionDecls :: Name -> CompUnit -> [TopDecl]
qualifiedFunctionDecls qualifier cunit =
  [ TFunDef (qualifyFunction qualifier fd)
    | TFunDef fd <- topDeclsFrom cunit
  ]

qualifiedFunctionStubDecls :: Name -> CompUnit -> [TopDecl]
qualifiedFunctionStubDecls qualifier cunit =
  [ TFunDef (stubFunction (QualName qualifier (show (sigName (funSignature fd)))))
    | TFunDef fd <- topDeclsFrom cunit
  ]

qualifiedTypeDecls :: Name -> CompUnit -> [TopDecl]
qualifiedTypeDecls qualifier cunit =
  dataAliases ++ symAliases
  where
    dataAliases =
      [ TSym (qualifyTyCon qualifier n vs)
        | TDataDef (DataTy n vs _) <- topDeclsFrom cunit
      ]
    symAliases =
      [ TSym (qualifyTyCon qualifier n vs)
        | TSym (TySym n vs _) <- topDeclsFrom cunit
      ]

qualifiedTypeStubDecls :: Name -> CompUnit -> [TopDecl]
qualifiedTypeStubDecls qualifier cunit =
  dataAliases ++ symAliases
  where
    dataAliases =
      [ TSym (stubType (QualName qualifier (show n)))
        | TDataDef (DataTy n _ _) <- topDeclsFrom cunit
      ]
    symAliases =
      [ TSym (stubType (QualName qualifier (show n)))
        | TSym (TySym n _ _) <- topDeclsFrom cunit
      ]

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

importedDeclsFor :: ModuleGraph -> (Import, FilePath) -> [TopDecl]
importedDeclsFor graph (imp, modulePath) =
  applyImportVisibility imp topDecls
  where
    topDecls =
      topDeclsFrom (modules graph Map.! modulePath)

strictValidationImportedDecls :: ModuleGraph -> (Import, FilePath) -> [TopDecl]
strictValidationImportedDecls graph (imp, modulePath) =
  case imp of
    ImportOnly _ names -> mapMaybe (selectTopDecl names) topDecls
    ImportModule moduleName
      | isStdModuleName moduleName -> topDecls
      | otherwise -> []
    ImportAlias moduleName _
      | isStdModuleName moduleName -> topDecls
      | otherwise -> []
  where
    topDecls =
      topDeclsFrom (modules graph Map.! modulePath)

isStdModuleName :: Name -> Bool
isStdModuleName n = show n == "std"

shadowImportedDecls :: [TopDecl] -> [TopDecl] -> [TopDecl]
shadowImportedDecls localDecls =
  mapMaybe filterDecl
  where
    localTermNames = concatMap topDeclTermNames localDecls
    localTypeNames = concatMap topDeclTypeNames localDecls
    localClassNames = concatMap topDeclClassNames localDecls

    filterDecl d@(TFunDef (FunDef sig _))
      | sigName sig `elem` localTermNames = Nothing
      | otherwise = Just d
    filterDecl d@(TSym (TySym n _ _))
      | n `elem` localTypeNames = Nothing
      | otherwise = Just d
    filterDecl d@(TClassDef (Class _ _ n _ _ _))
      | n `elem` localClassNames = Nothing
      | otherwise = Just d
    filterDecl d@(TContr (Contract n _ _))
      | n `elem` localTypeNames = Nothing
      | otherwise = Just d
    filterDecl (TDataDef (DataTy n ts cs))
      | n `elem` localTypeNames = Nothing
      | otherwise =
          let cs' = filter (\c -> constrName c `notElem` localTermNames) cs
           in if null cs' && not (null cs)
                then Nothing
                else Just (TDataDef (DataTy n ts cs'))
    filterDecl d@(TInstDef _) = Just d
    filterDecl (TPragmaDecl _) = Nothing

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
applyImportVisibility (ImportOnly _ names) =
  mapMaybe (selectTopDecl names)
applyImportVisibility (ImportModule _) =
  id
applyImportVisibility (ImportAlias _ _) =
  id

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
selectTopDecl _ (TPragmaDecl _) =
  Nothing

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
    qualifiers =
      mapMaybe moduleQualifier imps
    dupMap :: Map Name Int
    dupMap =
      Map.fromListWith (+) [(q, 1) | q <- qualifiers]
    duplicates =
      [ q
        | (q, count) <- Map.toList dupMap,
          count > 1
      ]

moduleQualifier :: Import -> Maybe Name
moduleQualifier (ImportModule n) = Just n
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
    duplicateItems (ImportOnly moduleName items) =
      [ "  " ++ show moduleName ++ "." ++ show item
        | (item, count) <- Map.toList (Map.fromListWith (+) [(n, 1 :: Int) | n <- items]),
          count > 1
      ]
    duplicateItems _ = []
