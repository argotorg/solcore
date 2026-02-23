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
  _ <- moduleExportItems modulePath unit
  let directDeps = Map.findWithDefault [] modulePath (dependencies graph)
      importPairs = zip (imports unit) directDeps
  ensureNoAmbiguousSelectedImports graph importPairs
  ensureNoDuplicateModuleQualifiers unit
  ensureNoDuplicateSelectedItems unit
  ensureImportItemsExist graph importPairs
  importedDecls <- concat <$> mapM (importedDeclsFor graph) importPairs
  qualifiedDecls <- concat <$> mapM (qualifiedImportDecls graph) importPairs
  pure (CompUnit (imports unit) (qualifiedDecls ++ importedDecls ++ topDeclsFrom unit))

flattenModuleStrictCompileCompUnit :: ModuleGraph -> FilePath -> Either String CompUnit
flattenModuleStrictCompileCompUnit graph modulePath = do
  unit <-
    maybe
      (Left ("Internal error: module not loaded: " ++ modulePath))
      Right
      (Map.lookup modulePath (modules graph))
  _ <- moduleExportItems modulePath unit
  let directDeps = Map.findWithDefault [] modulePath (dependencies graph)
      importPairs = zip (imports unit) directDeps
  ensureNoAmbiguousSelectedImports graph importPairs
  ensureNoDuplicateModuleQualifiers unit
  ensureNoDuplicateSelectedItems unit
  ensureImportItemsExist graph importPairs
  importedDecls <- concat <$> mapM (strictCompileImportedDecls graph) importPairs
  qualifiedDecls <- concat <$> mapM (qualifiedImportDecls graph) importPairs
  let localDecls = topDeclsFrom unit
      visibleImportedDecls = shadowImportedDecls localDecls importedDecls
  pure (CompUnit (imports unit) (qualifiedDecls ++ localDecls ++ visibleImportedDecls))

flattenModuleStrictValidationCompUnit :: ModuleGraph -> FilePath -> Either String CompUnit
flattenModuleStrictValidationCompUnit graph modulePath = do
  unit <-
    maybe
      (Left ("Internal error: module not loaded: " ++ modulePath))
      Right
      (Map.lookup modulePath (modules graph))
  _ <- moduleExportItems modulePath unit
  let directDeps = Map.findWithDefault [] modulePath (dependencies graph)
      importPairs = zip (imports unit) directDeps
  ensureNoAmbiguousSelectedImports graph importPairs
  ensureNoDuplicateModuleQualifiers unit
  ensureNoDuplicateSelectedItems unit
  ensureImportItemsExist graph importPairs
  importedDecls <- concat <$> mapM (strictValidationImportedDecls graph) importPairs
  qualifiedDecls <- concat <$> mapM (qualifiedImportStubDecls graph) importPairs
  let localDecls = topDeclsFrom unit
      visibleImportedDecls = shadowImportedDecls localDecls importedDecls
  pure (CompUnit (imports unit) (qualifiedDecls ++ localDecls ++ visibleImportedDecls))

ensureImportItemsExist :: ModuleGraph -> [(Import, FilePath)] -> Either String ()
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
    unknowns (ImportOnly moduleName items, modulePath) = do
      selected <- resolveSelectedImportItems graph moduleName modulePath items
      available <- importableNamesForModule graph modulePath
      let missing = filter (`notElem` available) selected
      pure [formatMissing moduleName n | n <- missing]
    unknowns _ = pure []

formatMissing :: Name -> Name -> String
formatMissing moduleName itemName =
  "  " ++ show moduleName ++ "." ++ show itemName

resolveSelectedImportItems :: ModuleGraph -> Name -> FilePath -> ItemSelector -> Either String [Name]
resolveSelectedImportItems graph _moduleName modulePath SelectAll =
  importableNamesForModule graph modulePath
resolveSelectedImportItems _graph _moduleName _modulePath (SelectOnly items) =
  Right items

importableNamesForModule :: ModuleGraph -> FilePath -> Either String [Name]
importableNamesForModule graph modulePath = do
  publicDecls <- publicTopDeclsForModule graph modulePath
  pure (uniqueNames (concatMap topDeclNames publicDecls))

publicTopDeclsForModule :: ModuleGraph -> FilePath -> Either String [TopDecl]
publicTopDeclsForModule graph modulePath = do
  unit <-
    maybe
      (Left ("Internal error: module not loaded: " ++ modulePath))
      Right
      (Map.lookup modulePath (modules graph))
  publicTopDeclsFromCompUnit modulePath unit

publicTopDeclsFromCompUnit :: FilePath -> CompUnit -> Either String [TopDecl]
publicTopDeclsFromCompUnit modulePath unit@(CompUnit _ ds) = do
  exports <- requiredExportItems modulePath unit
  let importableDecls = filter isImportableTopDecl ds
  pure $
    mapMaybe (selectTopDecl exports) importableDecls

moduleExportItems :: FilePath -> CompUnit -> Either String (Maybe [Name])
moduleExportItems modulePath (CompUnit _ ds) =
  case [items | TExportDecl (Export items) <- ds] of
    [] -> Right Nothing
    [items] -> do
      exports <- resolveExportItems modulePath ds items
      ensureNoDuplicateExports modulePath exports
      ensureExportsExist modulePath ds exports
      Right (Just exports)
    _ ->
      Left $
        unlines
          [ "Invalid export declaration:",
            "  " ++ modulePath,
            "  multiple export declarations are not allowed"
          ]

resolveExportItems :: FilePath -> [TopDecl] -> ItemSelector -> Either String [Name]
resolveExportItems _modulePath ds SelectAll =
  Right (availableExportNames ds)
resolveExportItems _modulePath _ds (SelectOnly items) =
  Right items

availableExportNames :: [TopDecl] -> [Name]
availableExportNames ds =
  uniqueNames (concatMap topDeclNames (filter isImportableTopDecl ds))

requiredExportItems :: FilePath -> CompUnit -> Either String [Name]
requiredExportItems modulePath unit = do
  exports <- moduleExportItems modulePath unit
  case exports of
    Just items -> Right items
    Nothing ->
      Left $
        unlines
          [ "Missing export declaration:",
            "  " ++ modulePath,
            "  imported modules must declare export { ... };"
          ]

ensureNoDuplicateExports :: FilePath -> [Name] -> Either String ()
ensureNoDuplicateExports modulePath names =
  case duplicates of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Duplicate names in export declaration:",
            "  " ++ modulePath,
            unlines (map (\n -> "  " ++ show n) xs)
          ]
  where
    dupMap :: Map Name Int
    dupMap = Map.fromListWith (+) [(n, 1) | n <- names]
    duplicates =
      [ n
        | (n, count) <- Map.toList dupMap,
          count > 1
      ]

ensureExportsExist :: FilePath -> [TopDecl] -> [Name] -> Either String ()
ensureExportsExist modulePath ds items =
  case missing of
    [] -> Right ()
    xs ->
      Left $
        unlines
          [ "Unknown exports:",
            "  " ++ modulePath,
            unlines (map (\n -> "  " ++ show n) xs)
          ]
  where
    available = availableExportNames ds
    missing = filter (`notElem` available) items

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

qualifiedImportDecls :: ModuleGraph -> (Import, FilePath) -> Either String [TopDecl]
qualifiedImportDecls graph (imp, modulePath) =
  case imp of
    ImportOnly _ _ -> Right []
    ImportModule n -> qualifyDecls n
    ImportAlias _ n -> qualifyDecls n
  where
    qualifyDecls qualifier = do
      publicDecls <- publicTopDeclsForModule graph modulePath
      let cunit = CompUnit [] publicDecls
      pure $
        qualifiedFunctionDecls qualifier cunit
          ++ qualifiedTypeDecls qualifier cunit

qualifiedImportStubDecls :: ModuleGraph -> (Import, FilePath) -> Either String [TopDecl]
qualifiedImportStubDecls graph (imp, modulePath) =
  case imp of
    ImportOnly _ _ -> Right []
    ImportModule n -> stubDecls n
    ImportAlias _ n -> stubDecls n
  where
    stubDecls qualifier = do
      publicDecls <- publicTopDeclsForModule graph modulePath
      let cunit = CompUnit [] publicDecls
      pure $
        qualifiedFunctionStubDecls qualifier cunit
          ++ qualifiedTypeStubDecls qualifier cunit

qualifyFunctionWrapper :: Name -> FunDef -> FunDef
qualifyFunctionWrapper qualifier (FunDef sig _body) =
  FunDef
    (sig {sigName = qualifiedName})
    wrapperBody
  where
    originalName = sigName sig
    qualifiedName = QualName qualifier (show originalName)
    implName = hiddenFunctionName qualifier originalName
    argNames = map sigParamName (sigParams sig)
    wrapperBody = [Return (ExpName Nothing implName (map (ExpVar Nothing) argNames))]

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
  QualName qualifier ("$impl$" ++ show originalName)

sigParamName :: Param -> Name
sigParamName (Typed n _) = n
sigParamName (Untyped n) = n

qualifiedFunctionDecls :: Name -> CompUnit -> [TopDecl]
qualifiedFunctionDecls qualifier cunit =
  concatMap qualify fds
  where
    fds = [fd | TFunDef fd <- topDeclsFrom cunit]
    renameMap =
      Map.fromList
        [ (sigName (funSignature fd), hiddenFunctionName qualifier (sigName (funSignature fd)))
          | fd <- fds,
            sigName (funSignature fd) /= Name "revert"
        ]
    qualify fd
      | sigName (funSignature fd) == Name "revert" =
          [TFunDef (qualifyRevertFunction qualifier fd)]
      | otherwise =
          [ TFunDef (qualifyFunctionImpl renameMap qualifier fd),
            TFunDef (qualifyFunctionWrapper qualifier fd)
          ]

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
  ExpName me' n' es'
  where
    me' = fmap (renameExpFunctionCalls renameMap) me
    n'
      | me == Nothing = Map.findWithDefault n n renameMap
      | otherwise = n
    es' = map (renameExpFunctionCalls renameMap) es
renameExpFunctionCalls renameMap (ExpVar me n) =
  ExpVar (fmap (renameExpFunctionCalls renameMap) me) n
renameExpFunctionCalls renameMap (ExpDotName n es) =
  ExpDotName n (map (renameExpFunctionCalls renameMap) es)
renameExpFunctionCalls _ (ExpDotVar n) =
  ExpDotVar n
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

importedDeclsFor :: ModuleGraph -> (Import, FilePath) -> Either String [TopDecl]
importedDeclsFor graph (imp, modulePath) =
  applyImportVisibility imp <$> publicTopDeclsForModule graph modulePath

strictValidationImportedDecls :: ModuleGraph -> (Import, FilePath) -> Either String [TopDecl]
strictValidationImportedDecls graph (imp, modulePath) =
  case imp of
    ImportOnly _ SelectAll ->
      mapMaybe toValidationImportStub <$> publicTopDeclsForModule graph modulePath
    ImportOnly _ (SelectOnly names) ->
      mapMaybe toValidationImportStub . mapMaybe (selectTopDecl names)
        <$> publicTopDeclsForModule graph modulePath
    ImportModule _ -> Right []
    ImportAlias _ _ -> Right []

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

strictCompileImportedDecls :: ModuleGraph -> (Import, FilePath) -> Either String [TopDecl]
strictCompileImportedDecls graph (imp, modulePath) =
  case imp of
    ImportOnly _ SelectAll ->
      publicTopDeclsForModule graph modulePath
    ImportOnly _ (SelectOnly names) ->
      mapMaybe (selectTopDecl names) <$> publicTopDeclsForModule graph modulePath
    ImportModule _ -> publicTopDeclsForModule graph modulePath
    ImportAlias _ _ -> publicTopDeclsForModule graph modulePath

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
    filterDecl (TExportDecl _) = Nothing
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

ensureNoAmbiguousSelectedImports :: ModuleGraph -> [(Import, FilePath)] -> Either String ()
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
      [ (item, uniqueNames mods)
        | (item, mods) <- Map.toList selections,
          length (uniqueNames mods) > 1
      ]
      where
        selections :: Map Name [Name]
        selections = Map.fromListWith (++) [(item, [modName]) | (item, modName) <- selectedPairs]

formatAmbiguous :: (Name, [Name]) -> String
formatAmbiguous (item, mods) =
  "  "
    ++ show item
    ++ " imported from "
    ++ intercalate ", " (map show mods)

uniqueNames :: [Name] -> [Name]
uniqueNames = reverse . fst . foldl step ([], Map.empty)
  where
    step (acc, seen) n
      | Map.member n seen = (acc, seen)
      | otherwise = (n : acc, Map.insert n () seen)

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
    duplicateItems (ImportOnly moduleName (SelectOnly items)) =
      [ "  " ++ show moduleName ++ "." ++ show item
        | (item, count) <- Map.toList (Map.fromListWith (+) [(n, 1 :: Int) | n <- items]),
          count > 1
      ]
    duplicateItems (ImportOnly _ SelectAll) = []
    duplicateItems _ = []
