module Solcore.Frontend.TypeInference.TcModule
  ( CheckedModule (..),
    CheckedAssembly (..),
    ModuleDeclSegment (..),
    ModuleInferenceDecl (..),
    ModuleResolvedTypeCheckInput (..),
    ModuleTypeCheckInput,
    assembleCheckedModules,
    checkedModulesInOrder,
    loadModuleLocalTypeCheckInput,
    mapModuleInferenceTopDecls,
    moduleInitialInferenceDecls,
    moduleInferenceImportedDecls,
    moduleInferenceLocalDecls,
    moduleInferenceQualifiedDecls,
    moduleInferenceDecls,
    modulePartialImportedTypes,
    moduleResolvedImportedDecls,
    moduleResolvedImports,
    moduleResolvedLocalDecls,
    moduleResolvedQualifiedDecls,
    moduleTrustedInstanceHeads,
    moduleInferenceTopDecls,
    retagModuleInferenceDecls,
    traverseModuleInferenceTopDecls,
    typeInferModuleLocals,
    withPreparedModuleInferenceDecls,
  )
where

import Data.List (foldl')
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Solcore.Frontend.Module.Identity qualified as Mod
import Solcore.Frontend.Module.Loader
import Solcore.Frontend.Syntax
import Solcore.Frontend.Syntax.NameResolution
import Solcore.Frontend.Syntax.SyntaxTree qualified as Parsed
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.TcContract
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Pipeline.Options

data ModuleDeclSegment
  = ModuleQualifiedDecl
  | ModuleLocalDecl
  | ModuleImportedDecl
  deriving (Eq, Show)

data ModuleInferenceDecl
  = ModuleInferenceDecl
  { moduleInferenceDeclSegment :: ModuleDeclSegment,
    moduleInferenceDeclTopDecl :: TopDecl Name
  }
  deriving (Eq, Show)

data ModuleResolvedTypeCheckInput
  = ModuleResolvedTypeCheckInput
  { moduleResolvedInputImports :: [Import],
    moduleResolvedInputQualifiedDecls :: [TopDecl Name],
    moduleResolvedInputLocalDecls :: [TopDecl Name],
    moduleResolvedInputImportedDecls :: [TopDecl Name],
    moduleResolvedInputTrustedInstanceHeads :: [InstanceHead],
    moduleResolvedInputPartialImportedTypes :: [(Name, [Name])]
  }
  deriving (Eq, Show)

data ModuleTypeCheckInput
  = ModuleTypeCheckInput
  { moduleTypeCheckResolvedInput :: ModuleResolvedTypeCheckInput,
    modulePreparedInferenceDecls :: [ModuleInferenceDecl]
  }
  deriving (Eq, Show)

data CheckedModule
  = CheckedModule
  { checkedModuleId :: Mod.ModuleId,
    checkedModuleInput :: ModuleTypeCheckInput,
    checkedModuleNoDesugar :: CompUnit Id,
    checkedModuleTyped :: CompUnit Id,
    checkedModuleEnv :: TcEnv
  }

data CheckedAssembly
  = CheckedAssembly
  { checkedAssemblyCompUnit :: CompUnit Id,
    checkedAssemblyEnv :: TcEnv
  }

loadModuleLocalTypeCheckInput ::
  ModuleGraph ->
  Mod.ModuleId ->
  IO (Either String ModuleResolvedTypeCheckInput)
loadModuleLocalTypeCheckInput graph moduleId =
  loadResolvedModuleTypeCheckInput (moduleLocalTypeCheckSurface graph moduleId)

loadResolvedModuleTypeCheckInput ::
  Either String ModuleTypeCheckSurface ->
  IO (Either String ModuleResolvedTypeCheckInput)
loadResolvedModuleTypeCheckInput input =
  case input of
    Left err ->
      pure (Left err)
    Right surface -> do
      resolved <-
        nameResolutionTopDeclSegments
          (moduleSurfaceImports surface)
          [ moduleSurfaceQualifiedDecls surface,
            moduleSurfaceLocalDecls surface,
            moduleSurfaceImportedDecls surface
          ]
      pure (resolved >>= mkModuleResolvedTypeCheckInput surface)

mkModuleResolvedTypeCheckInput ::
  ModuleTypeCheckSurface ->
  (CompUnit Name, [[TopDecl Name]]) ->
  Either String ModuleResolvedTypeCheckInput
mkModuleResolvedTypeCheckInput surface (resolved, resolvedSegments) =
  case resolvedSegments of
    [resolvedQualifiedDecls, resolvedLocalDecls, resolvedImportedDecls] ->
      Right
        ModuleResolvedTypeCheckInput
          { moduleResolvedInputImports = imports resolved,
            moduleResolvedInputQualifiedDecls = resolvedQualifiedDecls,
            moduleResolvedInputLocalDecls = resolvedLocalDecls,
            moduleResolvedInputImportedDecls = resolvedImportedDecls,
            moduleResolvedInputTrustedInstanceHeads =
              [ instanceHeadKey inst
                | TInstDef inst <- resolvedImportedDecls
              ],
            moduleResolvedInputPartialImportedTypes = moduleSurfacePartialImportedTypes surface
          }
    _ ->
      Left $
        "Internal error: expected 3 resolved module typecheck surface segments, got "
          ++ show (length resolvedSegments)

typeInferModuleLocals ::
  Option ->
  ModuleTypeCheckInput ->
  IO (Either String (CompUnit Id, TcEnv))
typeInferModuleLocals opts input =
  typeInferTopDeclChecks
    opts
    (moduleResolvedImports resolvedInput)
    (moduleTrustedInstanceHeads resolvedInput)
    (modulePartialImportedTypes resolvedInput)
    (moduleTopDeclChecks input)
  where
    resolvedInput = moduleTypeCheckResolvedInput input

moduleResolvedImports :: ModuleResolvedTypeCheckInput -> [Import]
moduleResolvedImports =
  moduleResolvedInputImports

moduleResolvedQualifiedDecls :: ModuleResolvedTypeCheckInput -> [TopDecl Name]
moduleResolvedQualifiedDecls =
  moduleResolvedInputQualifiedDecls

moduleResolvedLocalDecls :: ModuleResolvedTypeCheckInput -> [TopDecl Name]
moduleResolvedLocalDecls =
  moduleResolvedInputLocalDecls

moduleResolvedImportedDecls :: ModuleResolvedTypeCheckInput -> [TopDecl Name]
moduleResolvedImportedDecls =
  moduleResolvedInputImportedDecls

moduleTrustedInstanceHeads :: ModuleResolvedTypeCheckInput -> [InstanceHead]
moduleTrustedInstanceHeads =
  moduleResolvedInputTrustedInstanceHeads

modulePartialImportedTypes :: ModuleResolvedTypeCheckInput -> [(Name, [Name])]
modulePartialImportedTypes =
  moduleResolvedInputPartialImportedTypes

moduleInitialInferenceDecls :: ModuleResolvedTypeCheckInput -> [ModuleInferenceDecl]
moduleInitialInferenceDecls input =
  taggedInferenceDecls ModuleQualifiedDecl (moduleResolvedQualifiedDecls input)
    ++ taggedInferenceDecls ModuleLocalDecl (moduleResolvedLocalDecls input)
    ++ taggedInferenceDecls ModuleImportedDecl (moduleResolvedImportedDecls input)

moduleInferenceDecls :: ModuleTypeCheckInput -> [ModuleInferenceDecl]
moduleInferenceDecls =
  modulePreparedInferenceDecls

moduleInferenceQualifiedDecls :: ModuleTypeCheckInput -> [TopDecl Name]
moduleInferenceQualifiedDecls input =
  moduleInferenceDeclsInSegment ModuleQualifiedDecl (moduleInferenceDecls input)

moduleInferenceLocalDecls :: ModuleTypeCheckInput -> [TopDecl Name]
moduleInferenceLocalDecls input =
  moduleInferenceDeclsInSegment ModuleLocalDecl (moduleInferenceDecls input)

moduleInferenceImportedDecls :: ModuleTypeCheckInput -> [TopDecl Name]
moduleInferenceImportedDecls input =
  moduleInferenceDeclsInSegment ModuleImportedDecl (moduleInferenceDecls input)

withPreparedModuleInferenceDecls :: ModuleResolvedTypeCheckInput -> [ModuleInferenceDecl] -> ModuleTypeCheckInput
withPreparedModuleInferenceDecls input inferenceDecls =
  ModuleTypeCheckInput
    { moduleTypeCheckResolvedInput = input,
      modulePreparedInferenceDecls = inferenceDecls
    }

moduleInferenceTopDecls :: [ModuleInferenceDecl] -> [TopDecl Name]
moduleInferenceTopDecls =
  map moduleInferenceDeclTopDecl

mapModuleInferenceTopDecls ::
  ([TopDecl Name] -> [TopDecl Name]) ->
  [ModuleInferenceDecl] ->
  [ModuleInferenceDecl]
mapModuleInferenceTopDecls pass inferenceDecls =
  retagModuleInferenceDecls inferenceDecls (pass (moduleInferenceTopDecls inferenceDecls))

traverseModuleInferenceTopDecls ::
  (Functor f) =>
  ([TopDecl Name] -> f [TopDecl Name]) ->
  [ModuleInferenceDecl] ->
  f [ModuleInferenceDecl]
traverseModuleInferenceTopDecls pass inferenceDecls =
  retagModuleInferenceDecls inferenceDecls <$> pass (moduleInferenceTopDecls inferenceDecls)

moduleTopDeclChecks :: ModuleTypeCheckInput -> [TopDeclCheck Name]
moduleTopDeclChecks =
  map moduleInferenceDeclCheck . moduleInferenceDecls

moduleInferenceDeclCheck :: ModuleInferenceDecl -> TopDeclCheck Name
moduleInferenceDeclCheck inferenceDecl =
  TopDeclCheck
    { topDeclCheckMode = moduleDeclSegmentCheckMode (moduleInferenceDeclSegment inferenceDecl),
      topDeclCheckDecl = moduleInferenceDeclTopDecl inferenceDecl
    }

moduleDeclSegmentCheckMode :: ModuleDeclSegment -> TopDeclCheckMode
moduleDeclSegmentCheckMode ModuleLocalDecl = CheckTopDeclBody
moduleDeclSegmentCheckMode ModuleQualifiedDecl = TrustTopDeclBody
moduleDeclSegmentCheckMode ModuleImportedDecl = TrustTopDeclBody

retagModuleInferenceDecls ::
  [ModuleInferenceDecl] ->
  [TopDecl Name] ->
  [ModuleInferenceDecl]
retagModuleInferenceDecls inferenceDecls =
  map (retagModuleInferenceDecl keySegments)
  where
    keySegments =
      moduleInferenceDeclSegmentByKey inferenceDecls

retagModuleInferenceDecl :: Map TopDeclKey ModuleDeclSegment -> TopDecl Name -> ModuleInferenceDecl
retagModuleInferenceDecl keySegments topDecl =
  ModuleInferenceDecl (retaggedDeclSegment keySegments topDecl) topDecl

retaggedDeclSegment :: Map TopDeclKey ModuleDeclSegment -> TopDecl Name -> ModuleDeclSegment
retaggedDeclSegment keySegments topDecl =
  chooseSegment knownSegments
  where
    knownSegments =
      [ segment
        | key <- topDeclKeys topDecl,
          Just segment <- [Map.lookup key keySegments]
      ]
    chooseSegment segments
      | ModuleLocalDecl `elem` segments = ModuleLocalDecl
      | ModuleQualifiedDecl `elem` segments = ModuleQualifiedDecl
      | ModuleImportedDecl `elem` segments = ModuleImportedDecl
      | otherwise = ModuleLocalDecl

moduleInferenceDeclSegmentByKey :: [ModuleInferenceDecl] -> Map TopDeclKey ModuleDeclSegment
moduleInferenceDeclSegmentByKey =
  foldl' addDecl Map.empty
  where
    addDecl segments inferenceDecl =
      foldl' addKey segments (topDeclKeys (moduleInferenceDeclTopDecl inferenceDecl))
      where
        segment = moduleInferenceDeclSegment inferenceDecl
        addKey acc key =
          Map.insertWith (\_ old -> old) key segment acc

taggedInferenceDecls :: ModuleDeclSegment -> [TopDecl Name] -> [ModuleInferenceDecl]
taggedInferenceDecls segment =
  map (ModuleInferenceDecl segment)

moduleInferenceDeclsInSegment :: ModuleDeclSegment -> [ModuleInferenceDecl] -> [TopDecl Name]
moduleInferenceDeclsInSegment segment =
  map moduleInferenceDeclTopDecl
    . filter ((== segment) . moduleInferenceDeclSegment)

checkedModulesInOrder ::
  ModuleGraph ->
  Map Mod.ModuleId CheckedModule ->
  Either String [CheckedModule]
checkedModulesInOrder graph checkedModules =
  mapM lookupCheckedModule (moduleOrder graph)
  where
    lookupCheckedModule moduleId =
      maybe
        (Left ("Internal error: module was not typechecked: " ++ Mod.moduleIdDisplay moduleId))
        Right
        (Map.lookup moduleId checkedModules)

assembleCheckedModules ::
  ModuleGraph ->
  Map Mod.ModuleId CheckedModule ->
  Either String CheckedAssembly
assembleCheckedModules graph checkedModules = do
  orderedModules <- checkedModulesInOrder graph checkedModules
  entryCheckedModule <-
    maybe
      (Left ("Internal error: entry module was not typechecked: " ++ Mod.moduleIdDisplay (entryModule graph)))
      Right
      (Map.lookup (entryModule graph) checkedModules)
  importWrappers <- importForwardingWrappers graph checkedModules
  let assembledCompUnit =
        CompUnit
          (imports (checkedModuleTyped entryCheckedModule))
          (assemblyDecls orderedModules importWrappers)
  pure $
    CheckedAssembly
      { checkedAssemblyCompUnit = assembledCompUnit,
        checkedAssemblyEnv = mergeCheckedModuleEnvs entryCheckedModule orderedModules
      }

assemblyDecls :: [CheckedModule] -> [TopDecl Id] -> [TopDecl Id]
assemblyDecls orderedModules extraDecls =
  moduleDecls ++ dedupeNewFunctionDecls moduleFunctionNames extraDecls
  where
    moduleDecls = concatMap (contracts . checkedModuleTyped) orderedModules
    moduleFunctionNames = concatMap topDeclFunctionNames moduleDecls

dedupeNewFunctionDecls :: [Name] -> [TopDecl Id] -> [TopDecl Id]
dedupeNewFunctionDecls existingNames =
  go (Set.fromList existingNames)
  where
    go _ [] = []
    go seen (decl : rest)
      | any (`Set.member` seen) names =
          go seen rest
      | otherwise =
          decl : go (foldr Set.insert seen names) rest
      where
        names = topDeclFunctionNames decl

topDeclFunctionNames :: TopDecl Id -> [Name]
topDeclFunctionNames (TFunDef fd) =
  [sigName (funSignature fd)]
topDeclFunctionNames (TMutualDef mutualDecls) =
  concatMap topDeclFunctionNames mutualDecls
topDeclFunctionNames _ =
  []

mergeCheckedModuleEnvs :: CheckedModule -> [CheckedModule] -> TcEnv
mergeCheckedModuleEnvs entryCheckedModule orderedModules =
  (checkedModuleEnv entryCheckedModule)
    { typeTable =
        Map.unions (map (typeTable . checkedModuleEnv) orderedModules)
    }

importForwardingWrappers ::
  ModuleGraph ->
  Map Mod.ModuleId CheckedModule ->
  Either String [TopDecl Id]
importForwardingWrappers graph checkedModules =
  concat <$> mapM wrappersForLoadedModule (Map.elems (modules graph))
  where
    wrappersForLoadedModule loadedModule =
      concat <$> mapM (wrappersForImport loadedModule) (Parsed.imports (loadedCompUnit loadedModule))

    wrappersForImport loadedModule (Parsed.ImportModule importPath) =
      wrappersForQualifiers loadedModule importPath (defaultImportQualifiers importPath)
    wrappersForImport loadedModule (Parsed.ImportAlias importPath qualifier) =
      wrappersForQualifiers loadedModule importPath [qualifier]
    wrappersForImport _ (Parsed.ImportOnly _ _) =
      pure []

    wrappersForQualifiers loadedModule importPath qualifiers = do
      targetModuleId <-
        maybe
          (Left ("Internal error: import target was not loaded: " ++ Mod.modulePathDisplay importPath))
          Right
          (Map.lookup importPath (loadedModuleRefs loadedModule))
      targetModule <-
        maybe
          (Left ("Internal error: import target was not typechecked: " ++ Mod.moduleIdDisplay targetModuleId))
          Right
          (Map.lookup targetModuleId checkedModules)
      pure
        [ TFunDef (typedForwardingWrapper qualifier fd)
          | qualifier <- qualifiers,
            TFunDef fd <- contracts (checkedModuleTyped targetModule)
        ]

defaultImportQualifiers :: Parsed.ModulePath -> [Name]
defaultImportQualifiers importPath =
  if leafName == fullName
    then [leafName]
    else [leafName, fullName]
  where
    fullName = Mod.modulePathName importPath
    leafName = importedModuleLeafName fullName

importedModuleLeafName :: Name -> Name
importedModuleLeafName n@(Name _) = n
importedModuleLeafName q@(QualName _ n) = copyNameSourceSpan q (Name n)

typedForwardingWrapper :: Name -> FunDef Id -> FunDef Id
typedForwardingWrapper qualifier (FunDef sig body)
  | originalName == Name "revert" =
      FunDef
        (sig {sigName = qualifiedName})
        body
  | otherwise =
      FunDef
        (sig {sigName = qualifiedName})
        [Return (Call Nothing targetId args)]
  where
    originalName = sigName sig
    qualifiedName = qualifyName qualifier originalName
    targetId = Id originalName (typedSignatureType sig)
    args = map (Var . paramName) (sigParams sig)

typedSignatureType :: Signature Id -> Ty
typedSignatureType sig =
  funtype (map typedParamType (sigParams sig)) returnType
  where
    returnType =
      maybe
        (error ("no return type in checked signature of: " ++ show (sigName sig)))
        id
        (sigReturn sig)

typedParamType :: Param Id -> Ty
typedParamType (Typed i _) = idType i
typedParamType (Untyped i) = idType i
