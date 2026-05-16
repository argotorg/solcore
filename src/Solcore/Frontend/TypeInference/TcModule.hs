module Solcore.Frontend.TypeInference.TcModule
  ( CheckedModule (..),
    CheckedAssembly (..),
    ModuleTypeCheckInput (..),
    assembleCheckedModules,
    checkedModulesInOrder,
    loadModuleLocalTypeCheckInput,
    loadModuleTypeCheckInput,
    mkModuleTypeCheckInput,
    typeInferModule,
    typeInferModuleLocals,
  )
where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Set qualified as Set
import Solcore.Frontend.Module.Identity qualified as Mod
import Solcore.Frontend.Module.Loader
import Solcore.Frontend.Syntax.NameResolution
import Solcore.Frontend.Syntax
import Solcore.Frontend.Syntax.SyntaxTree qualified as Parsed
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.TcContract
import Solcore.Frontend.TypeInference.TcEnv
import Solcore.Pipeline.Options

data ModuleTypeCheckInput
  = ModuleTypeCheckInput
  { moduleTypeCheckCompUnit :: CompUnit Name,
    moduleLocalDeclKeys :: [TopDeclKey],
    moduleTrustedInstanceHeads :: [InstanceHead],
    modulePartialImportedTypes :: [(Name, [Name])]
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

loadModuleTypeCheckInput ::
  ModuleGraph ->
  Mod.ModuleId ->
  IO (Either String ModuleTypeCheckInput)
loadModuleTypeCheckInput graph moduleId =
  loadResolvedModuleTypeCheckInput (moduleTypeCheckCompUnitWithMetadata graph moduleId)

loadModuleLocalTypeCheckInput ::
  ModuleGraph ->
  Mod.ModuleId ->
  IO (Either String ModuleTypeCheckInput)
loadModuleLocalTypeCheckInput graph moduleId =
  loadResolvedModuleTypeCheckInput (moduleLocalTypeCheckCompUnitWithMetadata graph moduleId)

loadResolvedModuleTypeCheckInput ::
  Either String (Parsed.CompUnit, Int, Int, [(Name, [Name])]) ->
  IO (Either String ModuleTypeCheckInput)
loadResolvedModuleTypeCheckInput input =
  case input of
    Left err ->
      pure (Left err)
    Right (parsed, localStart, importedStart, partialImportedTypes) ->
      fmap
        (\resolved -> mkModuleTypeCheckInput resolved localStart importedStart partialImportedTypes)
        <$> nameResolution parsed

mkModuleTypeCheckInput ::
  CompUnit Name ->
  Int ->
  Int ->
  [(Name, [Name])] ->
  ModuleTypeCheckInput
mkModuleTypeCheckInput resolved@(CompUnit _ resolvedDecls) localStart importedStart partialImportedTypes =
  ModuleTypeCheckInput
    { moduleTypeCheckCompUnit = resolved,
      moduleLocalDeclKeys =
        take (importedStart - localStart) (drop localStart resolvedDecls) >>= topDeclKeys,
      moduleTrustedInstanceHeads =
        [ instanceHeadKey inst
          | TInstDef inst <- drop importedStart resolvedDecls
        ],
      modulePartialImportedTypes = partialImportedTypes
    }

typeInferModule ::
  Option ->
  ModuleTypeCheckInput ->
  IO (Either String (CompUnit Id, TcEnv))
typeInferModule opts input =
  typeInferWithTrustedInstanceHeadsAndPartialTypes
    opts
    (moduleTrustedInstanceHeads input)
    (moduleLocalDeclKeys input)
    (modulePartialImportedTypes input)
    (moduleTypeCheckCompUnit input)

typeInferModuleLocals ::
  Option ->
  ModuleTypeCheckInput ->
  IO (Either String (CompUnit Id, TcEnv))
typeInferModuleLocals opts input =
  typeInferWithImportedDeclMode
    TrustImportedDeclBodies
    opts
    (moduleTrustedInstanceHeads input)
    (moduleLocalDeclKeys input)
    (modulePartialImportedTypes input)
    (moduleTypeCheckCompUnit input)

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
importedModuleLeafName (Name n) = Name n
importedModuleLeafName (QualName _ n) = Name n

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
    qualifiedName = QualName qualifier (show originalName)
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
