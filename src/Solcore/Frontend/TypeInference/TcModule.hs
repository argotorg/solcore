module Solcore.Frontend.TypeInference.TcModule
  ( CheckedModule (..),
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

loadModuleTypeCheckInput ::
  ModuleGraph ->
  Mod.ModuleId ->
  IO (Either String ModuleTypeCheckInput)
loadModuleTypeCheckInput graph moduleId =
  case flattenModuleStrictCompileCompUnitWithMetadata graph moduleId of
    Left err ->
      pure (Left err)
    Right (parsed, localStart, importedStart, partialImportedTypes) ->
      fmap
        (\resolved -> mkModuleTypeCheckInput resolved localStart importedStart partialImportedTypes)
        <$> nameResolution parsed

loadModuleLocalTypeCheckInput ::
  ModuleGraph ->
  Mod.ModuleId ->
  IO (Either String ModuleTypeCheckInput)
loadModuleLocalTypeCheckInput graph moduleId =
  case flattenModuleStrictCompileCompUnitWithMetadata graph moduleId of
    Left err ->
      pure (Left err)
    Right (parsed, localStart, importedStart, partialImportedTypes) ->
      fmap
        (\resolved -> mkModuleTypeCheckInput resolved localStart importedStart partialImportedTypes)
        <$> nameResolution (stubNonLocalDeclBodies localStart importedStart parsed)

stubNonLocalDeclBodies :: Int -> Int -> Parsed.CompUnit -> Parsed.CompUnit
stubNonLocalDeclBodies localStart importedStart (Parsed.CompUnit imps topDecls) =
  Parsed.CompUnit imps (zipWith stubAt [(0 :: Int) ..] topDecls)
  where
    stubAt index decl
      | localStart <= index && index < importedStart = decl
      | otherwise = stubTopDeclBody decl

stubTopDeclBody :: Parsed.TopDecl -> Parsed.TopDecl
stubTopDeclBody (Parsed.TContr (Parsed.Contract n vs contractDecls)) =
  Parsed.TContr (Parsed.Contract n vs (map stubContractDeclBody contractDecls))
stubTopDeclBody (Parsed.TFunDef fd) =
  Parsed.TFunDef (stubFunDefBody fd)
stubTopDeclBody (Parsed.TInstDef (Parsed.Instance d vs predCtx n ts t _funs)) =
  Parsed.TInstDef (Parsed.Instance d vs predCtx n ts t [])
stubTopDeclBody decl =
  decl

stubContractDeclBody :: Parsed.ContractDecl -> Parsed.ContractDecl
stubContractDeclBody (Parsed.CFieldDecl (Parsed.Field n ty _initExp)) =
  Parsed.CFieldDecl (Parsed.Field n ty Nothing)
stubContractDeclBody (Parsed.CFunDecl fd) =
  Parsed.CFunDecl (stubFunDefBody fd)
stubContractDeclBody (Parsed.CConstrDecl (Parsed.Constructor params _body)) =
  Parsed.CConstrDecl (Parsed.Constructor params [])
stubContractDeclBody decl =
  decl

stubFunDefBody :: Parsed.FunDef -> Parsed.FunDef
stubFunDefBody (Parsed.FunDef sig _body) =
  Parsed.FunDef sig []

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
  Either String (CompUnit Id)
assembleCheckedModules graph checkedModules = do
  orderedModules <- checkedModulesInOrder graph checkedModules
  entryCheckedModule <-
    maybe
      (Left ("Internal error: entry module was not typechecked: " ++ Mod.moduleIdDisplay (entryModule graph)))
      Right
      (Map.lookup (entryModule graph) checkedModules)
  pure $
    CompUnit
      (imports (checkedModuleTyped entryCheckedModule))
      (concatMap (contracts . checkedModuleTyped) orderedModules)
