module Solcore.Frontend.TypeInference.TcModule
  ( ModuleTypeCheckInput (..),
    loadModuleTypeCheckInput,
    mkModuleTypeCheckInput,
    typeInferModule,
  )
where

import Solcore.Frontend.Module.Identity qualified as Mod
import Solcore.Frontend.Module.Loader
import Solcore.Frontend.Syntax.NameResolution
import Solcore.Frontend.Syntax
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
