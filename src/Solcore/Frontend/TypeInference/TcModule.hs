module Solcore.Frontend.TypeInference.TcModule
  ( ModuleTypeCheckInput (..),
    mkModuleTypeCheckInput,
    typeInferModule,
  )
where

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
