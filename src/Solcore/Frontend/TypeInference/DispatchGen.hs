-- | Module: Solcore.Frontend.TypeInference.DispatchGen
-- | Description: Desugaring for contract level method dispatch
module Solcore.Frontend.TypeInference.DispatchGen
  ( generateSelectorInstances
  , generateMainDecl
  ) where

import Solcore.Frontend.TypeInference.TcMonad (TcM)
import Solcore.Frontend.TypeInference.TcEnv (uniqueTypes)
import Solcore.Frontend.TypeInference.Id (Id(..))
import Solcore.Frontend.Syntax.Name (Name(..))
import Solcore.Frontend.Syntax.Ty (Ty(..))
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Stmt
import Solcore.Primitives.Primitives (unit)

import Crypto.Hash (digestFromByteString)
import Crypto.Hash.Algorithms (Keccak_256)

import Control.Monad (forM_)
import Control.Monad.Except (throwError)
import Control.Monad.State (gets)
import qualified Data.Map as Map

-- | Generates the `Selector` instances for the unique types of each method on a contract
generateSelectorInstances :: [FunDef Id] -> TcM ()
generateSelectorInstances methods = do
  tyMap <- gets uniqueTypes

  forM_ methods $ \m -> do
    let sig = funSignature m

    case Map.lookup (sigName sig) tyMap of
      Nothing -> throwError
        ("Internal Error: method " ++ show (sigName sig) ++ " does not have a generated unique type")
      Just ty -> do
        let body = Return . Lit . IntLit . selectorHash $ sig
        --let body
        --addInstance
        pure ()

  where
    getFns (CFunDecl fn) = Just fn
    getFns _ = Nothing

    selectorHash s = 0

    abiSig :: Signature Id -> TcM String
    abiSig (Signature _ _ nm params (Just (TyCon "()" []))) = pure $ abiName nm ++ "(" ++ intercalate ","
    abiSig (Signature _ _ nm _ Nothing) = throwError $ "Internal Error: method " ++ show nm ++ " does not have an infered return type"

    abiName (Name n) = n
    abiName (QualName _ n) = n


    ty (Typed _ t) = t
    ty (Untyped (Id _ t)) = t


-- | Generates the `main` function for a contract and adds it to the list of decls
generateMainDecl :: [FunDef Id] -> TcM (ContractDecl Name)
generateMainDecl methods = undefined
