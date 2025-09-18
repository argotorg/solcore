-- | Module: Solcore.Frontend.TypeInference.DispatchGen
-- | Description: Desugaring for contract level method dispatch
module Solcore.Frontend.TypeInference.DispatchGen
  ( generateSelectorInstances
  , addGeneratedMainDecl
  ) where

import Solcore.Frontend.TypeInference.TcMonad (TcM)
import Solcore.Frontend.Syntax.Contract (Contract)
import Solcore.Frontend.Syntax.Name (Name)

-- | Generates the `Selector` instances for the unique types of each method on a contract
generateSelectorInstances :: Contract Name -> TcM ()
generateSelectorInstances c = pure ()
--
-- | Generates the `main` function for a contract and adds it to the list of decls
addGeneratedMainDecl :: Contract Name -> TcM (Contract Name)
addGeneratedMainDecl c = pure c
