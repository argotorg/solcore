module Solcore.Frontend.Syntax.Pragma where

import Data.Generics
import Data.List.NonEmpty

import Solcore.Frontend.Syntax.Name

-- empty list in pragma: restriction on all class / instances

data PragmaType
  = NoCoverageCondition
  | NoPattersonCondition
  | NoBoundVariableCondition
  deriving (Eq, Ord, Show, Data, Typeable)

-- status for type inference pragmas

data PragmaStatus
  = Enabled
  | DisableAll
  | DisableFor (NonEmpty Name)
  deriving (Eq, Ord, Show, Data, Typeable)

-- Pragma type definition

data Pragma
  = InferencePragma
      { pragmaType :: PragmaType
      , pragmaStatus :: PragmaStatus
      }
  | BoolTypePragma (NonEmpty Name)
  deriving (Eq, Ord, Show, Data, Typeable)
