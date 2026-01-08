module Solcore.Frontend.TypeInference.Id where

import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Ty

import Data.Generics (Data, Typeable)

-- identifiers with a type

data Id
  = Id {
      idName :: Name
    , idType :: Ty
    } deriving (Eq, Ord, Show, Data, Typeable)
