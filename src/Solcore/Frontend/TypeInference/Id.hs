module Solcore.Frontend.TypeInference.Id where 

import Data.Generics (Data, Typeable)

import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.TcSubst 

-- identifiers with a type 

data Id 
  = Id {
      idName :: Name 
    , idType :: Ty 
    } deriving (Eq, Ord, Show, Data, Typeable)

instance HasType Id where 
  apply s (Id n t) = Id n (apply s t)
  fv (Id _ t) = fv t


