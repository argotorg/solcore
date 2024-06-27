module Solcore.Frontend.TypeInference.TcEnv where 

import Data.List 
import Data.Map (Map)
import qualified Data.Map as Map

import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Primitives.Primitives


-- definition of type environment  

type Env = Map Name Scheme
type DataEnv = Map Name Scheme
type TypeEnv = Map Name TypeInfo 
type FieldEnv = Map Name Scheme 
type FunctionEnv = Map Name Scheme 
type Inst = Qual Pred 
type InstEnv = Map Name [Inst] 

data TypeInfo 
  = TypeInfo {
      fieldEnv :: FieldEnv   
    , funEnv :: FunctionEnv
    }
    deriving (Eq, Show, Ord)

data TcEnv 
  = TcEnv {
      ctx :: Env               -- Variable environment
    , constructors :: DataEnv  -- ADT constructor environment
    , typeEnv :: TypeEnv       -- Type environment
    , instEnv :: InstEnv       -- Instance Environment
    , contract :: Name         -- current contract name 
                               -- used to type check calls.
    , returnType :: Ty         -- current function return type.
    , subst :: Subst           -- Current substitution
    , nameSupply :: NameSupply -- Fresh name supply
    , logs :: [String]         -- Logging
    , enableLog :: Bool        -- Enable logging?
    , enableCoverage :: Bool   -- Enable coverage checking?
    , maxRecursionDepth :: Int -- max recursion depth in 
                               -- context reduction
    }


