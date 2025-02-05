module Solcore.Frontend.TypeInference.TcEnv where 

import Data.List 
import Data.Map (Map)
import qualified Data.Map as Map

import Solcore.Desugarer.UniqueTypeGen (UniqueTyMap)
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcSubst
import Solcore.Frontend.TypeInference.TcUnify
import Solcore.Pipeline.Options
import Solcore.Primitives.Primitives


-- definition of type environment  
type Arity = Int 

-- type constructor arity and names of constructors 
data TypeInfo 
  = TypeInfo {
      arity :: Arity         -- number of type parameters 
    , constrNames :: [Name]  -- list of data constructor names 
    , fieldNames :: [Name]   -- list of field names 
    } deriving (Eq, Ord, Show)

wordTypeInfo :: TypeInfo 
wordTypeInfo = TypeInfo 0 [] []

unitTypeInfo :: TypeInfo 
unitTypeInfo = TypeInfo 0 [Name "()"] []

pairTypeInfo :: TypeInfo 
pairTypeInfo = TypeInfo 2 [Name "pair"] []

arrowTypeInfo :: TypeInfo
arrowTypeInfo = TypeInfo 2 [] []

-- name of constructor and its scheme
type ConInfo = (Name, Scheme)

-- number of weak parameters and method names
type Method = Name 
data ClassInfo 
  = ClassInfo {
      classArity :: Arity
    , methods :: [Method]
    , classpred :: Pred
    }

type Table a = Map Name a 

-- typing environment 
type Env = Table Scheme
type ClassTable = Table ClassInfo
type TypeTable = Table TypeInfo
type Inst = Qual Pred 
type InstTable = Table [Inst] 

data TcEnv
  = TcEnv {
      ctx :: Env               -- Variable environment
    , instEnv :: InstTable     -- Instance Environment
    , typeTable :: TypeTable   -- Type information environment 
    , classTable :: ClassTable -- Class information table
    , contract :: Maybe Name   -- current contract name 
                               -- used to type check calls.
    , subst :: Subst           -- Current substitution
    , nameSupply :: NameSupply -- Fresh name supply
    , uniqueTypes :: UniqueTyMap -- unique type map
    , directCalls :: [Name] -- defined function names 
    , generateDefs :: Bool     -- should generate new defs?
    , generated :: [TopDecl Id]
    , counter :: Int           -- used to generate new names 
    , logs :: [String]         -- Logging
    , warnings :: [String]     -- warnings collected to user 
    , enableLog :: Bool        -- Enable logging?
    , coverage :: PragmaStatus   -- Disable coverage checking for names. 
    , patterson :: PragmaStatus  -- Disable Patterson condition for names. 
    , boundVariable :: PragmaStatus -- Disable bound variable condition for names.
    , maxRecursionDepth :: Int -- max recursion depth in 
                               -- context reduction
    , tcOptions :: Option
    }

initTcEnv :: Option -> UniqueTyMap -> TcEnv
initTcEnv options utm
  = TcEnv { ctx = primCtx
          , instEnv = primInstEnv
          , typeTable = primTypeEnv
          , classTable = primClassEnv
          , contract = Nothing
          , subst = mempty
          , nameSupply = namePool
          , uniqueTypes = Map.union utm primDataType
          , directCalls = [ Name "primAddWord"
                          , Name "primEqWord"
                          ]
          , generateDefs = True
          , generated = []
          , counter = 0
          , logs = []
          , warnings = []
          , enableLog = True
          , coverage = Enabled
          , patterson = Enabled
          , boundVariable = Enabled
          , maxRecursionDepth = 100
          , tcOptions = options
          }

primCtx :: Env 
primCtx 
  = Map.fromList [ primAddWord
                 , primEqWord
                 , primInvoke
                 , primPair
                 , primUnit 
                 ]

primTypeEnv :: TypeTable 
primTypeEnv = Map.fromList [ (Name "word", wordTypeInfo)
                           , (Name "pair", pairTypeInfo)
                           , (Name "->", arrowTypeInfo)
                           , (Name "()", unitTypeInfo)
                           ]

primInstEnv :: InstTable
primInstEnv = Map.empty 

primClassEnv :: ClassTable 
primClassEnv = Map.empty 
{-  = Map.fromList [(Name "invokable", invokableInfo)]
    where 
      invokableInfo 
        = ClassInfo 2 [QualName (Name "invokable") "invoke"] 
                      (InCls (Name "invokable") self args)
      self = TyVar (TVar (Name "self") False)
      args = map TyVar [TVar (Name "args") False, TVar (Name "ret") False]
-}

primDataType :: Map Name DataTy 
primDataType = Map.fromList [ (Name "primAddWord", dt1 )
                            , (Name "primEqWord", dt2)
                            , (QualName (Name "invokable") "invoke", dt3)
                            ]
    where 
      dt1 = DataTy (Name "t_primAddWord") 
                   [] 
                   [Constr (Name "t_primAddWord") []]
      dt2 = DataTy (Name "t_primEqWord")
                   []
                   [Constr (Name "t_primEqWord") []] 
      dt3 = DataTy (Name "t_invokable.invoke") 
                   []
                   [Constr (Name "t_invokable.invoke") []]


