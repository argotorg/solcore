module Solcore.Frontend.Syntax.Contract where

import Data.Generics (Data, Typeable)
import Data.List.NonEmpty

import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty

-- compilation unit

data CompUnit a
  = CompUnit {
      imports :: [Import]
    , contracts :: [TopDecl a]
    } deriving (Eq, Ord, Show, Data, Typeable)
type NamedCompUnit = CompUnit Name

data TopDecl a
  = TContr (Contract a)
  | TFunDef (FunDef a)
  | TClassDef (Class a)
  | TInstDef (Instance a)
  | TMutualDef [TopDecl a]
  | TDataDef DataTy
  | TSym TySym
  | TPragmaDecl Pragma
  deriving (Eq, Ord, Show, Data, Typeable)
type NamedTopDecl = TopDecl Name

-- empty list in pragma: restriction on all class / instances

data PragmaType
  = NoCoverageCondition
  | NoPattersonCondition
  | NoBoundVariableCondition
  deriving (Eq, Ord, Show, Data, Typeable)

data PragmaStatus
  = Enabled
  | DisableAll
  | DisableFor (NonEmpty Name)
  deriving (Eq, Ord, Show, Data, Typeable)

data Pragma
  = Pragma {
      pragmaType :: PragmaType
    , pragmaStatus :: PragmaStatus
    } deriving (Eq, Ord, Show, Data, Typeable)

newtype Import
  = Import { unImport :: Name }
    deriving (Eq, Ord, Show, Data, Typeable)

-- definition of the contract structure

data Contract a
  = Contract {
      name :: Name
    , tyParams :: [Tyvar]
    , decls :: [ContractDecl a]
    } deriving (Eq, Ord, Show, Data, Typeable)
type NamedContract = Contract Name

-- definition of a algebraic data type

data DataTy
  = DataTy {
      dataName :: Name
    , dataParams :: [Tyvar]
    , dataConstrs :: [Constr]
    } deriving (Eq, Ord, Show, Data, Typeable)

data Constr
  = Constr {
      constrName :: Name
    , constrTy :: [Ty]
    } deriving (Eq, Ord, Show, Data, Typeable)

-- definition of type synonym

data TySym
  = TySym {
      symName :: Name
    , symVars :: [Tyvar]
    , symType :: Ty
    } deriving (Eq, Ord, Show, Data, Typeable)

-- definition of contract constructor

data Constructor a
  = Constructor {
      constrParams :: [Param a]
    , constrBody :: (Body a)
    } deriving (Eq, Ord, Show, Data, Typeable)
type NamedConstructor = Constructor Name

-- definition of classes and instances

data Class a
  = Class {
      classboundvars :: [Tyvar]
    , classContext :: [Pred]
    , className :: Name
    , paramsVar :: [Tyvar]
    , mainVar :: Tyvar
    , signatures :: [Signature a]
    } deriving (Eq, Ord, Show, Data, Typeable)
type NamedClass = Class Name

data Signature a
  = Signature {
      sigVars :: [Tyvar]
    , sigContext :: [Pred]
    , sigName :: Name
    , sigParams :: [Param a]
    , sigReturn :: Maybe Ty
    } deriving (Eq, Ord, Show, Data, Typeable)
type NamedSignature = Signature Name


data Instance a
  = Instance {
      instDefault :: Bool
    , instVars :: [Tyvar]
    , instContext :: [Pred]
    , instName :: Name
    , paramsTy :: [Ty]
    , mainTy :: Ty
    , instFunctions :: [FunDef a]
    } deriving (Eq, Ord, Show, Data, Typeable)
type NamedInstance = Instance Name

-- definition of contract field variables

data Field a
  = Field {
      fieldName :: Name
    , fieldTy :: Ty
    , fieldInit :: Maybe (Exp a)
    } deriving (Eq, Ord, Show, Data, Typeable)
type NamedField = Field Name

-- definition of functions

data FunDef a
  = FunDef {
      funSignature :: Signature a
    , funDefBody :: Body a
    } deriving (Eq, Ord, Show, Data, Typeable)
type NamedFunDef = FunDef Name

data ContractDecl a
  = CDataDecl DataTy
  | CFieldDecl (Field a)
  | CFunDecl (FunDef a)
  | CMutualDecl [ContractDecl a] -- used only after SCC analysis
  | CConstrDecl (Constructor a)
    deriving (Eq, Ord,Show, Data, Typeable)
type NamedContractDecl = ContractDecl Name
