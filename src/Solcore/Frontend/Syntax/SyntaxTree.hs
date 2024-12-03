module Solcore.Frontend.Syntax.SyntaxTree where

import Data.Generics (Data, Typeable)
import Data.List (union)
import Data.List.NonEmpty
import Language.Yul
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Pragma

-- compilation unit

data CompUnit
  = CompUnit
  { imports :: [Import]
  , contracts :: [TopDecl]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data TopDecl
  = TContr Contract
  | TFunDef FunDef
  | TClassDef Class
  | TInstDef Instance
  | TDataDef DataTy
  | TSym TySym
  | TPragmaDecl Pragma
  deriving (Eq, Ord, Show, Data, Typeable)

newtype Import
  = Import {unImport :: Name}
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of the contract structure

data Contract
  = Contract
  { name :: Name
  , tyParams :: [Ty]
  , decls :: [ContractDecl]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of a algebraic data type

data DataTy
  = DataTy
  { dataName :: Name
  , dataParams :: [Ty]
  , dataConstrs :: [Constr]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data Constr
  = Constr
  { constrName :: Name
  , constrTy :: [Ty]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- type definition

data Ty
  = TyCon Name [Ty] -- type constructor
  deriving (Eq, Ord, Show, Data, Typeable)

pattern (:->) t1 t2 = TyCon (Name "->") [t1, t2]

tyName :: Ty -> Name
tyName (TyCon n _) = n

data Pred = InCls
  { predName :: Name
  , predMain :: Ty
  , predParams :: [Ty]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

tysFrom :: [Pred] -> [Ty]
tysFrom = foldr go []
 where
  go p ac = (predMain p) : predParams p `union` ac

-- definition of type synonym

data TySym
  = TySym
  { symName :: Name
  , symVars :: [Ty]
  , symType :: Ty
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of contract constructor

data Constructor
  = Constructor
  { constrParams :: [Param]
  , constrBody :: Body
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of classes and instances

data Class
  = Class
  { classContext :: [Pred]
  , className :: Name
  , paramsVar :: [Ty]
  , mainVar :: Ty
  , signatures :: [Signature]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data Signature
  = Signature
  { sigVars :: [Ty]
  , sigContext :: [Pred]
  , sigName :: Name
  , sigParams :: [Param]
  , sigReturn :: Maybe Ty
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data Instance
  = Instance
  { instContext :: [Pred]
  , instName :: Name
  , paramsTy :: [Ty]
  , mainTy :: Ty
  , instFunctions :: [FunDef]
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of contract field variables

data Field
  = Field
  { fieldName :: Name
  , fieldTy :: Ty
  , fieldInit :: Maybe Exp
  }
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of functions

data FunDef
  = FunDef
  { funSignature :: Signature
  , funDefBody :: Body
  }
  deriving (Eq, Ord, Show, Data, Typeable)

data ContractDecl
  = CDataDecl DataTy
  | CFieldDecl Field
  | CFunDecl FunDef
  | CConstrDecl Constructor
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of statements

type Equation = ([Pat], [Stmt])
type Equations = [Equation]

data Stmt
  = Assign Exp Exp -- assignment
  | Let Name (Maybe Ty) (Maybe Exp) -- local variable
  | StmtExp Exp -- expression level statements
  | Return Exp -- return statements
  | Match [Exp] Equations -- pattern matching
  | Asm YulBlock -- Yul block
  deriving (Eq, Ord, Show, Data, Typeable)

type Body = [Stmt]

data Param
  = Typed Name Ty
  | Untyped Name
  deriving (Eq, Ord, Show, Data, Typeable)

-- expression syntax

data Exp
  = Lit Literal -- literal
  | ExpName (Maybe Exp) Name [Exp] -- function call or constructor
  | ExpVar (Maybe Exp) Name -- variables or field access
  | Lam [Param] Body (Maybe Ty) -- lambda-abstraction
  | TyExp Exp Ty -- type annotation expression
  deriving (Eq, Ord, Show, Data, Typeable)

-- pattern matching equations

data Pat
  = Pat Name [Pat]
  | PWildcard
  | PLit Literal
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of literals

data Literal
  = IntLit Integer
  | StrLit String
  deriving (Eq, Ord, Show, Data, Typeable)

pairTy :: Ty -> Ty -> Ty
pairTy t1 t2 = TyCon "pair" [t1, t2]

funtype :: [Ty] -> Ty -> Ty
funtype ts t = foldr (:->) t ts
