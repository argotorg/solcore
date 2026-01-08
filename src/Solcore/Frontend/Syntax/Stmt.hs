module Solcore.Frontend.Syntax.Stmt where

import Data.Generics (Data, Typeable)

import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Ty
import Language.Yul

-- definition of statements

type Equation a = ([Pat a], [Stmt a])
type Equations a = [Equation a]

type NamedEquation = Equation Name
type NamedEquations = Equations Name

data Stmt a
  = (Exp a) := (Exp a)                  -- assignment
  | Let a (Maybe Ty) (Maybe (Exp a))    -- local variable
  | StmtExp (Exp a)                     -- expression level statements
  | Return (Exp a)                      -- return statements
  | Match [Exp a] (Equations a)         -- pattern matching
  | Asm YulBlock                        -- Yul block
  | If (Exp a) (Body a) (Body a)        -- If statement
  deriving (Eq, Ord, Show, Data, Typeable)
type NamedStmt = Stmt Name

type Body a = [Stmt a]
type NamedBody = Body Name

data Param a
  = Typed a Ty
  | Untyped a
  deriving (Eq, Ord, Show, Data, Typeable)
type NamedParam = Param Name

paramName :: Param a -> a
paramName (Typed n _) = n
paramName (Untyped n) = n

-- definition of the expression syntax

data Exp a
  = Var a                              -- variable
  | Con a [Exp a]                      -- data type constructor
  | FieldAccess (Maybe (Exp a)) a      -- field access
  | Lit Literal                        -- literal
  | Call (Maybe (Exp a)) a [Exp a]     -- function call
  | Lam [Param a] (Body a) (Maybe Ty)  -- lambda-abstraction
  | TyExp (Exp a) Ty                   -- type annotated expression
  | Cond (Exp a) (Exp a) (Exp a)       -- conditional expression
  | Indexed (Exp a) (Exp a)            -- e1[e2]
  deriving (Eq, Ord, Show, Data, Typeable)
type NamedExp = Exp Name

-- pattern matching equations

data Pat a
  = PVar a
  | PCon a [Pat a]
  | PWildcard
  | PLit Literal
  deriving (Eq, Ord, Show, Data, Typeable)
type NamedPat = Pat Name
-- definition of literals

data Literal
  = IntLit Integer
  | StrLit String
  deriving (Eq, Ord, Show, Data, Typeable)
