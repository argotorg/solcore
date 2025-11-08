module Solcore.Frontend.Syntax.StmtX where

import Data.Generics (Data, Typeable)

import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.Syntax.Name
import Language.Yul

-- definition of statements

type EquationX x = ([Pat x], [StmtX x])
type EquationsX x = [EquationX x]

type NamedExp = ExpX 'Named
pattern (:=) :: NamedExp -> NamedExp -> NamedExp
pattern (:=) e1 e2 <- AssX _ e1 e2
  where e1 := e2 = AssX mempty e1 e2

data StmtX x
  = AssX (XAss x) (ExpX x) (ExpX x)                  -- assignment
  | LetX (XLet x) Name (Maybe Ty) (Maybe (ExpX x))    -- local variable
  | StmtExp (XStmtExp x) (ExpX x)                     -- expression level statements
  | ReturnX (XReturn x) (ExpX x)                      -- return statements
  | MatchX (XMatch x) [ExpX x] (EquationsX x)         -- pattern matching
  | AsmX (XAsm x) YulBlock                        -- Yul block (type annotations ???)
  | IfX (XIf x)(ExpX x) (BodyX x) (BodyX x)        -- If statement
  deriving (Eq, Ord, Show, Data, Typeable)

type family XAss x
type family XLet x
type family XStmtExp x
type family XReturn x
type family XMatch x
type family XAsm x
type family XIf x

type BodyX x = [StmtX x]

data ParamX x
  = TypedX (XTyped x) Name Ty
  | UntypedX (XUntyped x)  Name
  deriving (Eq, Ord, Show, Data, Typeable)


paramName :: Param a -> a
paramName (TypedX _ n _) = n
paramName (UntypedX _ n) = n

-- definition of the expression syntax

data ExpX x
  = VarX (XVar x) Name                 -- variable
  | ConX (XCon x) Name  [ExpX x]       -- data type constructor
  | FieldAccessX (XFA x) (Maybe (ExpX x)) Name      -- field access
  | LitX (XLit x) Literal                        -- literal
  | CallX (XCall x) (Maybe (ExpX x)) Name [ExpX x]     -- function calal
  | LamX (XLam x)[Param Name] (Body Name) (Maybe Ty)  -- lambda-abstraction
  | TyExpX (XTyExp x)(ExpX x) Ty                   -- type annotated expression
  | CondX (XCond x) (ExpX x) (ExpX x) (ExpX x)       -- conditional expression
  deriving (Eq, Ord, Show, Data, Typeable)

type family XVar x
type family XCon x
type family XFA x
type family XLit x
type family XCall x
type family XLam x
type family XTyExp x
type family XCond x



-- pattern matching equations

data Pat x
  = PVar Name
  | PCon Name [Pat Name]
  | PWildcard
  | PLit Literal
  deriving (Eq, Ord, Show, Data, Typeable)

-- definition of literals

data Literal
  = IntLit Integer
  | StrLit String
  deriving (Eq, Ord, Show, Data, Typeable)
