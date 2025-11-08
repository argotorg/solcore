{-# LANGUAGE GHC2021, DataKinds, GADTs, StandaloneDeriving, ConstraintKinds, UndecidableInstances #-}
module Solcore.Frontend.Syntax.StmtX where
import GHC.Types
import Data.Generics (Data, Typeable)

import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.Syntax.Name
import Language.Yul

data Pass = Parsed | Named | Typechecked deriving (Eq, Ord, Show, Data, Typeable)
data ComPass (c::Pass) where
    ComNm :: ComNm
    ComPs :: ComPs
    ComTc :: ComTc

type ComNm = ComPass 'Named
type ComPs = ComPass 'Parsed
type ComTc = ComPass 'Typechecked

data NoExtField = NoExtField deriving (Eq, Ord, Show, Data, Typeable)
instance Semigroup NoExtField where _ <> _ = NoExtField
instance Monoid NoExtField where mempty = NoExtField

data DataConCantHappen -- empty data type
-- void :: DataConCantHappen
-- void = error "Attempt to evaluate void"

absurd :: DataConCantHappen -> a
absurd v = case v of {}

-- definition of statements

type EquationX x = ([Pat x], [StmtX x])
type EquationsX x = [EquationX x]

type NamedExp = ExpX ComNm
type NamedStmt = StmtX ComNm

pattern (:=) :: NamedExp -> NamedExp -> NamedStmt
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
  | XStmtX (XXStmt x)                             -- extensions

type ForallStmtX (p:: Type -> Constraint) x =
    ( p(XAss x)
    , p(XLet x)
    , p(XStmtExp x)
    , p(XReturn x)
    , p(XMatch x)
    , p(XAsm x)
    , p(XIf x)
    , p(XXStmt x)
    , p(XVar   x)
    , p(XCon   x)   
    , p(XFA    x)
    , p(XLit   x)
    , p(XCall  x)
    , p(XLam   x)
    , p(XTyExp x)
    , p(XCond  x)
    , p(XTyped x)
    , p(XUntyped x)
  )
-- deriving instance (Eq, Ord, Show, Data, Typeable)

deriving instance ForallStmtX Eq x => Eq (StmtX x)
deriving instance ForallStmtX Ord x => Ord (StmtX x)
deriving instance ForallStmtX Show x => Show (StmtX x)
         
type family XAss x
type family XLet x
type family XStmtExp x
type family XReturn x
type family XMatch x
type family XAsm x
type family XIf x
type family XXStmt x

type instance XAss ComNm = NoExtField
type instance XLet ComNm = NoExtField
type instance XStmtExp ComNm = NoExtField
type instance XReturn ComNm = NoExtField
type instance XMatch ComNm = NoExtField
type instance XAsm ComNm = NoExtField
type instance XIf ComNm = NoExtField
type instance XXStmt ComNm = NoExtField


type BodyX x = [StmtX x]

data ParamX x
  = TypedX (XTyped x) Name Ty
  | UntypedX (XUntyped x)  Name
--  deriving (Eq, Ord, Show, Data, Typeable)

type family XTyped x
type family XUntyped x

type instance XTyped ComNm = NoExtField
type instance XUntyped ComNm = NoExtField

type ForallParamX (p:: Type -> Constraint) x =
    ( p(XTyped x), p(XUntyped x) )

deriving instance ForallStmtX Eq x => Eq (ParamX x)
deriving instance ForallStmtX Ord x => Ord (ParamX x)
deriving instance ForallStmtX Show x => Show (ParamX x)

paramName :: ParamX x -> Name
paramName (TypedX _ n _) = n
paramName (UntypedX _ n) = n

-- definition of the expression syntax

data ExpX x
  = VarX (XVar x) Name                 -- variable
  | ConX (XCon x) Name  [ExpX x]       -- data type constructor
  | FieldAccessX (XFA x) (Maybe (ExpX x)) Name      -- field access
  | LitX (XLit x) Literal                        -- literal
  | CallX (XCall x) (Maybe (ExpX x)) Name [ExpX x]     -- function calal
  | LamX (XLam x)[ParamX x] (BodyX x) (Maybe Ty)  -- lambda-abstraction
  | TyExpX (XTyExp x)(ExpX x) Ty                   -- type annotated expression
  | CondX (XCond x) (ExpX x) (ExpX x) (ExpX x)       -- conditional expression
--   deriving (Eq, Ord, Show, Data, Typeable)

type family XVar x
type family XCon x
type family XFA x
type family XLit x
type family XCall x
type family XLam x
type family XTyExp x
type family XCond x

deriving instance ForallStmtX Eq x => Eq (ExpX x)
deriving instance ForallStmtX Ord x => Ord (ExpX x)
deriving instance ForallStmtX Show x => Show (ExpX x)

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
