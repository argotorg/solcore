{-# LANGUAGE GHC2021, DataKinds, GADTs, StandaloneDeriving, ConstraintKinds, UndecidableInstances, DeriveDataTypeable #-}
module Solcore.Frontend.Syntax.StmtX where
import GHC.Types
import Data.Generics (Data, Typeable)

import Common.Pretty
import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.TypeInference.Id -- todo: move to Syntax
import Language.Yul

data Pass = Parsed | Named | Typechecked | Specialised deriving (Eq, Ord, Show, Data, Typeable)
data ComPass (c::Pass) where
    ComNm :: ComNm
    ComPs :: ComPs
    ComTc :: ComTc

type ComPs = ComPass 'Parsed
type ComNm = ComPass 'Named
type ComSp = ComPass 'Specialised
type ComTc = ComPass 'Typechecked

data NoExtField = NoExtField deriving (Eq, Ord, Show, Data, Typeable)
instance Semigroup NoExtField where _ <> _ = NoExtField
instance Monoid NoExtField where mempty = NoExtField

data DataConCantHappen -- empty data type
-- void :: DataConCantHappen
-- void = error "Attempt to evaluate void"

absurd :: DataConCantHappen -> a
absurd v = case v of {}

-- names in differeent passes: Name/Id etc
type family XName x
type instance XName ComNm = Name
type instance XName ComTc = Id
type instance XName ComSp = Id

-- definition of statements

type EquationX x = ([PatX x], [StmtX x])
type EquationsX x = [EquationX x]

type NamedExp = ExpX ComNm
type NamedStmt = StmtX ComNm
type NamedBody = BodyX ComNm
type NamedPat = PatX ComNm
type NamedParam = ParamX ComNm

pattern (:=) :: NamedExp -> NamedExp -> NamedStmt
pattern (:=) e1 e2 <- AssX _ e1 e2
  where e1 := e2 = AssX mempty e1 e2

data StmtX x
  = AssX (XAss x) (ExpX x) (ExpX x)                        -- assignment
  | LetX (XLet x) (XName x) (Maybe Ty) (Maybe (ExpX x))    -- local variable
  | StmtExpX (XStmtExp x) (ExpX x)                         -- expression level statements
  | ReturnX (XReturn x) (ExpX x)                           -- return statements
  | MatchX (XMatch x) [ExpX x] (EquationsX x)              -- pattern matching
  | AsmX (XAsm x) YulBlock                                 -- Yul block (no type annotations)
  | IfX (XIf x)(ExpX x) (BodyX x) (BodyX x)                -- If statement
  | XStmtX (XXStmt x)                                      -- extensions

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
    , p(XIndexed x)
    , p(XTyped x)
    , p(XUntyped x)
    , p(XName x)
  )

deriving instance (ForallStmtX Eq x, Eq (XName x)) => Eq (StmtX x)
deriving instance ForallStmtX Ord x => Ord (StmtX x)
deriving instance ForallStmtX Show x => Show (StmtX x)
deriving instance (ForallStmtX Data x, Typeable x, Data x) => Data (StmtX x)
deriving instance ForallStmtX Typeable x => Typeable (StmtX x)

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

type instance XVar ComNm = NoExtField
type instance XCon ComNm = NoExtField
type instance XFA ComNm = NoExtField
type instance XLit ComNm = NoExtField
type instance XCall ComNm = NoExtField
type instance XLam ComNm = NoExtField
type instance XTyExp ComNm = NoExtField
type instance XCond ComNm = NoExtField
type instance XIndexed ComNm = NoExtField

type BodyX x = [StmtX x]

data ParamX x
  = TypedX (XTyped x) (XName x) Ty
  | UntypedX (XUntyped x) (XName x)

type family XTyped x
type family XUntyped x

type instance XTyped ComNm = NoExtField
type instance XUntyped ComNm = NoExtField

type ForallParamX (p:: Type -> Constraint) x =
    ( p(XTyped x), p(XUntyped x) )

deriving instance ForallStmtX Eq x => Eq (ParamX x)
deriving instance ForallStmtX Ord x => Ord (ParamX x)
deriving instance ForallStmtX Show x => Show (ParamX x)
deriving instance (ForallStmtX Data x, Typeable x, Data x) => Data (ParamX x)

paramName :: ParamX x -> XName x
paramName (TypedX _ n _) = n
paramName (UntypedX _ n) = n

-- definition of the expression syntax

data ExpX x
  = VarX (XVar x) (XName x)                              -- variable
  | ConX (XCon x) (XName x)  [ExpX x]                    -- data type constructor
  | FieldAccessX (XFA x) (Maybe (ExpX x)) (XName x)      -- field access
  | LitX (XLit x) Literal                                -- literal
  | CallX (XCall x) (Maybe (ExpX x)) (XName x) [ExpX x]  -- function call
  | LamX (XLam x)[ParamX x] (BodyX x) (Maybe Ty)         -- lambda-abstraction
  | TyExpX (XTyExp x)(ExpX x) Ty                         -- type annotated expression
  | CondX (XCond x) (ExpX x) (ExpX x) (ExpX x)           -- conditional expression
  | IndexedX (XIndexed x) (ExpX x) (ExpX x)              -- e1[e2]
--   deriving (Eq, Ord, Show, Data, Typeable)

type family XVar x
type family XCon x
type family XFA x
type family XLit x
type family XCall x
type family XLam x
type family XTyExp x
type family XCond x
type family XIndexed x

deriving instance (ForallStmtX Eq x) => Eq (ExpX x)
deriving instance (ForallStmtX Ord x) => Ord (ExpX x)
deriving instance (ForallStmtX Show x) => Show (ExpX x)
deriving instance (ForallStmtX Data x, Typeable x, Data x) => Data (ExpX  x)

-- pattern matching equations

data PatX x
  = PVar (XName x)
  | PCon (XName x) [PatX x]
  | PWildcard
  | PLit Literal
--  deriving (Eq, Ord, Show, Data, Typeable)

deriving instance Eq (XName x) => Eq (PatX x)
deriving instance Ord (XName x) => Ord (PatX x)
deriving instance Show (XName x) => Show (PatX x)
deriving instance (Data (XName x), Typeable x, Data x) => Data (PatX x)

-- ============================================================================
-- Pattern synonyms for migration ease
-- ============================================================================

-- StmtX patterns
pattern Ass :: ExpX ComNm -> ExpX ComNm -> StmtX ComNm
pattern Ass e1 e2 = AssX NoExtField e1 e2

pattern Let :: XName ComNm -> Maybe Ty -> Maybe (ExpX ComNm) -> StmtX ComNm
pattern Let n t e = LetX NoExtField n t e

pattern StmtExp :: ExpX ComNm -> StmtX ComNm
pattern StmtExp e = StmtExpX NoExtField e

pattern Return :: ExpX ComNm -> StmtX ComNm
pattern Return e = ReturnX NoExtField e

pattern Match :: [ExpX ComNm] -> EquationsX ComNm -> StmtX ComNm
pattern Match es eqs = MatchX NoExtField es eqs

pattern Asm :: YulBlock -> StmtX ComNm
pattern Asm blk = AsmX NoExtField blk

pattern If :: ExpX ComNm -> BodyX ComNm -> BodyX ComNm -> StmtX ComNm
pattern If c t e = IfX NoExtField c t e

{-# COMPLETE Ass, Let, StmtExp, Return, Match, Asm, If, XStmtX #-}

-- ExpX patterns
pattern Var :: XName ComNm -> ExpX ComNm
pattern Var n = VarX NoExtField n

pattern Con :: XName ComNm -> [ExpX ComNm] -> ExpX ComNm
pattern Con n es = ConX NoExtField n es

pattern FieldAccess :: Maybe (ExpX ComNm) -> XName ComNm -> ExpX ComNm
pattern FieldAccess e n = FieldAccessX NoExtField e n

pattern Lit :: Literal -> ExpX ComNm
pattern Lit l = LitX NoExtField l

pattern Call :: Maybe (ExpX ComNm) -> XName ComNm -> [ExpX ComNm] -> ExpX ComNm
pattern Call e n as = CallX NoExtField e n as

pattern Lam :: [ParamX ComNm] -> BodyX ComNm -> Maybe Ty -> ExpX ComNm
pattern Lam ps body ty = LamX NoExtField ps body ty

pattern TyExp :: ExpX ComNm -> Ty -> ExpX ComNm
pattern TyExp e ty = TyExpX NoExtField e ty

pattern Cond :: ExpX ComNm -> ExpX ComNm -> ExpX ComNm -> ExpX ComNm
pattern Cond c t e = CondX NoExtField c t e

pattern Indexed :: ExpX ComNm -> ExpX ComNm -> ExpX ComNm
pattern Indexed e1 e2 <- IndexedX NoExtField e1 e2
  where Indexed e1 e2 =  IndexedX NoExtField e1 e2

{-# COMPLETE Var, Con, FieldAccess, Lit, Call, Lam, TyExp, Cond #-}

-- ParamX patterns
pattern Typed :: XName ComNm -> Ty -> ParamX ComNm
pattern Typed n t = TypedX NoExtField n t

pattern Untyped :: XName ComNm -> ParamX ComNm
pattern Untyped n = UntypedX NoExtField n

{-# COMPLETE Typed, Untyped #-}

-- definition of literals

data Literal
  = IntLit Integer
  | StrLit String
  deriving (Eq, Ord, Show, Data, Typeable)

instance Pretty (PatX ComNm) where
  ppr (PVar n)
    = ppr n
  ppr (PCon n []) = ppr n
  ppr (PCon n ps@(_ : _))
    | n == "pair" = parens (commaSep $ map ppr ps)
    | otherwise = ppr n <> (parens $ commaSep $ map ppr ps )
  ppr PWildcard
    = text "_"
  ppr (PLit l)
    = ppr l

instance Pretty Literal where
  ppr (IntLit l) = integer (toInteger l)
  ppr (StrLit l) = quotes (text l)
