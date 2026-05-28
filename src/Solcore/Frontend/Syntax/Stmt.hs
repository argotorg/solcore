{-# LANGUAGE PatternSynonyms #-}

module Solcore.Frontend.Syntax.Stmt where

import Data.Generics (Data, Typeable)
import Language.Yul
import Solcore.Diagnostics (SourceSpan)
import Solcore.Frontend.Syntax.Location
import Solcore.Frontend.Syntax.Ty
import Prelude hiding (exp)

-- definition of statements

type Equation a = ([Pat a], [Stmt a])

type Equations a = [Equation a]

data Stmt a
  = AssignWithLocation NodeLocation (Exp a) (Exp a) -- assignment
  | LetWithLocation NodeLocation a (Maybe Ty) (Maybe (Exp a)) -- local variable
  | BlockWithLocation NodeLocation (Body a) -- lexical block
  | StmtExpWithLocation NodeLocation (Exp a) -- expression level statements
  | ReturnWithLocation NodeLocation (Exp a) -- return statements
  | MatchWithLocation NodeLocation [Exp a] (Equations a) -- pattern matching
  | AsmWithLocation NodeLocation YulBlock -- Yul block
  | IfWithLocation NodeLocation (Exp a) (Body a) (Body a) -- If statement
  | ForWithLocation NodeLocation (Stmt a) (Exp a) (Stmt a) (Body a) -- for(init; cond; post) { body }
  deriving (Eq, Ord, Show, Data, Typeable)

infix 4 :=

pattern (:=) :: Exp a -> Exp a -> Stmt a
pattern lhs := rhs <- AssignWithLocation _ lhs rhs
  where
    lhs := rhs = AssignWithLocation unlocatedNode lhs rhs

pattern Let :: a -> Maybe Ty -> Maybe (Exp a) -> Stmt a
pattern Let n ty value <- LetWithLocation _ n ty value
  where
    Let n ty value = LetWithLocation unlocatedNode n ty value

pattern Block :: Body a -> Stmt a
pattern Block body <- BlockWithLocation _ body
  where
    Block body = BlockWithLocation unlocatedNode body

pattern StmtExp :: Exp a -> Stmt a
pattern StmtExp exp <- StmtExpWithLocation _ exp
  where
    StmtExp exp = StmtExpWithLocation unlocatedNode exp

pattern Return :: Exp a -> Stmt a
pattern Return exp <- ReturnWithLocation _ exp
  where
    Return exp = ReturnWithLocation unlocatedNode exp

pattern Match :: [Exp a] -> Equations a -> Stmt a
pattern Match exps equations <- MatchWithLocation _ exps equations
  where
    Match exps equations = MatchWithLocation unlocatedNode exps equations

pattern Asm :: YulBlock -> Stmt a
pattern Asm block <- AsmWithLocation _ block
  where
    Asm block = AsmWithLocation unlocatedNode block

pattern If :: Exp a -> Body a -> Body a -> Stmt a
pattern If cond thenBody elseBody <- IfWithLocation _ cond thenBody elseBody
  where
    If cond thenBody elseBody = IfWithLocation unlocatedNode cond thenBody elseBody

pattern For :: Stmt a -> Exp a -> Stmt a -> Body a -> Stmt a
pattern For initStmt cond postStmt body <- ForWithLocation _ initStmt cond postStmt body
  where
    For initStmt cond postStmt body = ForWithLocation unlocatedNode initStmt cond postStmt body

{-# COMPLETE (:=), Let, Block, StmtExp, Return, Match, Asm, If, For #-}

type Body a = [Stmt a]

locatedStmt :: SourceSpan -> Stmt a -> Stmt a
locatedStmt sourceSpan (lhs := rhs) = AssignWithLocation (locatedNode sourceSpan) lhs rhs
locatedStmt sourceSpan (Let n ty value) = LetWithLocation (locatedNode sourceSpan) n ty value
locatedStmt sourceSpan (Block body) = BlockWithLocation (locatedNode sourceSpan) body
locatedStmt sourceSpan (StmtExp exp) = StmtExpWithLocation (locatedNode sourceSpan) exp
locatedStmt sourceSpan (Return exp) = ReturnWithLocation (locatedNode sourceSpan) exp
locatedStmt sourceSpan (Match exps equations) = MatchWithLocation (locatedNode sourceSpan) exps equations
locatedStmt sourceSpan (Asm block) = AsmWithLocation (locatedNode sourceSpan) block
locatedStmt sourceSpan (If cond thenBody elseBody) = IfWithLocation (locatedNode sourceSpan) cond thenBody elseBody
locatedStmt sourceSpan (For initStmt cond postStmt body) = ForWithLocation (locatedNode sourceSpan) initStmt cond postStmt body

instance (HasSourceSpan a) => HasSourceSpan (Stmt a) where
  sourceSpanOf (AssignWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]
  sourceSpanOf (LetWithLocation location n ty value) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf n, sourceSpanOf ty, sourceSpanOf value]
  sourceSpanOf (BlockWithLocation location body) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf body]
  sourceSpanOf (StmtExpWithLocation location exp) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf exp]
  sourceSpanOf (ReturnWithLocation location exp) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf exp]
  sourceSpanOf (MatchWithLocation location exps equations) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf exps, sourceSpanOf equations]
  sourceSpanOf (AsmWithLocation location _) =
    sourceSpanOf location
  sourceSpanOf (IfWithLocation location cond thenBody elseBody) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf cond, sourceSpanOf thenBody, sourceSpanOf elseBody]
  sourceSpanOf (ForWithLocation location initStmt cond postStmt body) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf initStmt, sourceSpanOf cond, sourceSpanOf postStmt, sourceSpanOf body]

data Param a
  = Typed a Ty
  | Untyped a
  deriving (Eq, Ord, Show, Data, Typeable)

paramName :: Param a -> a
paramName (Typed n _) = n
paramName (Untyped n) = n

instance (HasSourceSpan a) => HasSourceSpan (Param a) where
  sourceSpanOf (Typed n ty) =
    firstSourceSpan [sourceSpanOf n, sourceSpanOf ty]
  sourceSpanOf (Untyped n) =
    sourceSpanOf n

-- definition of the expression syntax

data Exp a
  = VarWithLocation NodeLocation a -- variable
  | ConWithLocation NodeLocation a [Exp a] -- data type constructor
  | FieldAccessWithLocation NodeLocation (Maybe (Exp a)) a -- field access
  | LitWithLocation NodeLocation Literal -- literal
  | CallWithLocation NodeLocation (Maybe (Exp a)) a [Exp a] -- function call
  | LamWithLocation NodeLocation [Param a] (Body a) (Maybe Ty) -- lambda-abstraction
  | TyExpWithLocation NodeLocation (Exp a) Ty -- type annotated expression
  | CondWithLocation NodeLocation (Exp a) (Exp a) (Exp a) -- conditional expression
  | IndexedWithLocation NodeLocation (Exp a) (Exp a) -- e1[e2]
  deriving (Eq, Ord, Show, Data, Typeable)

pattern Var :: a -> Exp a
pattern Var n <- VarWithLocation _ n
  where
    Var n = VarWithLocation unlocatedNode n

pattern Con :: a -> [Exp a] -> Exp a
pattern Con n es <- ConWithLocation _ n es
  where
    Con n es = ConWithLocation unlocatedNode n es

pattern FieldAccess :: Maybe (Exp a) -> a -> Exp a
pattern FieldAccess me n <- FieldAccessWithLocation _ me n
  where
    FieldAccess me n = FieldAccessWithLocation unlocatedNode me n

pattern Lit :: Literal -> Exp a
pattern Lit lit <- LitWithLocation _ lit
  where
    Lit lit = LitWithLocation unlocatedNode lit

pattern Call :: Maybe (Exp a) -> a -> [Exp a] -> Exp a
pattern Call me n es <- CallWithLocation _ me n es
  where
    Call me n es = CallWithLocation unlocatedNode me n es

pattern Lam :: [Param a] -> Body a -> Maybe Ty -> Exp a
pattern Lam ps body ty <- LamWithLocation _ ps body ty
  where
    Lam ps body ty = LamWithLocation unlocatedNode ps body ty

pattern TyExp :: Exp a -> Ty -> Exp a
pattern TyExp exp ty <- TyExpWithLocation _ exp ty
  where
    TyExp exp ty = TyExpWithLocation unlocatedNode exp ty

pattern Cond :: Exp a -> Exp a -> Exp a -> Exp a
pattern Cond cond thenExp elseExp <- CondWithLocation _ cond thenExp elseExp
  where
    Cond cond thenExp elseExp = CondWithLocation unlocatedNode cond thenExp elseExp

pattern Indexed :: Exp a -> Exp a -> Exp a
pattern Indexed lhs rhs <- IndexedWithLocation _ lhs rhs
  where
    Indexed lhs rhs = IndexedWithLocation unlocatedNode lhs rhs

{-# COMPLETE Var, Con, FieldAccess, Lit, Call, Lam, TyExp, Cond, Indexed #-}

locatedExp :: SourceSpan -> Exp a -> Exp a
locatedExp sourceSpan (Var n) = VarWithLocation (locatedNode sourceSpan) n
locatedExp sourceSpan (Con n es) = ConWithLocation (locatedNode sourceSpan) n es
locatedExp sourceSpan (FieldAccess me n) = FieldAccessWithLocation (locatedNode sourceSpan) me n
locatedExp sourceSpan (Lit lit) = LitWithLocation (locatedNode sourceSpan) lit
locatedExp sourceSpan (Call me n es) = CallWithLocation (locatedNode sourceSpan) me n es
locatedExp sourceSpan (Lam ps body ty) = LamWithLocation (locatedNode sourceSpan) ps body ty
locatedExp sourceSpan (TyExp exp ty) = TyExpWithLocation (locatedNode sourceSpan) exp ty
locatedExp sourceSpan (Cond cond thenExp elseExp) = CondWithLocation (locatedNode sourceSpan) cond thenExp elseExp
locatedExp sourceSpan (Indexed lhs rhs) = IndexedWithLocation (locatedNode sourceSpan) lhs rhs

instance (HasSourceSpan a) => HasSourceSpan (Exp a) where
  sourceSpanOf (VarWithLocation location n) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf n]
  sourceSpanOf (ConWithLocation location n es) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf n, sourceSpanOf es]
  sourceSpanOf (FieldAccessWithLocation location me n) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf me, sourceSpanOf n]
  sourceSpanOf (LitWithLocation location _) =
    sourceSpanOf location
  sourceSpanOf (CallWithLocation location me n es) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf me, sourceSpanOf n, sourceSpanOf es]
  sourceSpanOf (LamWithLocation location ps body ty) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf ps, sourceSpanOf body, sourceSpanOf ty]
  sourceSpanOf (TyExpWithLocation location exp ty) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf exp, sourceSpanOf ty]
  sourceSpanOf (CondWithLocation location cond thenExp elseExp) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf cond, sourceSpanOf thenExp, sourceSpanOf elseExp]
  sourceSpanOf (IndexedWithLocation location lhs rhs) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf lhs, sourceSpanOf rhs]

-- pattern matching equations

data Pat a
  = PVarWithLocation NodeLocation a
  | PConWithLocation NodeLocation a [Pat a]
  | PWildcardWithLocation NodeLocation
  | PLitWithLocation NodeLocation Literal
  deriving (Eq, Ord, Show, Data, Typeable)

pattern PVar :: a -> Pat a
pattern PVar n <- PVarWithLocation _ n
  where
    PVar n = PVarWithLocation unlocatedNode n

pattern PCon :: a -> [Pat a] -> Pat a
pattern PCon n ps <- PConWithLocation _ n ps
  where
    PCon n ps = PConWithLocation unlocatedNode n ps

pattern PWildcard :: Pat a
pattern PWildcard <- PWildcardWithLocation _
  where
    PWildcard = PWildcardWithLocation unlocatedNode

pattern PLit :: Literal -> Pat a
pattern PLit lit <- PLitWithLocation _ lit
  where
    PLit lit = PLitWithLocation unlocatedNode lit

{-# COMPLETE PVar, PCon, PWildcard, PLit #-}

locatedPat :: SourceSpan -> Pat a -> Pat a
locatedPat sourceSpan (PVar n) = PVarWithLocation (locatedNode sourceSpan) n
locatedPat sourceSpan (PCon n ps) = PConWithLocation (locatedNode sourceSpan) n ps
locatedPat sourceSpan PWildcard = PWildcardWithLocation (locatedNode sourceSpan)
locatedPat sourceSpan (PLit lit) = PLitWithLocation (locatedNode sourceSpan) lit

instance (HasSourceSpan a) => HasSourceSpan (Pat a) where
  sourceSpanOf (PVarWithLocation location n) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf n]
  sourceSpanOf (PConWithLocation location n ps) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf n, sourceSpanOf ps]
  sourceSpanOf (PWildcardWithLocation location) =
    sourceSpanOf location
  sourceSpanOf (PLitWithLocation location _) =
    sourceSpanOf location

-- definition of literals

data Literal
  = IntLit Integer
  | StrLit String
  deriving (Eq, Ord, Show, Data, Typeable)
