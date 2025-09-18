
{-# OPTIONS_GHC -Wincomplete-patterns #-}
{-# LANGUAGE InstanceSigs #-}
module Language.Core
  ( Expr(..), Stmt(..), Arg(..), Alt(..), pattern ConAlt, Pat(..), Con(..), Contract(..), Core(..), Body
  , module Language.Core.Types
  ,  pattern SAV
  , Name
  ) where

import Common.Pretty
import Language.Core.Types
import Language.Yul


type Name = String

data Expr
    = EWord Integer
    | EBool Bool
    | EVar Name
    | EPair Expr Expr
    | EFst Expr
    | ESnd Expr
    | EInl Type Expr
    | EInr Type Expr
    | EInK Int Type Expr
    | ECall Name [Expr]
    | EUnit
instance Show Expr where
    show = render . ppr

pattern SAV :: Name -> Expr -> Stmt
pattern SAV x e = SAssign (EVar x) e
data Stmt
    = SAssign Expr Expr
    | SAlloc Name Type
    | SExpr Expr
    | SAssembly [YulStmt]
    | SReturn Expr
    | SComment String
    | SBlock Body
    -- | SMatch Expr [Alt]
    | SMatch Type Expr [Alt]
    | SFunction Name [Arg] Type [Stmt]
    | SRevert String
    -- deriving Show

type Body = [Stmt]
data Arg = TArg Name Type
instance Show Arg where show = render . ppr
instance Show Stmt where show :: Stmt -> String
                         show = render . ppr

data Alt = Alt Pat Name Body deriving Show
pattern ConAlt :: Con -> Name -> Body -> Alt
pattern ConAlt c n s = Alt (PCon c) n s

data Pat = PVar Name | PCon Con | PWildcard | PIntLit Integer
  deriving Show
data Con = CInl | CInr | CInK Int deriving Show

data Contract = Contract { ccName :: Name, ccStmts ::  [Stmt] }

newtype Core = Core [Stmt]
instance Show Core where show = render . ppr
instance Show Contract where show = render . ppr

instance Pretty Contract where
    ppr (Contract n stmts) = text "contract" <+> text n <+> lbrace $$ nest 4 (vcat (map ppr stmts)) $$ rbrace

instance Pretty Type where
    ppr TWord = text "word"
    ppr TBool = text "bool"
    ppr TUnit = text "unit"
    ppr (TPair t1 t2) = parens (ppr t1 <+> text "*" <+> ppr t2)
    ppr (TSum t1 t2) = parens (ppr t1 <+> text "+" <+> ppr t2)
    ppr (TSumN ts) = text "sum" >< parens(commaSepList ts)
    ppr (TFun ts t) = parens (hsep (map ppr ts) <+> text "->" <+> ppr t)
    ppr (TNamed n t) = text n >< braces(ppr t)

instance Pretty Expr where
    ppr (EWord i) = text (show i)
    ppr (EBool b) = text (show b)
    ppr EUnit = text "()"
    ppr (EVar x) = text x
    ppr (EPair e1 e2) = parens (ppr e1 >< comma <+> ppr e2)
    ppr (EFst e) = text "fst" >< parens (ppr e)
    ppr (ESnd e) = text "snd" >< parens (ppr e)
    ppr (EInl t e) = text "inl" >< angles (ppr t) >< parens (ppr e)
    ppr (EInr t e) = text "inr" >< angles (ppr t) >< parens (ppr e)
    ppr (EInK k t e) = text "in" >< parens(int k) >< angles (ppr t) >< parens (ppr e)
    ppr (ECall f es) = text f >< parens(commaSepList es)

instance Pretty Stmt where
    ppr (SAssign lhs rhs) = ppr lhs <+> text ":=" <+> ppr rhs
    ppr (SAlloc x t) = text "let" <+> text x <+> text ":" <+> ppr t
    ppr (SExpr e) = ppr e
    ppr (SAssembly yul) = text "assembly" <+> lbrace
                            $$ nest 2 (vcat (map ppr yul))
                            $$ rbrace
    ppr (SReturn e) = text "return" <+> ppr e
    ppr (SComment c) = text "/*" <+> text c <+> "*/"
    ppr (SBlock stmts) = lbrace $$ nest 2 (vcat (map ppr stmts)) $$ rbrace
    {-
    ppr (SMatch e alts) =
        text "match" <+> ppr e <+> text "with"
        <+> lbrace $$ nest 2 (vcat $ map ppr alts) $$ rbrace
    -}
    ppr (SMatch t e alts) =
        text "match" >< angles (ppr t) <+> ppr e <+> text "with"
        <+> lbrace $$ nest 2 (vcat $ map ppr alts) $$ rbrace
    ppr (SFunction f args ret stmts) =
        text "function" <+> text f
        <+> parens (hsep (punctuate comma (map ppr args)))
        <+> text "->" <+> ppr ret
        <+> lbrace $$ nest 2 (vcat (map ppr stmts))  $$ rbrace
    ppr (SRevert s) = text "revert" <+> text (show s)

instance Pretty Pat where
    ppr (PVar x) = text x
    ppr (PCon c) = ppr c
    ppr PWildcard = text "_"
    ppr (PIntLit i) = integer i

instance Pretty Alt where
    ppr (Alt c n s) = ppr c <+> text n <+> text "=>" <+> braces(ppr s)

instance Pretty Con where
    ppr CInl = text "inl"
    ppr CInr = text "inr"
    ppr (CInK k) = text "in" <+> parens (int k)

instance Pretty Arg where
    ppr (TArg n t) = text n <+> text ":" <+> ppr t

instance Pretty Core where
    ppr (Core stmts) = vcat (map ppr stmts)


instance Pretty [Stmt] where
    ppr stmts = vcat (map ppr stmts)
