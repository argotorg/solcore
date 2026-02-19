{-# LANGUAGE PatternSynonyms #-}

module Solcore.Backend.Mast where

{- Monomorphic Abstract Syntax Tree
   Represents specialised code with no type variables or meta variables.
   Produced by the specialiser, consumed by EmitHull.
-}

import Common.Pretty
import Language.Yul (YulBlock)
import Solcore.Frontend.Pretty.Name ()
import Solcore.Frontend.Pretty.SolcorePretty ()
import Solcore.Frontend.Syntax.Contract (DataTy (..), Import (..))
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt (Literal (..))
import Solcore.Frontend.Syntax.Ty (Ty (..), Tyvar (..))
import Solcore.Primitives.Primitives (word)

-----------------------------------------------------------------------
-- Types: no TyVar, no Meta — only type constructors
-----------------------------------------------------------------------

data MastTy = MastTyCon Name [MastTy]
  deriving (Eq, Ord, Show)

pattern MastArrow :: MastTy -> MastTy -> MastTy
pattern MastArrow a b = MastTyCon (Name "->") [a, b]

-----------------------------------------------------------------------
-- Identifiers: name + monomorphic type
-----------------------------------------------------------------------

data MastId = MastId {mastIdName :: Name, mastIdType :: MastTy}
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------
-- Top level
-----------------------------------------------------------------------

data MastCompUnit = MastCompUnit
  { mastImports :: [Import],
    mastTopDecls :: [MastTopDecl]
  }
  deriving (Eq, Ord, Show)

data MastTopDecl
  = MastTContr MastContract
  | MastTDataDef DataTy
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------
-- Contract: no type parameters
-----------------------------------------------------------------------

data MastContract = MastContract
  { mastContrName :: Name,
    mastContrDecls :: [MastContractDecl]
  }
  deriving (Eq, Ord, Show)

data MastContractDecl
  = MastCDataDecl DataTy
  | MastCFunDecl MastFunDef
  | MastCMutualDecl [MastContractDecl]
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------
-- Function def: no sigVars, no sigContext, return type always present
-----------------------------------------------------------------------

data MastFunDef = MastFunDef
  { mastFunName :: Name,
    mastFunParams :: [MastParam],
    mastFunReturn :: MastTy,
    mastFunBody :: [MastStmt]
  }
  deriving (Eq, Ord, Show)

data MastParam = MastParam
  { mastParamName :: Name,
    mastParamType :: MastTy
  }
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------
-- Statements
-----------------------------------------------------------------------

data MastStmt
  = MastAssign MastId MastExp
  | MastLet MastId (Maybe MastTy) (Maybe MastExp)
  | MastStmtExp MastExp
  | MastReturn MastExp
  | MastMatch MastExp [MastAlt]
  | MastAsm YulBlock
  deriving (Eq, Ord, Show)

type MastAlt = (MastPat, [MastStmt])

-----------------------------------------------------------------------
-- Expressions
-----------------------------------------------------------------------

data MastExp
  = MastVar MastId
  | MastCon MastId [MastExp]
  | MastLit Literal
  | MastCall MastId [MastExp]
  | MastCond MastExp MastExp MastExp
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------
-- Patterns
-----------------------------------------------------------------------

data MastPat
  = MastPVar MastId
  | MastPCon MastId [MastPat]
  | MastPWildcard
  | MastPLit Literal
  deriving (Eq, Ord, Show)

-----------------------------------------------------------------------
-- Type of expression
-----------------------------------------------------------------------

typeOfMastExp :: MastExp -> MastTy
typeOfMastExp (MastVar i) = mastIdType i
typeOfMastExp (MastCon i []) = mastIdType i
typeOfMastExp (MastCon i args) = go (mastIdType i) args
  where
    go ty [] = ty
    go (MastArrow _ u) (_ : as) = go u as
    go _ _ = error $ "typeOfMastExp(Con): " ++ show (MastCon i args)
typeOfMastExp (MastLit (IntLit _)) = tyToMast word
typeOfMastExp (MastLit (StrLit _)) = error "typeOfMastExp: string literal"
typeOfMastExp (MastCall i args) = applyTo args (mastIdType i)
  where
    applyTo [] ty = ty
    applyTo (_ : as) (MastArrow _ u) = applyTo as u
    applyTo _ _ = error $ "typeOfMastExp(Call): " ++ show (MastCall i args)
typeOfMastExp (MastCond _ _ e) = typeOfMastExp e

-----------------------------------------------------------------------
-- Conversion helpers: MastTy <-> Ty
-----------------------------------------------------------------------

mastToTy :: MastTy -> Ty
mastToTy (MastTyCon n ts) = TyCon n (map mastToTy ts)

tyToMast :: Ty -> MastTy
tyToMast (TyCon n ts) = MastTyCon n (map tyToMast ts)
-- Catch-all pattern variables may retain unresolved type variables from the
-- match compiler; these types are never inspected by EmitHull so we pass
-- them through as nullary type constructors.
tyToMast (TyVar (TVar n)) = MastTyCon n []
tyToMast (TyVar v) = error $ "tyToMast: unexpected type variable " ++ show v
tyToMast (Meta m) = error $ "tyToMast: unexpected meta variable " ++ show m

-----------------------------------------------------------------------
-- Pretty instances
-----------------------------------------------------------------------

-- Reuse the same printing conventions as the main AST pretty printer

instance Pretty MastTy where
  ppr (MastArrow t1@(MastArrow _ _) t2) = parens (ppr t1) <+> text "->" <+> ppr t2
  ppr (MastArrow t1 t2) = ppr t1 <+> text "->" <+> ppr t2
  ppr (MastTyCon n ts)
    | isTuple n = parens $ commaSep (map ppr ts)
    | isUnit n = text "()"
    | otherwise = ppr n >< pprTyParams ts

instance Pretty MastId where
  ppr (MastId n t) = ppr n >< text "<" >< ppr t >< text ">"

instance Pretty MastCompUnit where
  ppr (MastCompUnit imps ds) = vcat (map ppr imps ++ map ppr ds)

instance Pretty MastTopDecl where
  ppr (MastTContr c) = ppr c
  ppr (MastTDataDef d) = ppr d

instance Pretty MastContract where
  ppr (MastContract n ds) =
    text "contract"
      <+> ppr n
      <+> lbrace
      $$ nest 3 (vcat (map ppr ds))
      $$ rbrace

instance Pretty MastContractDecl where
  ppr (MastCDataDecl dt) = ppr dt
  ppr (MastCFunDecl fd) = ppr fd
  ppr (MastCMutualDecl ds) = vcat (map ppr ds)

instance Pretty MastFunDef where
  ppr (MastFunDef n ps ret bd) =
    text "function"
      <+> ppr n
      <+> parens (commaSep (map ppr ps))
      <+> text "->"
      <+> ppr ret
      <+> lbrace
      $$ nest 3 (vcat (map ppr bd))
      $$ rbrace

instance Pretty MastParam where
  ppr (MastParam n t) = ppr n <+> colon <+> ppr t

instance Pretty MastStmt where
  ppr (MastAssign i e) = ppr i <+> equals <+> ppr e <+> semi
  ppr (MastLet i ty m) =
    text "let" <+> ppr i <+> pprOptMastTy ty <+> pprMastInit m
  ppr (MastStmtExp e) = ppr e >< semi
  ppr (MastReturn e) = text "return" <+> ppr e >< semi
  ppr (MastMatch e alts) =
    text "match"
      <+> parens (ppr e)
      <+> lbrace
      $$ vcat (map pprMastAlt alts)
      $$ rbrace
  ppr (MastAsm yblk) =
    text "assembly"
      <+> lbrace
      $$ nest 3 (vcat (map ppr yblk))
      $$ rbrace

pprMastAlt :: MastAlt -> Doc
pprMastAlt (p, ss) =
  text "|"
    <+> ppr p
    <+> text "=>"
    $$ nest 3 (vcat (map ppr ss))

pprOptMastTy :: Maybe MastTy -> Doc
pprOptMastTy Nothing = empty
pprOptMastTy (Just t) = text ":" <+> ppr t

pprMastInit :: Maybe MastExp -> Doc
pprMastInit Nothing = semi
pprMastInit (Just e) = equals <+> ppr e <+> semi

instance Pretty MastExp where
  ppr (MastVar v) = ppr v
  ppr (MastCon n es)
    | isTuple n = parens $ commaSep (map ppr es)
    | otherwise =
        ppr n
          >< if null es
            then empty
            else parens (nest 1 $ commaSep $ map ppr es)
  ppr (MastLit l) = ppr l
  ppr (MastCall f as) = ppr f >< parens (nest 1 $ commaSep $ map ppr as)
  ppr (MastCond e1 e2 e3) = hsep [text "if", ppr e1, text "then", ppr e2, text "else", ppr e3]

instance Pretty MastPat where
  ppr (MastPVar n) = ppr n
  ppr (MastPCon n []) = ppr n
  ppr (MastPCon n ps)
    | isTuple n = parens (commaSep $ map ppr ps)
    | otherwise = ppr n >< parens (commaSep $ map ppr ps)
  ppr MastPWildcard = text "_"
  ppr (MastPLit l) = ppr l

-----------------------------------------------------------------------
-- Helpers (shared with SolcorePretty)
-----------------------------------------------------------------------

isUnit :: Name -> Bool
isUnit n = show n == "unit"

isTuple :: (Pretty a) => a -> Bool
isTuple s = render (ppr s) == "pair"

pprTyParams :: [MastTy] -> Doc
pprTyParams [] = empty
pprTyParams ts = parens (commaSep (map ppr ts))
