{-# OPTIONS_GHC -Wno-orphans #-}

module Solcore.Frontend.Pretty.TreePretty where

import Common.Pretty
import Data.List.NonEmpty qualified as N
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

pretty :: (Pretty a) => a -> String
pretty = render . ppr

instance Pretty CompUnit where
  ppr (CompUnit imps cs) =
    vcat (map ppr imps ++ map ppr cs)

instance Pretty Import where
  ppr (Import qn) =
    text "import" <+> ppr qn <+> semi

instance Pretty TopDecl where
  ppr (TContr c) = ppr c
  ppr (TFunDef fd) = ppr fd
  ppr (TClassDef c) = ppr c
  ppr (TInstDef is) = ppr is
  ppr (TDataDef d) = ppr d
  ppr (TSym s) = ppr s
  ppr (TPragmaDecl p) = ppr p

instance Pretty Pragma where
  ppr (Pragma _ Enabled) = empty
  ppr (Pragma ty st) =
    hsep [text "pragma", ppr ty, ppr st, semi]

instance Pretty PragmaType where
  ppr NoBoundVariableCondition = text "no-bounded-variable-condition"
  ppr NoCoverageCondition = text "no-coverage-condition"
  ppr NoPattersonCondition = text "no-patterson-condition"

instance Pretty PragmaStatus where
  ppr (DisableFor ns) =
    commaSep (map ppr $ N.toList ns)
  ppr _ = empty

instance Pretty Contract where
  ppr (Contract n ts ds) =
    text "contract"
      <+> ppr n
      <+> pprTyParams ts
      <+> lbrace
      $$ nest 3 (vcat (map ppr ds))
      $$ rbrace

instance Pretty ContractDecl where
  ppr (CDataDecl dt) =
    ppr dt
  ppr (CFieldDecl fd) =
    ppr fd
  ppr (CFunDecl fd) =
    ppr fd
  ppr (CConstrDecl c) =
    ppr c

instance Pretty Constructor where
  ppr (Constructor ps bd) =
    text "constructor"
      <+> pprParams ps
      <+> lbrace
      $$ nest 3 (vcat (map ppr bd))
      $$ rbrace

instance Pretty DataTy where
  ppr (DataTy n ps cs) =
    text "data"
      <+> ppr n
      <+> pprTyParams ps
      <+> rs
      <+> text ";"
    where
      rs =
        if null cs
          then empty
          else
            equals <+> hsep (punctuate bar (map ppr cs))
      bar = text " |"

instance Pretty TySym where
  ppr (TySym n vs t) =
    text "type"
      <+> ppr n
      <+> pprTyParams vs
      <+> text "="
      <+> ppr t

instance Pretty Constr where
  ppr (Constr n []) = ppr n <> text " "
  ppr (Constr n ts) =
    ppr n <> parens (pprConstrArgs ts)

pprConstrArgs :: [Ty] -> Doc
pprConstrArgs [] = empty
pprConstrArgs ts = commaSep $ map ppr ts

instance Pretty Class where
  ppr (Class bvs ps n vs v sigs) =
    pprSigPrefix bvs ps
      <+> text "class "
      <+> ppr v
      <+> colon
      <+> ppr n
      <+> pprTyParams vs
      <+> lbrace
      $$ nest 3 (pprSignatures sigs)
      $$ rbrace

pprSignatures :: [Signature] -> Doc
pprSignatures =
  vcat . map ((<> semi) . ppr)

instance Pretty Signature where
  ppr (Signature vs ctx n ps ty) =
    pprSigPrefix vs ctx
      <+> text "function"
      <+> ppr n
      <+> pprParams ps
      <+> pprRetTy ty

pprSigPrefix :: [Ty] -> [Pred] -> Doc
pprSigPrefix [] [] = empty
pprSigPrefix [] ps = pprContext ps
pprSigPrefix vs [] =
  text "forall" <+> hsep (map ppr vs) <+> text "."
pprSigPrefix vs ps =
  text "forall" <+> hsep (map ppr vs) <+> text "." $$ pprContext ps

instance Pretty Instance where
  ppr (Instance d vs ctx n tys ty funs) =
    pprSigPrefix vs ctx
      <+> pprDefault d
      <+> text "instance"
      <+> ppr ty
      <+> colon
      <+> ppr n
      <+> pprTyParams tys
      <+> lbrace
      $$ nest 3 (pprFunBlock funs)
      $$ rbrace

pprDefault :: Bool -> Doc
pprDefault b = if b then text "default " else empty

pprContext :: [Pred] -> Doc
pprContext [] = empty
pprContext ps =
  (commaSep $ map ppr ps) <+> text "=>"

instance Pretty [Pred] where
  ppr = parens . commaSepList

pprFunBlock :: [FunDef] -> Doc
pprFunBlock =
  vcat . map ppr

instance Pretty Field where
  ppr (Field n ty e) =
    ppr n <+> colon <+> (ppr ty) <+> pprInitOpt e

instance Pretty Body where
  ppr = vcat . map ppr

instance Pretty FunDef where
  ppr (FunDef sig bd) =
    ppr sig
      <+> lbrace
      $$ nest 3 (vcat (map ppr bd))
      $$ rbrace

pprRetTy :: Maybe Ty -> Doc
pprRetTy (Just t) = text "->" <+> ppr t
pprRetTy Nothing = empty

pprParams :: [Param] -> Doc
pprParams = parens . commaSep . map ppr

instance Pretty Param where
  ppr (Typed n ty) =
    ppr n <+> colon <+> ppr ty
  ppr (Untyped n) =
    ppr n

instance Pretty Stmt where
  ppr (Assign n e) =
    ppr n <+> equals <+> ppr e <+> semi
  ppr (StmtPlusEq e1 e2) =
    hsep [ppr e1, text "+=", ppr e2]
  ppr (StmtMinusEq e1 e2) =
    hsep [ppr e1, text "-=", ppr e2]
  ppr (Let n ty m) =
    text "let" <+> ppr n <+> pprOptTy ty <+> pprInitOpt m
  ppr (StmtExp e) =
    ppr e <> semi
  ppr (Return e) =
    text "return" <+> ppr e <+> semi
  ppr (Match e eqns) =
    text "match"
      <+> (parens $ commaSep $ map ppr e)
      <+> lbrace
      $$ vcat (map ppr eqns)
      $$ rbrace
  ppr (Asm yblk) =
    text "assembly"
      <+> lbrace
      $$ nest 3 (vcat (map ppr yblk))
      $$ rbrace
  ppr (If e blk1 blk2) =
    text "if"
      <+> parens (ppr e)
      <+> lbrace
      $$ nest 3 (ppr blk1)
      $$ rbrace
      <+> text "else"
      <+> lbrace
      $$ nest 3 (ppr blk2)
      $$ rbrace

instance Pretty Equation where
  ppr (p, ss) =
    text "|"
      <+> commaSep (map ppr p)
      <+> text "=>"
      $$ nest 3 (vcat (map ppr ss))

instance Pretty Equations where
  ppr = vcat . map ppr

pprOptTy :: Maybe Ty -> Doc
pprOptTy Nothing = empty
pprOptTy (Just t) =
  case splitTy t of
    ([], t') -> text ":" <+> ppr t'
    (ts', t') ->
      text ":"
        <+> parens (commaSep (map ppr ts'))
        <+> text "->"
        <+> ppr t'

pprInitOpt :: Maybe Exp -> Doc
pprInitOpt Nothing = semi
pprInitOpt (Just e) = equals <+> ppr e <+> semi

parensWhen :: Bool -> Doc -> Doc
parensWhen True d = parens d
parensWhen _ d = d

instance Pretty Exp where
  ppr (Lit l) = ppr l
  ppr (ExpName me n es) =
    maybe (text "this") ppr me
      <> char 'n'
      <> ppr n
      <> parensWhen
        (not $ null es)
        (commaSep (map ppr es))
  ppr (ExpVar me v) =
    maybe (text "this") ppr me
      <> char '.'
      <> ppr v
  ppr (Lam args bd _) =
    text "lam"
      <+> pprParams args
      <+> lbrace
      $$ nest 3 (vcat (map ppr bd))
      $$ rbrace
  ppr (TyExp e ty) =
    ppr e <+> text ":" <+> ppr ty
  ppr (ExpIndexed e1 e2) =
    ppr e1 <> brackets (ppr e2)
  ppr (ExpPlus e1 e2) =
    hsep [ppr e1, text "+", ppr e2]
  ppr (ExpMinus e1 e2) =
    hsep [ppr e1, text "-", ppr e2]
  ppr (ExpTimes e1 e2) =
    hsep [ppr e1, text "*", ppr e2]
  ppr (ExpDivide e1 e2) =
    hsep [ppr e1, text "/", ppr e2]
  ppr (ExpModulo e1 e2) =
    hsep [ppr e1, text "%", ppr e2]
  ppr (ExpLT e1 e2) =
    hsep [ppr e1, text "<", ppr e2]
  ppr (ExpGT e1 e2) =
    hsep [ppr e1, text ">", ppr e2]
  ppr (ExpLE e1 e2) =
    hsep [ppr e1, text "<=", ppr e2]
  ppr (ExpGE e1 e2) =
    hsep [ppr e1, text ">=", ppr e2]
  ppr (ExpEE e1 e2) =
    hsep [ppr e1, text "==", ppr e2]
  ppr (ExpNE e1 e2) =
    hsep [ppr e1, text "!=", ppr e2]
  ppr (ExpLAnd e1 e2) =
    hsep [ppr e1, text "&&", ppr e2]
  ppr (ExpLOr e1 e2) =
    hsep [ppr e1, text "||", ppr e2]
  ppr (ExpLNot e1) =
    hsep [text "!", ppr e1]
  ppr (ExpCond e1 e2 e3) =
    hsep
      [ text "if",
        ppr e1,
        text "then",
        ppr e2,
        text "else",
        ppr e3
      ]

pprE :: Maybe Exp -> Doc
pprE Nothing = ""
pprE (Just e) = ppr e <> text "."

instance Pretty Pat where
  ppr (Pat n []) = ppr n
  ppr (Pat n ps@(_ : _))
    | isTuple n = parens (commaSep $ map ppr ps)
    | otherwise = ppr n <> (parens $ commaSep $ map ppr ps)
  ppr PWildcard =
    text "_"
  ppr (PLit l) =
    ppr l

instance Pretty Literal where
  ppr (IntLit l) = integer (toInteger l)
  ppr (StrLit l) = quotes (text l)

instance Pretty Pred where
  ppr (InCls n t ts) =
    ppr t <+> colon <+> ppr n <+> pprTyParams ts

instance Pretty Ty where
  ppr (t1@(_ :-> _) :-> t2) =
    parens (ppr t1) <+> text "->" <+> ppr t2
  ppr (t1 :-> t2) =
    ppr t1 <+> (text "->") <+> ppr t2
  ppr (TyCon n ts)
    | isTuple n = parens $ commaSep (map ppr ts)
    | isUnit n = text "()"
    | otherwise = ppr n <> (pprTyParams ts)

isUnit :: Name -> Bool
isUnit n = pretty n == "unit"

isTuple :: (Pretty a) => a -> Bool
isTuple s = pretty s == "pair"

pprTyParams :: [Ty] -> Doc
pprTyParams [] = empty
pprTyParams ts =
  parens (commaSep (map ppr ts))

splitTy :: Ty -> ([Ty], Ty)
splitTy (a :-> b) =
  let (as, r) = splitTy b
   in (a : as, r)
splitTy t = ([], t)
