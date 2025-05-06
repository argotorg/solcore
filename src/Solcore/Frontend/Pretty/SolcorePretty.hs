{-# LANGUAGE InstanceSigs #-}
module Solcore.Frontend.Pretty.SolcorePretty(module Common.Pretty, pretty) where

import qualified Data.List.NonEmpty as N 
import Data.List

import Prelude hiding ((<>))

import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name 
import Solcore.Frontend.Pretty.Name
import Solcore.Frontend.Syntax.Stmt 
import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.TcSubst 

import Common.Pretty
import Language.Yul

-- For compatibility
(<>) :: Doc -> Doc -> Doc
(<>) = (><)

-- top level pretty printer function 

pretty :: Pretty a => a -> String 
pretty = render . ppr

instance Pretty a => Pretty (Qual a) where 
  ppr (ps :=> t) = pprContext ps <+> ppr t

instance Pretty ([Pred],Ty) where 
  ppr (x, y) = ppr (x :=> y)

instance Pretty a => Pretty (CompUnit a) where 
  ppr (CompUnit imps cs)
    = vcat (map ppr imps ++ map ppr cs)

instance Pretty Import where 
  ppr (Import qn) 
    = text "import" <+> ppr qn <+> semi

instance Pretty a => Pretty (TopDecl a) where 
  ppr (TContr c) = ppr c 
  ppr (TFunDef fd) = ppr fd 
  ppr (TClassDef c) = ppr c 
  ppr (TInstDef is) = ppr is 
  ppr (TMutualDef ts)
    = vcat (map ppr ts)
  ppr (TDataDef d) = ppr d
  ppr (TPragmaDecl p) = ppr p 

instance Pretty Pragma where 
  ppr (Pragma _ Enabled) = empty 
  ppr (Pragma ty st) 
    = hsep [text "pragma", ppr ty, ppr st, semi] 

instance Pretty PragmaType where 
  ppr NoBoundVariableCondition = text "no-bounded-variable-condition"
  ppr NoCoverageCondition = text "no-coverage-condition"
  ppr NoPattersonCondition = text "no-patterson-condition"

instance Pretty PragmaStatus where 
  ppr (DisableFor ns) 
    = commaSep (map ppr $ N.toList ns)
  ppr _ = empty 

instance Pretty a => Pretty (Contract a) where 
  ppr (Contract n ts ds)
    = text "contract" <+> 
      ppr n <+> 
      pprTyParams (map TyVar ts) <+> 
      lbrace $$ 
      nest 3 (vcat (map ppr ds)) $$ 
      rbrace 

instance Pretty a => Pretty (ContractDecl a) where 
  ppr (CDataDecl dt)
    = ppr dt 
  ppr (CFieldDecl fd)
    = ppr fd 
  ppr (CFunDecl fd)
    = ppr fd
  ppr (CMutualDecl ds) 
    = vcat (map ppr ds)
  ppr (CConstrDecl c)
    = ppr c 

instance Pretty a => Pretty (Constructor a) where 
  ppr (Constructor ps bd)
    =  text "constructor" <+> 
       pprParams ps <+> 
       lbrace $$ 
       nest 3 (vcat (map ppr bd)) $$ 
       rbrace

instance Pretty DataTy where 
  ppr (DataTy n ps cs)
    = text "data" <+> 
      ppr n <+>
      pprTyParams (map TyVar ps) <+> 
      equals <+>  
      hsep (punctuate bar (map ppr cs))
    where 
      bar = text " |"

instance Pretty TySym where 
  ppr (TySym n vs t) 
    = text "type" <+> 
      ppr n <+> 
      pprTyParams (map TyVar vs) <+> 
      text "=" <+> 
      ppr t

instance Pretty Constr where
  ppr (Constr n []) = ppr n <> text " "
  ppr (Constr n ts)
    = ppr n <> parens (pprConstrArgs ts)

pprConstrArgs :: [Ty] -> Doc  
pprConstrArgs [] = empty 
pprConstrArgs ts = commaSep $ map ppr ts 

instance Pretty a => Pretty (Class a) where 
  ppr (Class ps n vs v sigs)
    = pprSigPrefix (fv ps) ps <+>
      text "class " <+> 
      ppr v <+> 
      colon <+> 
      ppr n <+>
      pprTyParams (TyVar <$> vs) <+>
      lbrace $$ 
      nest 3 (pprSignatures sigs) $$  
      rbrace 

pprSignatures :: Pretty a => [Signature a] -> Doc 
pprSignatures 
  = vcat . map ppr

instance Pretty a => Pretty (Signature a) where 
   ppr (Signature vs ctx n ps ty) 
    = pprSigPrefix vs ctx <+> 
      text "function" <+> 
      ppr n <+> 
      pprParams ps <+> 
      pprRetTy ty 

pprSigPrefix :: [Tyvar] -> [Pred] -> Doc 
pprSigPrefix [] [] = empty 
pprSigPrefix vs [] 
  = text "forall" <+> hsep (map ppr vs) <+> text "." 
pprSigPrefix vs ps 
  = text "forall" <+> hsep (map ppr vs) <+> text "." <+> pprContext ps 

instance Pretty a => Pretty (Instance a) where 
  ppr (Instance d ctx n tys ty funs)
    = pprSigPrefix (fv ctx `union` fv (ty : tys)) ctx <+>
      pprDefault d    <>
      text "instance" <+> 
      ppr ty          <+>
      colon           <+> 
      ppr n           <+> 
      pprTyParams tys <+> 
      lbrace          $$ 
      nest 3 (pprFunBlock funs) $$ 
      rbrace 

pprDefault :: Bool -> Doc 
pprDefault b = if b then text "default " else empty

pprContext :: [Pred] -> Doc 
pprContext [] = empty 
pprContext ps 
  = (commaSep $ map ppr ps) <+> text "=>"

instance Pretty [Pred] where 
  ppr = hsep . map ppr 

pprFunBlock :: Pretty a => [FunDef a] -> Doc 
pprFunBlock 
  = vcat . map ppr

instance Pretty a => Pretty (Field a) where 
  ppr (Field n ty e)
    = ppr n <+> colon <+> (ppr ty) <+> pprInitOpt e

instance Pretty a => Pretty (Body a) where 
  ppr = vcat . map ppr

instance Pretty a => Pretty (FunDef a) where 
  ppr (FunDef sig bd)
    = ppr sig <+>
      lbrace $$ 
      nest 3 (vcat (map ppr bd)) $$ 
      rbrace

pprRetTy :: Maybe Ty -> Doc  
pprRetTy (Just t) = text "->" <+> ppr t
pprRetTy Nothing = empty 

pprParams :: Pretty a => [Param a] -> Doc  
pprParams = parens . commaSep . map ppr

instance Pretty a => Pretty (Param a) where 
  ppr (Typed n ty) 
    = ppr n <+> colon <+> ppr ty
  ppr (Untyped n)
    = ppr n

instance Pretty a => Pretty (Stmt a) where 
  ppr (n := e) 
    = ppr n <+> equals <+> ppr e <+> semi 
  ppr (Let n ty m)
    = text "let" <+> ppr n <+> pprOptTy ty <+> pprInitOpt m 
  ppr (StmtExp e)
    = ppr e <> semi
  ppr (Return e)
    = text "return" <+> ppr e <> semi
  ppr (Match e eqns)
    = text "match" <+> 
      (parens $ commaSep $ map ppr e) <+> 
      lbrace $$ 
      vcat (map ppr eqns) $$ 
      rbrace
  ppr (Asm yblk) 
    = text "assembly" <+> 
        ppr (YBlock yblk)

instance Pretty a => Pretty (Equation a) where 
  ppr (p,ss) 
    = text "|" <+> commaSep (map ppr p) <+> text "=>" $$ 
      nest 3 (vcat (map ppr ss))

instance Pretty a => Pretty (Equations a) where 
  ppr = vcat . map ppr

pprOptTy :: Maybe Ty -> Doc 
pprOptTy Nothing = empty 
pprOptTy (Just t)
  | isVar t = empty 
  | otherwise = case splitTy t of 
                  ([],t') -> text ":" <+> ppr t' 
                  (ts', t') -> 
                    text ":" <+> parens (commaSep (map ppr ts')) <+> 
                    text "->" <+> ppr t'

isVar (TyVar _) = True 
isVar _ = False 

pprInitOpt :: Pretty a => Maybe (Exp a) -> Doc
pprInitOpt Nothing = semi
pprInitOpt (Just e) = equals <+> ppr e <+> semi 

instance Pretty a => Pretty (Exp a) where 
  ppr (Var v) = ppr v 
  ppr (Con n es)
    | isTuple n = parens $ commaSep (map ppr es)
    | otherwise 
      = ppr n <> if null es then empty 
                 else (parens $ commaSep $ map ppr es)
  ppr (Lit l) = ppr l 
  ppr (Call e n es) 
    = pprE e <> ppr n <> (parens $ commaSep $ map ppr es)
  ppr (Lam args bd _) 
    = text "lam" <+> 
      pprParams args <+> 
      lbrace $$ 
      nest 3 (vcat (map ppr bd)) $$
      rbrace
  ppr (TyExp e ty) 
    = ppr e <+> text ":" <+> ppr ty

pprE :: Pretty a => Maybe (Exp a) -> Doc  
pprE Nothing = ""
pprE (Just e) = ppr e <> text "."

instance Pretty a => Pretty (Pat a) where 
  ppr (PVar n) 
    = ppr n
  ppr (PCon n []) = ppr n
  ppr (PCon n ps@(_ : _))
    | isTuple n = parens (commaSep $ map ppr ps) 
    | otherwise = ppr n <> (parens $ commaSep $ map ppr ps )
  ppr PWildcard 
    = text "_"
  ppr (PLit l)
    = ppr l

instance Pretty Literal where 
  ppr (IntLit l) = integer (toInteger l)
  ppr (StrLit l) = quotes (text l)

instance Pretty Tyvar where 
  ppr (TVar n) = ppr n

instance Pretty Pred where 
  ppr (InCls n t ts) =
    ppr t <+> colon <+> ppr n <+> pprTyParams ts 
  ppr (t1 :~: t2) = 
    ppr t1 <+> text "~" <+> ppr t2

instance Pretty Scheme where
  ppr (Forall vs ty) = ppr' (Forall vs ty) 
    where 
      ppr' (Forall [] ([] :=> t)) = ppr t
      ppr' (Forall [] (ctx :=> t))
        = text "forall"       <+>
          pprContext ctx <+> ppr t
      ppr' (Forall vs (ctx :=> t)) 
        = text "forall"       <+> 
          hsep (map ppr vs)   <+>
          text "."            <+> 
          pprContext ctx      <+>
          ppr t 

instance Pretty Ty where 
  ppr (TyVar v) = ppr v
  ppr (t1@(_ :-> _) :-> t2) 
    = parens (ppr t1) <+> text "->" <+> ppr t2
  ppr (t1 :-> t2) 
    = ppr t1 <+> (text "->") <+> ppr t2
  ppr (TyCon n ts)
    | isTuple n = parens $ commaSep (map ppr ts)
    | isUnit n = text "()"
    | otherwise = ppr n <> (pprTyParams ts)

isUnit :: Name -> Bool 
isUnit n = pretty n == "unit"

isTuple :: Pretty a => a -> Bool 
isTuple s = pretty s == "pair"

pprTyParams :: [Ty] -> Doc 
pprTyParams [] = empty 
pprTyParams ts 
  = parens (commaSep (map ppr ts))


instance Pretty Subst where 
  ppr = braces . commaSep . map go . unSubst
    where 
      go (v,t) = ppr v <+> text "+->" <+> ppr t


instance Pretty Id  where 
  ppr (Id n t) = ppr n  <> text "<" <> ppr t <> text ">" 

