{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Solcore.Frontend.Pretty.SolcorePretty (module Common.Pretty, pretty) where

import Common.Pretty
import Data.List
import Data.List.NonEmpty qualified as N
import Data.Map qualified as Map
import Language.Yul ()
import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.TcSubst
import Prelude hiding ((<>))

-- For compatibility
(<>) :: Doc -> Doc -> Doc
(<>) = (><)

-- top level pretty printer function

pretty :: (Pretty a) => a -> String
pretty = render . ppr

instance (Pretty a) => Pretty (Qual a) where
  ppr (ps :=> t) = pprContext ps <+> ppr t

instance Pretty ([Pred], Ty) where
  ppr (x, y) = ppr (x :=> y)

instance (Pretty a) => Pretty (CompUnit a) where
  ppr (CompUnit imps cs) =
    vcat (map ppr imps ++ map ppr cs)

instance Pretty Import where
  ppr (ImportModule path) =
    text "import" <+> (ppr path <> semi)
  ppr (ImportAlias path asName) =
    hsep [text "import", text "*", text "as", ppr asName, text "from", ppr path] <> semi
  ppr (ImportOnly path (SelectItems items hidden)) =
    hsep
      ( [ text "import",
          pprItemSelector items,
          text "from",
          ppr path
        ]
          ++ pprHiding hidden
      )
      <> semi

instance Pretty ModulePath where
  ppr (RelativePath path) = ppr path
  ppr (LibraryPath path) = text "lib." <> ppr path
  ppr (ExternalPath libName path) =
    text "@" <> ppr libName <> text "." <> ppr path

instance (Pretty a) => Pretty (TopDecl a) where
  ppr (TContr c) = ppr c
  ppr (TFunDef fd) = ppr fd
  ppr (TClassDef c) = ppr c
  ppr (TInstDef is) = ppr is
  ppr (TMutualDef ts) =
    vcat (map ppr ts)
  ppr (TDataDef d) = ppr d
  ppr (TSym s) = ppr s
  ppr (TExportDecl e) = ppr e
  ppr (TPragmaDecl p) = ppr p

instance Pretty Export where
  ppr (ExportList items) =
    hsep
      [ text "export",
        pprExportSpecs items <> semi
      ]
  ppr (ExportModule path) =
    hsep [text "export", ppr path <> semi]
  ppr (ExportModuleAs path asName) =
    hsep [text "export", ppr path, text "as", ppr asName <> semi]
  ppr (ExportItemsFrom path items)
    | exportSelectorIsOnlyWildcard items =
        hsep [text "export", ppr path <> text ".*;"]
    | otherwise =
        hsep [text "export", ppr path <> text ".", pprExportSelector items <> semi]

pprExportSpecs :: [ExportSpec] -> Doc
pprExportSpecs items = lbrace <> commaSep (map ppr items) <> rbrace

instance Pretty ExportSpec where
  ppr ExportAll = text "*"
  ppr (ExportName itemName) = ppr itemName
  ppr (ExportNameWithConstructors typeName ctorSelector) =
    ppr typeName <> parens (ppr ctorSelector)
  ppr (ExportModuleAll path) = ppr path <> text ".*"

instance Pretty ConstructorSelector where
  ppr SelectAllConstructors = text "*"
  ppr (SelectConstructors names) = commaSep (map ppr names)

pprExportSelector :: ExportSelector -> Doc
pprExportSelector (SelectExportItems items) =
  lbrace <> commaSep (map ppr items) <> rbrace

instance Pretty ExportSelectorEntry where
  ppr SelectExportAllItems = text "*"
  ppr (SelectExportItem itemName) = ppr itemName
  ppr (SelectExportConstructors typeName ctorSelector) =
    ppr typeName <> parens (ppr ctorSelector)

pprItemSelector :: [ItemSelectorEntry] -> Doc
pprItemSelector items =
  lbrace <> commaSep (map ppr items) <> rbrace

pprHiding :: [Name] -> [Doc]
pprHiding [] = []
pprHiding names =
  [text "hiding", lbrace <> commaSep (map ppr names) <> rbrace]

instance Pretty ItemSelectorEntry where
  ppr SelectAllItems = text "*"
  ppr (SelectItem itemName) = ppr itemName
  ppr (SelectItemAs itemName aliasName) =
    hsep [ppr itemName, text "as", ppr aliasName]

exportSelectorIsOnlyWildcard :: ExportSelector -> Bool
exportSelectorIsOnlyWildcard (SelectExportItems [SelectExportAllItems]) = True
exportSelectorIsOnlyWildcard _ = False

instance Pretty Pragma where
  ppr (Pragma (SolidityPragma version) Enabled) =
    hsep [text "pragma", text "solidity", text version] <> semi
  ppr (Pragma (AbiCoderPragma version) Enabled) =
    hsep [text "pragma", text "abicoder", text version] <> semi
  ppr (Pragma _ Enabled) = empty
  ppr (Pragma ty st) =
    hsep [text "pragma", text "solcore", ppr ty, ppr st] <> semi

instance Pretty PragmaType where
  ppr NoBoundVariableCondition = text "noBoundVariableCondition"
  ppr NoCoverageCondition = text "noCoverageCondition"
  ppr NoPattersonCondition = text "noPattersonCondition"
  ppr NoGenericInstanceFor = text "noGenericInstanceFor"
  ppr (SolidityPragma version) =
    hsep [text "solidity", text version]
  ppr (AbiCoderPragma version) =
    hsep [text "abicoder", text version]

instance Pretty PragmaStatus where
  ppr (DisableFor ns) =
    commaSep (map ppr $ N.toList ns)
  ppr _ = empty

instance (Pretty a) => Pretty (Contract a) where
  ppr (ContractWithKind kind n ts ds) =
    pprContractKind kind
      <+> (ppr n <> pprTyParams (map TyVar ts))
      <+> lbrace
      $$ nest 3 (vcat (map ppr ds))
      $$ rbrace

pprContractKind :: ContractKind -> Doc
pprContractKind ContractKind = text "contract"
pprContractKind InterfaceKind = text "interface"
pprContractKind LibraryKind = text "library"

instance (Pretty a) => Pretty (ContractDecl a) where
  ppr (CDataDecl dt) =
    ppr dt
  ppr (CFieldDecl fd) =
    ppr fd
  ppr (CFunDecl fd) =
    ppr fd
  ppr (CSignatureDecl isExternal sig) =
    pprContractSignature isExternal sig <> semi
  ppr (CMutualDecl ds) =
    vcat (map ppr ds)
  ppr (CConstrDecl c) =
    ppr c

instance (Pretty a) => Pretty (Constructor a) where
  ppr (Constructor ps bd payable) =
    (text "constructor" <> pprParams ps)
      <+> pprPayable payable
      <+> lbrace
      $$ nest 3 (vcat (map ppr bd))
      $$ rbrace

instance Pretty DataTy where
  ppr (StructTy n ps fieldNames fieldTypes) =
    text "struct"
      <+> (ppr (constructorLeafName n) <> pprTyParams (map TyVar ps))
      <+> lbrace
      $$ nest 3 (vcat (zipWith pprStructField fieldNames fieldTypes))
      $$ rbrace
  ppr (DataTy n ps cs) =
    text "enum"
      <+> (ppr (constructorLeafName n) <> pprTyParams (map TyVar ps))
      <+> lbrace
      $$ nest 3 (vcat (punctuate comma (map ppr cs)))
      $$ rbrace

pprStructField :: Name -> Ty -> Doc
pprStructField fieldName' fieldType =
  ((ppr fieldName' <> colon) <+> ppr fieldType) <> semi

instance Pretty TySym where
  ppr (TySym n vs t) =
    ( text "alias"
        <+> (ppr n <> pprTyParams (map TyVar vs))
        <+> equals
        <+> ppr t
    )
      <> semi

instance Pretty Constr where
  ppr (Constr n []) = ppr (constructorLeafName n)
  ppr (Constr n ts) =
    ppr (constructorLeafName n) <> parens (pprConstrArgs ts)

pprConstrArgs :: [Ty] -> Doc
pprConstrArgs [] = empty
pprConstrArgs ts = commaSep $ map ppr ts

instance (Pretty a) => Pretty (Class a) where
  ppr (Class _ ps n vs v sigs) =
    text "trait"
      <+> (ppr n <> pprTyParams (TyVar <$> (v : vs)))
      <+> pprWhere ps
      <+> lbrace
      $$ nest 3 (pprSignatures sigs)
      $$ rbrace

pprSignatures :: (Pretty a) => [Signature a] -> Doc
pprSignatures =
  vcat . map ((<> semi) . ppr)

instance (Pretty a) => Pretty (Signature a) where
  ppr = pprSignature False

instance (Pretty a) => Pretty (Instance a) where
  ppr (Instance d vs ctx n tys ty funs) =
    pprDefault d
      <> text "impl"
      <> pprTyParams (map TyVar vs)
      <+> (ppr n <> pprTyParams (ty : tys))
      <+> pprWhere ctx
      <+> lbrace
      $$ nest 3 (pprFunBlock funs)
      $$ rbrace

pprDefault :: Bool -> Doc
pprDefault b = if b then text "default " else empty

pprContext :: [Pred] -> Doc
pprContext [] = empty
pprContext ps =
  (commaSep $ map ppr ps) <+> text "=>"

pprWhere :: [Pred] -> Doc
pprWhere [] = empty
pprWhere ps =
  text "where" <+> commaSep (map ppr ps)

instance Pretty [Pred] where
  ppr = parens . commaSepList

pprFunBlock :: (Pretty a) => [FunDef a] -> Doc
pprFunBlock =
  vcat . map ppr

instance (Pretty a) => Pretty (Field a) where
  ppr (Field n ty e) =
    ((ppr n <> colon) <+> ppr ty) <> pprInitOpt e

instance (Pretty a) => Pretty (Body a) where
  ppr = vcat . map ppr

instance (Pretty a) => Pretty (FunDef a) where
  ppr (FunDef isPub sig bd) =
    pprSignature isPub sig
      <+> lbrace
      $$ nest 3 (vcat (map ppr (dropResolvedReturnLocals sig bd)))
      $$ rbrace

pprSignature :: (Pretty a) => Bool -> Signature a -> Doc
pprSignature isPub sig@(Signature vs ctx n ps rc ty _)
  | n == Name "fallback" =
      (text "fallback" <> pprParams ps)
        <+> pprResolvedFunctionModifiers (Just VisibilityExternal) sig
  | otherwise =
      text "function"
        <+> (ppr n <> pprTyParams (map TyVar vs) <> pprParams ps)
        <+> pprResolvedFunctionModifiers
          (if isPub then Just VisibilityPublic else Nothing)
          sig
        <+> pprResolvedReturns sig rc ty
        <+> pprWhere ctx

pprContractSignature :: (Pretty a) => Bool -> Signature a -> Doc
pprContractSignature isExternal sig@(Signature vs ctx n ps rc ty _) =
  text "function"
    <+> (ppr n <> pprTyParams (map TyVar vs) <> pprParams ps)
    <+> pprResolvedFunctionModifiers
      (if isExternal then Just VisibilityExternal else Nothing)
      sig
    <+> pprResolvedReturns sig rc ty
    <+> pprWhere ctx

pprResolvedReturns :: Signature a -> Bool -> Maybe Ty -> Doc
pprResolvedReturns sig returnComptime returnTy =
  case sigReturnItems sig of
    [] -> pprRetTy returnComptime returnTy
    items ->
      text "returns"
        <+> parens (commaSep (map pprResolvedReturnItem items))

pprResolvedReturnItem :: SignatureReturnItem -> Doc
pprResolvedReturnItem returnItem =
  pprConst (signatureReturnItemComptime returnItem)
    <> case signatureReturnItemName returnItem of
      Nothing -> ppr (signatureReturnItemType returnItem)
      Just returnName ->
        (ppr returnName <> colon)
          <+> ppr (signatureReturnItemType returnItem)

-- Name resolution materializes named return slots as uninitialized leading
-- lets. They are an internal representation detail; printing them alongside
-- the restored @returns (name: type)@ clause would create duplicate bindings
-- when the output is parsed again.
dropResolvedReturnLocals :: Signature a -> Body a -> Body a
dropResolvedReturnLocals sig =
  dropLeadingReturnLocals namedReturnCount
  where
    namedReturnCount =
      length
        [ ()
        | returnItem <- sigReturnItems sig,
          signatureReturnItemName returnItem /= Nothing
        ]

    dropLeadingReturnLocals 0 body = body
    dropLeadingReturnLocals count (Let _ _ _ Nothing : body) =
      dropLeadingReturnLocals (count - 1) body
    dropLeadingReturnLocals _ body = body

pprResolvedFunctionModifiers :: Maybe FunctionVisibility -> Signature a -> Doc
pprResolvedFunctionModifiers fallbackVisibility sig =
  hsep (map pprResolvedFunctionModifier modifiers)
  where
    modifiers
      | Just _ <- sigVisibility sig = sigModifiers sig
      | Just visibility <- fallbackVisibility =
          VisibilityModifier visibility : sigModifiers sig
      | otherwise = sigModifiers sig

pprResolvedFunctionModifier :: FunctionModifier -> Doc
pprResolvedFunctionModifier (VisibilityModifier VisibilityPublic) = text "public"
pprResolvedFunctionModifier (VisibilityModifier VisibilityExternal) = text "external"
pprResolvedFunctionModifier (VisibilityModifier VisibilityInternal) = text "internal"
pprResolvedFunctionModifier (VisibilityModifier VisibilityPrivate) = text "private"
pprResolvedFunctionModifier (MutabilityModifier MutabilityPure) = text "pure"
pprResolvedFunctionModifier (MutabilityModifier MutabilityView) = text "view"
pprResolvedFunctionModifier (MutabilityModifier MutabilityPayable) = text "payable"

pprPayable :: Bool -> Doc
pprPayable True = text "payable"
pprPayable False = empty

pprRetTy :: Bool -> Maybe Ty -> Doc
pprRetTy _ Nothing = empty
pprRetTy True (Just t) =
  text "returns" <+> parens (text "comptime" <+> ppr t)
pprRetTy False (Just t) =
  text "returns" <+> parens (pprReturnItems t)

pprReturnItems :: Ty -> Doc
pprReturnItems t@(TyCon n _)
  | isTuple n = commaSep (map ppr (tupleElements t))
pprReturnItems t = ppr t

pprParams :: (Pretty a) => [Param a] -> Doc
pprParams = parens . commaSep . map ppr

pprConst :: Bool -> Doc
pprConst True = text "comptime "
pprConst False = empty

pprComptime :: Bool -> Doc
pprComptime True = text "comptime"
pprComptime False = empty

instance (Pretty a) => Pretty (Param a) where
  ppr (Typed c n ty) =
    pprConst c <> ((ppr n <> colon) <+> ppr ty)
  ppr (Untyped c n) =
    pprConst c <> ppr n

instance (Pretty a) => Pretty (Stmt a) where
  ppr (n := e) =
    ppr n <+> equals <+> (ppr e <> semi)
  ppr (Let c n ty m) =
    ( text "let"
        <+> pprComptime c
        <+> (ppr n <> pprOptTy ty)
    )
      <> pprInitOpt m
  ppr (LetPattern ct pat ty value) =
    (text "let" <+> pprComptime ct <+> (ppr pat <> pprOptTy ty))
      <> pprInitOpt (Just value)
  ppr (Block body) =
    lbrace
      $$ nest 3 (ppr body)
      $$ rbrace
  ppr (StmtExp e)
    | isBareRevert e = text "revert" <> semi
    | otherwise = ppr e <> semi
  ppr (Return e)
    | isUnitExp e = text "return" <> semi
    | otherwise = text "return" <+> (ppr e <> semi)
  ppr (Match e eqns) =
    text "match"
      <+> (parens $ commaSep $ map ppr e)
      <+> lbrace
      $$ nest 3 (vcat (map ppr eqns))
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
  ppr (For initStmt cond postStmt body) =
    text "for"
      <+> parens (hsep [pprForClause initStmt <> semi, ppr cond <> semi, pprForClause postStmt])
      <+> lbrace
      $$ nest 3 (ppr body)
      $$ rbrace
  ppr Break = text "break" <> semi
  ppr Continue = text "continue" <> semi
  ppr EmptyStmt = empty

pprForClause :: (Pretty a) => Stmt a -> Doc
pprForClause (n := e) = ppr n <+> equals <+> ppr e
pprForClause (Let ct n ty m) =
  text "let"
    <+> pprComptime ct
    <+> (ppr n <> pprOptTy ty)
    <+> pprForInitOpt m
pprForClause (LetPattern ct pat ty value) =
  text "let"
    <+> pprComptime ct
    <+> (ppr pat <> pprOptTy ty)
    <+> pprForInitOpt (Just value)
pprForClause (StmtExp e) = ppr e
pprForClause (Block stmts) = hsep (punctuate comma (map pprForClause stmts))
pprForClause EmptyStmt = empty
pprForClause s = ppr s

pprForInitOpt :: (Pretty a) => Maybe (Exp a) -> Doc
pprForInitOpt Nothing = empty
pprForInitOpt (Just e) = equals <+> ppr e

instance (Pretty a) => Pretty (Equation a) where
  ppr (ps, ss)
    | not (null ps) && all isWildcardPat ps =
        text "default"
          <+> lbrace
          $$ nest 3 (vcat (map ppr ss))
          $$ rbrace
    | otherwise =
        text "case"
          <+> pprCasePatterns ps
          <+> lbrace
          $$ nest 3 (vcat (map ppr ss))
          $$ rbrace

instance (Pretty a) => Pretty (Equations a) where
  ppr = vcat . map ppr

pprCasePatterns :: (Pretty a) => [Pat a] -> Doc
pprCasePatterns [pat] = ppr pat
pprCasePatterns pats = parens (commaSep (map ppr pats))

isWildcardPat :: Pat a -> Bool
isWildcardPat PWildcard = True
isWildcardPat _ = False

pprOptTy :: Maybe Ty -> Doc
pprOptTy Nothing = empty
pprOptTy (Just t)
  | isVar t = empty
  | otherwise = colon <+> ppr t

isVar :: Ty -> Bool
isVar (TyVar _) = True
isVar _ = False

pprInitOpt :: (Pretty a) => Maybe (Exp a) -> Doc
pprInitOpt Nothing = semi
pprInitOpt (Just e) =
  space <> (equals <+> ppr e) <> semi

parensWhen :: Bool -> Doc -> Doc
parensWhen True d = parens d
parensWhen _ d = d

instance (Pretty a) => Pretty (Exp a) where
  ppr = pprTypedExpPrec lowestTypedExpPrec

lowestTypedExpPrec, ternaryTypedExpPrec, castTypedExpPrec :: Int
postfixTypedExpPrec, atomTypedExpPrec :: Int
lowestTypedExpPrec = 0
ternaryTypedExpPrec = 10
castTypedExpPrec = 110

postfixTypedExpPrec = 130

atomTypedExpPrec = 140

pprTypedExpPrec :: (Pretty a) => Int -> Exp a -> Doc
pprTypedExpPrec context expression =
  parensWhen
    (typedExpPrecedence expression < context)
    (pprTypedExpNode expression)

pprTypedExpNode :: (Pretty a) => Exp a -> Doc
pprTypedExpNode (Var v) = ppr v
pprTypedExpNode expression@(Con n [_, _])
  | isTuple n =
      parens
        ( commaSep
            (map (pprTypedExpPrec lowestTypedExpPrec) (typedTupleExpElements expression))
        )
pprTypedExpNode (Con n [])
  | isUnitConstructorName n = text "()"
  | otherwise = ppr n
pprTypedExpNode (Con n es) =
  ppr n
    <> parens
      (nest 1 $ commaSep $ map (pprTypedExpPrec lowestTypedExpPrec) es)
pprTypedExpNode (Lit l) = ppr l
pprTypedExpNode (Call Nothing n es) =
  ppr n
    <> parens
      (nest 1 $ commaSep $ map (pprTypedExpPrec lowestTypedExpPrec) es)
pprTypedExpNode (Call (Just receiver) n es) =
  pprTypedExpPrec postfixTypedExpPrec receiver
    <> char '.'
    <> ppr n
    <> parens
      (nest 1 $ commaSep $ map (pprTypedExpPrec lowestTypedExpPrec) es)
pprTypedExpNode (Lam args bd lambdaRetTy) =
  (text "lam" <> pprParams args)
    <+> pprRetTy False lambdaRetTy
    <+> lbrace
    $$ nest 3 (vcat (map ppr bd))
    $$ rbrace
pprTypedExpNode (TyExp e ty) =
  pprTypedExpPrec castTypedExpPrec e <+> text "as" <+> ppr ty
pprTypedExpNode (FieldAccess Nothing n) =
  ppr n
pprTypedExpNode (FieldAccess (Just receiver) n) =
  pprTypedExpPrec postfixTypedExpPrec receiver <> char '.' <> ppr n
pprTypedExpNode (Cond condition thenExpression elseExpression) =
  hsep
    [ pprTypedExpPrec (ternaryTypedExpPrec + 1) condition,
      text "?",
      pprTypedExpPrec ternaryTypedExpPrec thenExpression,
      colon,
      pprTypedExpPrec ternaryTypedExpPrec elseExpression
    ]
pprTypedExpNode (Indexed collection index) =
  pprTypedExpPrec postfixTypedExpPrec collection
    <> brackets (pprTypedExpPrec lowestTypedExpPrec index)

-- ppr e = text $ "Pretty.ppr not implemented for\n" ++ show(pShow e)

typedExpPrecedence :: Exp a -> Int
typedExpPrecedence (Cond _ _ _) = ternaryTypedExpPrec
typedExpPrecedence (TyExp _ _) = castTypedExpPrec
typedExpPrecedence (Call (Just _) _ _) = postfixTypedExpPrec
typedExpPrecedence (FieldAccess _ _) = postfixTypedExpPrec
typedExpPrecedence (Indexed _ _) = postfixTypedExpPrec
typedExpPrecedence _ = atomTypedExpPrec

typedTupleExpElements :: (Pretty a) => Exp a -> [Exp a]
typedTupleExpElements (Con n [left, right])
  | isTuple n = left : typedTupleExpElements right
typedTupleExpElements expression = [expression]

isUnitConstructorName :: (Pretty a) => a -> Bool
isUnitConstructorName constructorName =
  rendered == "()"
    || rendered == "unit"
    || "()<" `isPrefixOf` rendered
    || "unit<" `isPrefixOf` rendered
  where
    rendered = pretty constructorName

instance (Pretty a) => Pretty (Pat a) where
  ppr (PVar n) =
    ppr n
  ppr (PCon n []) = ppr n
  ppr (PCon n ps@(_ : _))
    | isTuple n = parens (commaSep $ map ppr ps)
    | otherwise = ppr n <> (parens $ commaSep $ map ppr ps)
  ppr PWildcard =
    text "_"
  ppr (PLit l) =
    ppr l
  ppr (PExp e) =
    text "comptime" <+> ppr e

instance Pretty Literal where
  ppr (IntLit l) = integer (toInteger l)
  ppr (StrLit l) = pprStringLiteral l

pprStringLiteral :: String -> Doc
pprStringLiteral = doubleQuotes . text . concatMap escapeStringChar

escapeStringChar :: Char -> String
escapeStringChar '\\' = "\\\\"
escapeStringChar '"' = "\\\""
escapeStringChar '\n' = "\\n"
escapeStringChar '\t' = "\\t"
escapeStringChar c = [c]

instance Pretty Tyvar where
  ppr (TVar n) = ppr n
  ppr (Skolem n) = text "@" <> ppr n

instance Pretty Pred where
  ppr (InCls n t ts) =
    (ppr t <> colon) <+> (ppr n <> pprTyParams ts)
  ppr (t1 :~: t2) =
    ppr t1 <+> text "~" <+> ppr t2

instance Pretty Scheme where
  ppr (Forall vs ty) = ppr' (Forall vs ty)
    where
      ppr' (Forall [] ([] :=> t)) = ppr t
      ppr' (Forall [] (ctx :=> t)) =
        pprContext ctx <+> ppr t
      ppr' (Forall vars (ctx :=> t)) =
        text "forall"
          <+> hsep (map ppr vars)
          <+> text "."
          <+> pprContext ctx
          <+> ppr t

instance Pretty MetaTv where
  ppr (MetaTv v) = text "?" <> ppr v

instance Pretty Ty where
  ppr (TyVar v) = ppr v
  ppr (Meta v) = ppr v
  ppr t@(_ :-> _) =
    let (args, ret) = splitTy t
     in (text "function" <> parens (commaSep (map ppr args)))
          <+> text "internal"
          <+> pprRetTy False (Just ret)
  ppr (TyCon n [keyTy, valueTy])
    | n == Name "mapping" =
        text "mapping"
          <> parens (ppr keyTy <+> text "=>" <+> ppr valueTy)
  ppr (TyCon n [elementTy])
    | n == Name "array" =
        ppr elementTy <> brackets empty
  ppr (TyCon n [sizeTy, elementTy])
    | n == Name "array" =
        ppr elementTy <> brackets (ppr sizeTy)
  ppr (TyCon n [t])
    | isDataLocation n = ppr t <+> ppr n
  ppr t@(TyCon n _)
    | isTuple n = parens $ commaSep (map ppr (tupleElements t))
    | isUnit n = text "()"
  ppr (TyCon n ts) =
    ppr n <> pprTyParams ts

isUnit :: Name -> Bool
isUnit n =
  n == Name "unit" || n == Name "()"

isTuple :: (Pretty a) => a -> Bool
isTuple s = pretty s == "pair"

isDataLocation :: Name -> Bool
isDataLocation n =
  n `elem` [Name "memory", Name "storage", Name "calldata"]

tupleElements :: Ty -> [Ty]
tupleElements (TyCon n [left, right])
  | isTuple n = left : tupleElements right
tupleElements t = [t]

pprTyParams :: [Ty] -> Doc
pprTyParams [] = empty
pprTyParams ts =
  angles (commaSep (map ppr ts))

constructorLeafName :: Name -> Name
constructorLeafName (QualName _ leaf) = Name leaf
constructorLeafName n = n

isUnitExp :: (Pretty a) => Exp a -> Bool
isUnitExp (Con n []) =
  rendered == "()"
    || rendered == "unit"
    || "()<" `isPrefixOf` rendered
    || "unit<" `isPrefixOf` rendered
  where
    rendered = pretty n
isUnitExp _ = False

isBareRevert :: (Pretty a) => Exp a -> Bool
isBareRevert (Call Nothing n []) =
  rendered == "revert" || "revert<" `isPrefixOf` rendered
  where
    rendered = pretty n
isBareRevert _ = False

instance Pretty Subst where
  ppr = braces . commaSep . map go . Map.toList . unSubst
    where
      go (v, t) = ppr v <+> text "+->" <+> ppr t

instance Pretty Id where
  ppr (Id n t) = ppr n <> text "<" <> ppr t <> text ">"
