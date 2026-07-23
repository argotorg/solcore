{-# OPTIONS_GHC -Wno-orphans #-}

module Solcore.Frontend.Pretty.TreePretty (pretty, isTuple) where

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

instance Pretty TopDecl where
  ppr (TContr c) = ppr c
  ppr (TFunDef fd) = ppr fd
  ppr (TClassDef c) = ppr c
  ppr (TInstDef is) = ppr is
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

instance Pretty Contract where
  ppr (ContractShell kind n ts ds) =
    pprContractKind kind
      <+> (ppr n <> pprTyParams ts)
      <+> lbrace
      $$ nest 3 (vcat (map ppr ds))
      $$ rbrace

pprContractKind :: ContractKind -> Doc
pprContractKind ContractKind = text "contract"
pprContractKind InterfaceKind = text "interface"
pprContractKind LibraryKind = text "library"

instance Pretty ContractDecl where
  ppr (CDataDecl dt) =
    ppr dt
  ppr (CFieldDecl fd) =
    ppr fd
  ppr (CFunDecl fd) =
    ppr fd
  ppr (CSignatureDecl isPublic sig) =
    pprInterfaceSignature isPublic sig <> semi
  ppr (CConstrDecl c) =
    ppr c

instance Pretty Constructor where
  ppr (Constructor ps bd payable) =
    (text "constructor" <> pprParams ps)
      <+> pprPayable payable
      <+> lbrace
      $$ nest 3 (vcat (map ppr bd))
      $$ rbrace

instance Pretty DataTy where
  ppr (StructTy n ps fieldNames fieldTypes) =
    text "struct"
      <+> (ppr n <> pprTyParams ps)
      <+> lbrace
      $$ nest 3 (vcat (zipWith pprStructField fieldNames fieldTypes))
      $$ rbrace
  ppr (DataTy n ps cs) =
    text "enum"
      <+> (ppr n <> pprTyParams ps)
      <+> lbrace
      $$ nest 3 (vcat (punctuate comma (map ppr cs)))
      $$ rbrace

pprStructField :: Name -> Ty -> Doc
pprStructField fieldName' fieldType =
  ((ppr fieldName' <> colon) <+> ppr fieldType) <> semi

instance Pretty TySym where
  ppr (TySym n vs t) =
    ( text "alias"
        <+> (ppr n <> pprTyParams vs)
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

instance Pretty Class where
  ppr (Class _ ps n vs v sigs) =
    text "trait"
      <+> (ppr n <> pprTyParams (v : vs))
      <+> pprWhere ps
      <+> lbrace
      $$ nest 3 (pprSignatures sigs)
      $$ rbrace

pprSignatures :: [Signature] -> Doc
pprSignatures =
  vcat . map ((<> semi) . ppr)

instance Pretty Signature where
  ppr = pprSignature False

instance Pretty Instance where
  ppr (Instance d vs ctx n tys ty funs) =
    (pprDefault d <> text "impl" <> pprTyParams vs)
      <+> (ppr n <> pprTyParams (ty : tys))
      <+> pprWhere ctx
      <+> lbrace
      $$ nest 3 (pprFunBlock funs)
      $$ rbrace

pprDefault :: Bool -> Doc
pprDefault b = if b then text "default " else empty

pprWhere :: [Pred] -> Doc
pprWhere [] = empty
pprWhere ps =
  text "where" <+> commaSep (map ppr ps)

instance Pretty [Pred] where
  ppr = parens . commaSepList

pprFunBlock :: [FunDef] -> Doc
pprFunBlock =
  vcat . map ppr

instance Pretty Field where
  ppr (Field n ty e) =
    ((ppr n <> colon) <+> ppr ty) <> pprInitOpt e

instance Pretty Body where
  ppr = vcat . map ppr

instance Pretty FunDef where
  ppr (FunDef isPub sig bd) =
    pprSignature isPub sig
      <+> lbrace
      $$ nest 3 (vcat (map ppr bd))
      $$ rbrace

pprSignature :: Bool -> Signature -> Doc
pprSignature isPub (SignatureWithSyntax vs ctx n ps returnItems modifiers)
  | n == Name "fallback" =
      (text "fallback" <> pprParams ps)
        <+> pprFunctionModifiers
          (ensureVisibility VisibilityExternal modifiers)
  | otherwise =
      text "function"
        <+> (ppr n <> pprTyParams vs <> pprParams ps)
        <+> pprFunctionModifiers
          ( if isPub
              then ensureVisibility VisibilityPublic modifiers
              else modifiers
          )
        <+> pprSignatureReturns returnItems
        <+> pprWhere ctx

pprInterfaceSignature :: Bool -> Signature -> Doc
pprInterfaceSignature isExternal (SignatureWithSyntax vs ctx n ps returnItems modifiers) =
  text "function"
    <+> (ppr n <> pprTyParams vs <> pprParams ps)
    <+> pprFunctionModifiers
      ( if isExternal
          then ensureVisibility VisibilityExternal modifiers
          else modifiers
      )
    <+> pprSignatureReturns returnItems
    <+> pprWhere ctx

pprFunctionModifiers :: [FunctionModifier] -> Doc
pprFunctionModifiers = hsep . map pprFunctionModifier

pprFunctionModifier :: FunctionModifier -> Doc
pprFunctionModifier (VisibilityModifier VisibilityPublic) = text "public"
pprFunctionModifier (VisibilityModifier VisibilityExternal) = text "external"
pprFunctionModifier (VisibilityModifier VisibilityInternal) = text "internal"
pprFunctionModifier (VisibilityModifier VisibilityPrivate) = text "private"
pprFunctionModifier (MutabilityModifier MutabilityPure) = text "pure"
pprFunctionModifier (MutabilityModifier MutabilityView) = text "view"
pprFunctionModifier (MutabilityModifier MutabilityPayable) = text "payable"

ensureVisibility :: FunctionVisibility -> [FunctionModifier] -> [FunctionModifier]
ensureVisibility fallbackVisibility modifiers
  | any isVisibilityModifier modifiers = modifiers
  | otherwise = VisibilityModifier fallbackVisibility : modifiers
  where
    isVisibilityModifier (VisibilityModifier _) = True
    isVisibilityModifier _ = False

pprSignatureReturns :: Maybe [ReturnItem] -> Doc
pprSignatureReturns Nothing = empty
pprSignatureReturns (Just items) =
  text "returns" <+> parens (commaSep (map pprReturnItem items))

pprReturnItem :: ReturnItem -> Doc
pprReturnItem (ReturnItem isComptime returnName returnTy) =
  pprConst isComptime
    <> case returnName of
      Nothing -> ppr returnTy
      Just n -> (ppr n <> colon) <+> ppr returnTy

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

pprParams :: [Param] -> Doc
pprParams = parens . commaSep . map ppr

pprConst :: Bool -> Doc
pprConst True = text "comptime "
pprConst False = empty

pprComptime :: Bool -> Doc
pprComptime True = text "comptime"
pprComptime False = empty

instance Pretty Param where
  ppr (Typed c n ty) =
    pprConst c <> ((ppr n <> colon) <+> ppr ty)
  ppr (Untyped c n) =
    pprConst c <> ppr n

instance Pretty Stmt where
  ppr (Assign n e) =
    ppr n <+> equals <+> (ppr e <> semi)
  ppr (StmtPlusEq e1 e2) =
    hsep [ppr e1, text "+=", ppr e2] <> semi
  ppr (StmtMinusEq e1 e2) =
    hsep [ppr e1, text "-=", ppr e2] <> semi
  ppr (StmtBXorEq e1 e2) =
    hsep [ppr e1, text "^=", ppr e2] <> semi
  ppr (StmtBAndEq e1 e2) =
    hsep [ppr e1, text "&=", ppr e2] <> semi
  ppr (StmtBOrEq e1 e2) =
    hsep [ppr e1, text "|=", ppr e2] <> semi
  ppr (StmtModEq e1 e2) =
    hsep [ppr e1, text "%=", ppr e2] <> semi
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
  ppr (Return e) = text "return" <+> (ppr e <> semi)
  ppr BareReturn = text "return" <> semi
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
  ppr (While cond body) =
    text "while"
      <+> parens (ppr cond)
      <+> lbrace
      $$ nest 3 (ppr body)
      $$ rbrace
  ppr (Unchecked body) =
    text "unchecked"
      <+> lbrace
      $$ nest 3 (ppr body)
      $$ rbrace
  ppr (For initStmt cond postStmt body) =
    text "for"
      <+> parens (hsep [pprForClause initStmt <> semi, ppr cond <> semi, pprForClause postStmt])
      <+> lbrace
      $$ nest 3 (ppr body)
      $$ rbrace
  ppr Break = text "break" <> semi
  ppr Continue = text "continue" <> semi
  ppr Revert = text "revert" <> semi
  ppr EmptyStmt = empty

pprForClause :: Stmt -> Doc
pprForClause (Assign n e) = ppr n <+> equals <+> ppr e
pprForClause (StmtPlusEq e1 e2) = hsep [ppr e1, text "+=", ppr e2]
pprForClause (StmtMinusEq e1 e2) = hsep [ppr e1, text "-=", ppr e2]
pprForClause (StmtBXorEq e1 e2) = hsep [ppr e1, text "^=", ppr e2]
pprForClause (StmtBAndEq e1 e2) = hsep [ppr e1, text "&=", ppr e2]
pprForClause (StmtBOrEq e1 e2) = hsep [ppr e1, text "|=", ppr e2]
pprForClause (StmtModEq e1 e2) = hsep [ppr e1, text "%=", ppr e2]
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
pprForClause (Block stmts) = commaSep (map pprForClause stmts)
pprForClause EmptyStmt = empty
pprForClause s = ppr s

pprForInitOpt :: Maybe Exp -> Doc
pprForInitOpt Nothing = empty
pprForInitOpt (Just e) = equals <+> ppr e

instance Pretty Equation where
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

instance Pretty Equations where
  ppr = vcat . map ppr

pprCasePatterns :: [Pat] -> Doc
pprCasePatterns [pat] = ppr pat
pprCasePatterns pats = parens (commaSep (map ppr pats))

isWildcardPat :: Pat -> Bool
isWildcardPat PWildcard = True
isWildcardPat _ = False

pprOptTy :: Maybe Ty -> Doc
pprOptTy Nothing = empty
pprOptTy (Just t) = colon <+> ppr t

pprInitOpt :: Maybe Exp -> Doc
pprInitOpt Nothing = semi
pprInitOpt (Just e) =
  space <> (equals <+> ppr e) <> semi

parensWhen :: Bool -> Doc -> Doc
parensWhen True d = parens d
parensWhen _ d = d

instance Pretty Exp where
  ppr = pprExpPrec lowestExpPrec

lowestExpPrec, ternaryExpPrec, logicalOrExpPrec, logicalAndExpPrec :: Int
equalityExpPrec, relationalExpPrec, bitOrExpPrec, bitXorExpPrec :: Int
bitAndExpPrec, shiftExpPrec, additiveExpPrec, multiplicativeExpPrec :: Int
powerExpPrec, castExpPrec :: Int
unaryExpPrec, postfixExpPrec, atomExpPrec :: Int
lowestExpPrec = 0
ternaryExpPrec = 10
logicalOrExpPrec = 20
logicalAndExpPrec = 30

equalityExpPrec = 40

relationalExpPrec = 50

bitOrExpPrec = 60

bitXorExpPrec = 70

bitAndExpPrec = 80

shiftExpPrec = 85

additiveExpPrec = 90

multiplicativeExpPrec = 100

powerExpPrec = 105

castExpPrec = 110

unaryExpPrec = 120

postfixExpPrec = 130

atomExpPrec = 140

pprExpPrec :: Int -> Exp -> Doc
pprExpPrec context expression =
  parensWhen
    (expPrecedence expression < context)
    (pprExpNode expression)

pprExpNode :: Exp -> Doc
pprExpNode (Lit l) = ppr l
pprExpNode expression@(ExpName Nothing n [_, _])
  | isTuple n =
      parens (commaSep (map (pprExpPrec lowestExpPrec) (tupleExpElements expression)))
pprExpNode (ExpName Nothing n [])
  | isUnit n = text "()"
  | otherwise = ppr n <> parens empty
pprExpNode (ExpName Nothing n es) =
  ppr n <> parens (commaSep (map (pprExpPrec lowestExpPrec) es))
pprExpNode (ExpName (Just receiver) n es) =
  pprExpPrec postfixExpPrec receiver
    <> char '.'
    <> ppr n
    <> parens (commaSep (map (pprExpPrec lowestExpPrec) es))
pprExpNode (ExpApply callee args) =
  pprExpPrec postfixExpPrec callee
    <> parens (commaSep (map (pprExpPrec lowestExpPrec) args))
pprExpNode (ExpVar Nothing v) = ppr v
pprExpNode (ExpVar (Just receiver) v) =
  pprExpPrec postfixExpPrec receiver <> char '.' <> ppr v
pprExpNode (ExpDotName n []) =
  char '.' <> ppr n
pprExpNode (ExpDotName n es) =
  char '.'
    <> ppr n
    <> parens (commaSep (map (pprExpPrec lowestExpPrec) es))
pprExpNode (Lam args bd lambdaRetTy) =
  (text "lam" <> pprParams args)
    <+> pprRetTy False lambdaRetTy
    <+> lbrace
    $$ nest 3 (vcat (map ppr bd))
    $$ rbrace
pprExpNode (TyExp e ty) =
  pprExpPrec castExpPrec e <+> text "as" <+> ppr ty
pprExpNode (ExpIndexed collection index) =
  pprExpPrec postfixExpPrec collection
    <> brackets (pprExpPrec lowestExpPrec index)
pprExpNode (ExpPlus left right) =
  pprLeftAssocBinary additiveExpPrec "+" left right
pprExpNode (ExpMinus left right) =
  pprLeftAssocBinary additiveExpPrec "-" left right
pprExpNode (ExpPower left right) =
  pprRightAssocBinary powerExpPrec "**" left right
pprExpNode (ExpTimes left right) =
  pprLeftAssocBinary multiplicativeExpPrec "*" left right
pprExpNode (ExpDivide left right) =
  pprLeftAssocBinary multiplicativeExpPrec "/" left right
pprExpNode (ExpModulo left right) =
  pprLeftAssocBinary multiplicativeExpPrec "%" left right
pprExpNode (ExpShiftL left right) =
  pprLeftAssocBinary shiftExpPrec "<<" left right
pprExpNode (ExpShiftR left right) =
  pprLeftAssocBinary shiftExpPrec ">>" left right
pprExpNode (ExpBXor left right) =
  pprLeftAssocBinary bitXorExpPrec "^" left right
pprExpNode (ExpBAnd left right) =
  pprLeftAssocBinary bitAndExpPrec "&" left right
pprExpNode (ExpBOr left right) =
  pprLeftAssocBinary bitOrExpPrec "|" left right
pprExpNode (ExpLT left right) =
  pprNonAssocBinary relationalExpPrec "<" left right
pprExpNode (ExpGT left right) =
  pprNonAssocBinary relationalExpPrec ">" left right
pprExpNode (ExpLE left right) =
  pprNonAssocBinary relationalExpPrec "<=" left right
pprExpNode (ExpGE left right) =
  pprNonAssocBinary relationalExpPrec ">=" left right
pprExpNode (ExpEE left right) =
  pprNonAssocBinary equalityExpPrec "==" left right
pprExpNode (ExpNE left right) =
  pprNonAssocBinary equalityExpPrec "!=" left right
pprExpNode (ExpLAnd left right) =
  pprLeftAssocBinary logicalAndExpPrec "&&" left right
pprExpNode (ExpLOr left right) =
  pprLeftAssocBinary logicalOrExpPrec "||" left right
pprExpNode (ExpLNot operand) =
  char '!' <> pprExpPrec unaryExpPrec operand
pprExpNode (ExpCond condition thenExpression elseExpression) =
  hsep
    [ pprExpPrec (ternaryExpPrec + 1) condition,
      text "?",
      pprExpPrec ternaryExpPrec thenExpression,
      colon,
      pprExpPrec ternaryExpPrec elseExpression
    ]
pprExpNode (ExpAt t) =
  text "Proxy"
    <+> text "as"
    <+> ppr (TyCon (Name "Proxy") [t])

pprLeftAssocBinary :: Int -> String -> Exp -> Exp -> Doc
pprLeftAssocBinary precedence operator left right =
  hsep
    [ pprExpPrec precedence left,
      text operator,
      pprExpPrec (precedence + 1) right
    ]

pprRightAssocBinary :: Int -> String -> Exp -> Exp -> Doc
pprRightAssocBinary precedence operator left right =
  hsep
    [ pprExpPrec (precedence + 1) left,
      text operator,
      pprExpPrec precedence right
    ]

pprNonAssocBinary :: Int -> String -> Exp -> Exp -> Doc
pprNonAssocBinary precedence operator left right =
  hsep
    [ pprExpPrec (precedence + 1) left,
      text operator,
      pprExpPrec (precedence + 1) right
    ]

expPrecedence :: Exp -> Int
expPrecedence (ExpCond _ _ _) = ternaryExpPrec
expPrecedence (ExpLOr _ _) = logicalOrExpPrec
expPrecedence (ExpLAnd _ _) = logicalAndExpPrec
expPrecedence (ExpEE _ _) = equalityExpPrec
expPrecedence (ExpNE _ _) = equalityExpPrec
expPrecedence (ExpLT _ _) = relationalExpPrec
expPrecedence (ExpGT _ _) = relationalExpPrec
expPrecedence (ExpLE _ _) = relationalExpPrec
expPrecedence (ExpGE _ _) = relationalExpPrec
expPrecedence (ExpBOr _ _) = bitOrExpPrec
expPrecedence (ExpBXor _ _) = bitXorExpPrec
expPrecedence (ExpBAnd _ _) = bitAndExpPrec
expPrecedence (ExpShiftL _ _) = shiftExpPrec
expPrecedence (ExpShiftR _ _) = shiftExpPrec
expPrecedence (ExpPlus _ _) = additiveExpPrec
expPrecedence (ExpMinus _ _) = additiveExpPrec
expPrecedence (ExpTimes _ _) = multiplicativeExpPrec
expPrecedence (ExpDivide _ _) = multiplicativeExpPrec
expPrecedence (ExpModulo _ _) = multiplicativeExpPrec
expPrecedence (ExpPower _ _) = powerExpPrec
expPrecedence (TyExp _ _) = castExpPrec
expPrecedence (ExpAt _) = castExpPrec
expPrecedence (ExpLNot _) = unaryExpPrec
expPrecedence (ExpName (Just _) _ _) = postfixExpPrec
expPrecedence (ExpApply _ _) = postfixExpPrec
expPrecedence (ExpVar (Just _) _) = postfixExpPrec
expPrecedence (ExpIndexed _ _) = postfixExpPrec
expPrecedence _ = atomExpPrec

tupleExpElements :: Exp -> [Exp]
tupleExpElements (ExpName Nothing n [left, right])
  | isTuple n = left : tupleExpElements right
tupleExpElements expression = [expression]

instance Pretty Pat where
  ppr (Pat n []) = ppr n
  ppr (Pat n ps@(_ : _))
    | isTuple n = parens (commaSep $ map ppr ps)
    | otherwise = ppr n <> (parens $ commaSep $ map ppr ps)
  ppr (PatDot n []) =
    char '.' <> ppr n
  ppr (PatDot n ps@(_ : _)) =
    char '.' <> ppr n <> (parens $ commaSep $ map ppr ps)
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

instance Pretty Pred where
  ppr (InCls n t ts) =
    (ppr t <> colon) <+> (ppr n <> pprTyParams ts)

instance Pretty Ty where
  ppr (FunctionTy args visibility returns) =
    (text "function" <> parens (commaSep (map ppr args)))
      <+> pprFunctionTypeVisibility visibility
      <+> pprFunctionTypeReturns returns
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

pprFunctionTypeVisibility :: Maybe FunctionTypeVisibility -> Doc
pprFunctionTypeVisibility Nothing = empty
pprFunctionTypeVisibility (Just FunctionTypeInternal) = text "internal"
pprFunctionTypeVisibility (Just FunctionTypeExternal) = text "external"

pprFunctionTypeReturns :: Maybe [Ty] -> Doc
pprFunctionTypeReturns Nothing = empty
pprFunctionTypeReturns (Just returns) =
  text "returns" <+> parens (commaSep (map ppr returns))

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

isBareRevert :: Exp -> Bool
isBareRevert (ExpName Nothing n []) = n == Name "revert"
isBareRevert _ = False

splitTy :: Ty -> ([Ty], Ty)
splitTy (a :-> b) =
  let (as, r) = splitTy b
   in (a : as, r)
splitTy t = ([], t)
