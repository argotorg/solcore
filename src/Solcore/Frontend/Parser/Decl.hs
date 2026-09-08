module Solcore.Frontend.Parser.Decl
  ( compUnitP,
    topDeclP,
    importP,
  )
where

import Common.LightYear
import Control.Monad (void, when)
import Data.List.NonEmpty qualified as NE
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.Expr (exprP)
import Solcore.Frontend.Parser.SolcoreTypes
  ( locatedP,
    namedParamP,
    qualifiedName,
    simpleNameP,
    typeP,
    typeParamsP,
    whereClauseP,
  )
import Solcore.Frontend.Parser.Stmt (bodyP, namedBodyP)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

compUnitP :: Parser CompUnit
compUnitP = do
  sc
  items <- many (Left <$> importP <|> Right <$> topDeclP)
  eof
  pure $ CompUnit [i | Left i <- items] [d | Right d <- items]

expP :: Parser Exp
expP = exprP bodyP

importP :: Parser Import
importP = do
  keyword "import"
  choice
    [ do
        _ <- symbol "*"
        aliasName <- optional (keyword "as" *> simpleNameP)
        keyword "from"
        path <- importPathP
        case aliasName of
          Just alias -> ImportAlias path alias <$ semicolon
          Nothing -> do
            hiddenNames <- option [] hidingP
            ImportOnly path (SelectItems [SelectAllItems] hiddenNames) <$ semicolon,
      do
        entries <- braces (itemEntryP `sepEndBy1` comma)
        keyword "from"
        path <- importPathP
        hiddenNames <- option [] hidingP
        ImportOnly path (SelectItems entries hiddenNames) <$ semicolon,
      ImportModule <$> importPathP <* semicolon
    ]
  where
    hidingP = keyword "hiding" *> braces (selectorNameP `sepEndBy1` comma)

importPathP :: Parser ModulePath
importPathP = externalPathP <|> modulePathP

modulePathP :: Parser ModulePath
modulePathP = classifyModulePath <$> moduleNameP

externalPathP :: Parser ModulePath
externalPathP = do
  _ <- symbol "@"
  lib <- simpleNameP
  rest <- many (try (symbol "." *> identifier))
  pure (ExternalPath lib (case rest of [] -> Name ""; _ -> mkQualName rest))

classifyModulePath :: Name -> ModulePath
classifyModulePath n = case splitQual n of
  ("lib" : rest@(_ : _)) -> LibraryPath (mkQualName rest)
  _ -> RelativePath n

splitQual :: Name -> [String]
splitQual (Name s) = [s]
splitQual (QualName n s) = splitQual n ++ [s]

mkQualName :: [String] -> Name
mkQualName [] = error "mkQualName: empty list"
mkQualName (x : xs) = foldl QualName (Name x) xs

moduleNameP :: Parser Name
moduleNameP = do
  h <- simpleNameP
  ts <- many (try (symbol "." *> identifier))
  pure (foldl QualName h ts)

-- Selectors may name an operator by enclosing its token sequence in parens.
-- Token boundaries are insignificant, so `( + = )` names the same operator
-- as `(+=)`, just as in the token-based reference parser.
selectorNameP :: Parser Name
selectorNameP = operatorNameP <|> simpleNameP

operatorNameP :: Parser Name
operatorNameP = locatedP locatedName $ do
  parts <- parens (some operatorPartP)
  pure (Name (concat parts))
  where
    operatorPartP =
      choice
        ( map
            (try . symbol)
            [ ":=",
              "->",
              "=>",
              "==",
              "!=",
              ">=",
              "<=",
              "&&",
              "||",
              "+=",
              "-=",
              "*=",
              "/=",
              "^=",
              "&=",
              "|=",
              "%=",
              "~=",
              "+",
              "-",
              "*",
              "/",
              "%",
              "!",
              "~",
              "<",
              ">",
              "=",
              "|",
              "&",
              "^",
              ":"
            ]
        )

itemEntryP :: Parser ItemSelectorEntry
itemEntryP = do
  n <- selectorNameP
  alias <- optional (keyword "as" *> simpleNameP)
  pure (maybe (SelectItem n) (SelectItemAs n) alias)

exportP :: Parser Export
exportP = do
  keyword "export"
  (ExportList <$> braces (exportSpecP `sepEndBy` comma) <* semicolon)
    <|> (modulePathP >>= exportTailP)

exportTailP :: ModulePath -> Parser Export
exportTailP path =
  choice
    [ do
        _ <- symbol "."
        entries <-
          [SelectExportAllItems]
            <$ symbol "*"
              <|> braces (exportSelEntryP `sepEndBy` comma)
        ExportItemsFrom path (SelectExportItems entries) <$ semicolon,
      keyword "as" *> (ExportModuleAs path <$> simpleNameP) <* semicolon,
      ExportModule path <$ semicolon
    ]

exportSpecP :: Parser ExportSpec
exportSpecP =
  choice
    [ ExportAll <$ symbol "*",
      try (ExportModuleAll <$> modulePathP <* symbol "." <* symbol "*"),
      ExportName <$> operatorNameP,
      do
        n <- simpleNameP
        constructors <- optional (parens constrSelectorP)
        pure (maybe (ExportName n) (ExportNameWithConstructors n) constructors)
    ]

exportSelEntryP :: Parser ExportSelectorEntry
exportSelEntryP =
  choice
    [ SelectExportAllItems <$ symbol "*",
      SelectExportItem <$> operatorNameP,
      do
        n <- simpleNameP
        constructors <- optional (parens constrSelectorP)
        pure (maybe (SelectExportItem n) (SelectExportConstructors n) constructors)
    ]

constrSelectorP :: Parser ConstructorSelector
constrSelectorP =
  SelectAllConstructors
    <$ symbol "*"
      <|> SelectConstructors
    <$> (simpleNameP `sepBy1` comma)

pragmaP :: Parser Pragma
pragmaP = do
  keyword "pragma"
  pragmaName <- pragmaIdentifier
  names <- simpleNameP `sepEndBy` comma
  _ <- semicolon
  let pragmaTy = case pragmaName of
        "no-coverage-condition" -> NoCoverageCondition
        "no-patterson-condition" -> NoPattersonCondition
        "no-bounded-variable-condition" -> NoBoundVariableCondition
        "no-generic-instance-for" -> NoGenericInstanceFor
        other -> CustomPragma other
      status = maybe DisableAll DisableFor (NE.nonEmpty names)
  pure (Pragma pragmaTy status)

dataDeclP :: Parser DataTy
dataDeclP = do
  derives <- option [] deriveP
  withDataDerives derives <$> (enumP <|> structP)

enumP :: Parser DataTy
enumP = do
  keyword "enum"
  n <- simpleNameP
  params <- typeParamsP
  cs <- braces (constrP `sepEndBy` comma)
  pure (DataTy n params cs)

-- Named product fields use the same type parameters and derive attributes as enums.
structP :: Parser DataTy
structP = do
  keyword "struct"
  n <- simpleNameP
  params <- typeParamsP
  fields <- braces (many structFieldP)
  let (names, tys) = unzip fields
  pure (StructTy n params names tys)
  where
    structFieldP = do
      n <- simpleNameP
      _ <- colon
      ty <- typeP
      _ <- semicolon
      pure (n, ty)

deriveP :: Parser [Name]
deriveP = do
  _ <- symbol "#"
  brackets $ do
    keyword "derive"
    parens (qualifiedName `sepBy1` comma)

constrP :: Parser Constr
constrP = do
  n <- simpleNameP
  args <- option [] (parens (typeP `sepBy` comma))
  pure (Constr n args)

tySymP :: Parser TySym
tySymP = do
  keyword "type"
  n <- simpleNameP
  params <- option [] (parens (tyVarP `sepEndBy` comma))
  equalsP
  t <- typeP
  TySym n params t <$ semicolon
  where
    tyVarP = locatedP locatedTy (flip TyCon [] <$> simpleNameP)

parseFunctionModifiers :: Bool -> Parser (Bool, [FunctionModifier])
parseFunctionModifiers allowContractModifiers = do
  isPublic <- option False (True <$ keyword "public")
  isPayable <- option False (True <$ keyword "payable")
  when (not allowContractModifiers && (isPublic || isPayable)) $
    fail "`public` and `payable` modifiers are only allowed on contract functions"
  pure
    ( isPublic,
      [VisibilityModifier VisibilityPublic | isPublic]
        ++ [MutabilityModifier MutabilityPayable | isPayable]
    )

funDefP :: Parser FunDef
funDefP = funDefWithModifiers False

funDefWithModifiers :: Bool -> Parser FunDef
funDefWithModifiers allowContractModifiers = do
  (isPublic, sig) <- signatureP allowContractModifiers
  body <- braces namedBodyP
  pure (FunDef isPublic sig body)

signatureP :: Bool -> Parser (Bool, Signature)
signatureP allowContractModifiers = do
  keyword "function"
  n <- simpleNameP
  vars <- typeParamsP
  ps <- parens (namedParamP `sepEndBy` comma)
  (isPublic, modifiers) <- parseFunctionModifiers allowContractModifiers
  returnItems <- optional returnsClauseP
  ctx <- whereClauseP
  pure (isPublic, SignatureWithSyntax vars ctx n ps returnItems modifiers)

returnsClauseP :: Parser [ReturnItem]
returnsClauseP = do
  keyword "returns"
  parens (returnItemP `sepEndBy` comma)
  where
    returnItemP = ReturnItem False Nothing <$> typeP

fallbackDefP :: Parser FunDef
fallbackDefP = do
  keyword "fallback"
  ps <- parens (namedParamP `sepEndBy` comma)
  when (not (null ps)) $
    fail "fallback function must not declare input parameters"
  isPayable <- option False (True <$ keyword "payable")
  body <- braces bodyP
  let modifiers = [MutabilityModifier MutabilityPayable | isPayable]
      sig = SignatureWithSyntax [] [] (Name "fallback") [] Nothing modifiers
  pure (FunDef False sig body)

traitSignatureP :: Parser Signature
traitSignatureP = do
  (_, sig) <- signatureP False
  sig <$ (semicolon <?> "';' after function signature")

traitP :: Parser Class
traitP = do
  keyword "trait"
  traitName <- simpleNameP
  vars <- typeParamsP
  (primaryVar, params) <- case vars of
    [] -> fail "a trait must declare at least one type parameter"
    primaryTy : extraParams -> pure (primaryTy, extraParams)
  ctx <- whereClauseP
  sigs <- braces (many traitSignatureP)
  pure (Class vars ctx traitName params primaryVar sigs)

implP :: Parser Instance
implP = do
  isDefault <- option False (True <$ keyword "default")
  keyword "impl"
  vars <- typeParamsP
  implName <- simpleNameP
  args <- between (symbol "<") (symbol ">") (typeP `sepEndBy1` comma)
  (primaryTy, params) <- case args of
    mainArg : extraArgs -> pure (mainArg, extraArgs)
    [] -> fail "an impl must supply at least one trait type argument"
  ctx <- whereClauseP
  funs <- braces (many funDefP)
  pure (Instance isDefault vars ctx implName params primaryTy funs)

contractP :: Parser Contract
contractP = do
  keyword "contract"
  n <- simpleNameP
  params <- typeParamsP
  ds <- braces (many contractDeclP)
  pure (Contract n params ds)

contractDeclP :: Parser ContractDecl
contractDeclP =
  choice
    [ CFieldDecl <$> try fieldDeclP,
      CDataDecl <$> dataDeclP,
      CSymDecl <$> tySymP,
      CConstrDecl <$> constructorDeclP,
      CFunDecl <$> fallbackDefP,
      CFunDecl <$> funDefWithModifiers True
    ]

fieldDeclP :: Parser Field
fieldDeclP = do
  n <- simpleNameP
  _ <- colon
  ty <- typeP
  me <- optional (equalsP *> expP)
  Field n ty me <$ semicolon

constructorDeclP :: Parser Constructor
constructorDeclP = do
  keyword "constructor"
  ps <- parens (namedParamP `sepEndBy` comma)
  isPayable <- option False (True <$ keyword "payable")
  body <- braces bodyP
  pure (Constructor ps body isPayable)

topDeclP :: Parser TopDecl
topDeclP =
  choice
    [ TPragmaDecl <$> pragmaP,
      TExportDecl <$> exportP,
      TDataDef <$> dataDeclP,
      TSym <$> tySymP,
      TContr <$> contractP,
      contractOnlyDeclP,
      TFunDef <$> funDefP,
      TClassDef <$> traitP,
      TInstDef <$> implP
    ]

contractOnlyDeclP :: Parser TopDecl
contractOnlyDeclP =
  keyword "constructor"
    *> fail "a `constructor` may only be declared inside a contract"
      <|> keyword "fallback"
    *> fail "a `fallback` may only be declared inside a contract"

equalsP :: Parser ()
equalsP = void $ try (lexeme (char '=' <* notFollowedBy (char '=')))
