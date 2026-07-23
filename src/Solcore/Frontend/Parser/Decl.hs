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
  ( paramP,
    qualifiedName,
    simpleNameP,
    typeParamsP,
    typeP,
    whereClauseP,
  )
import Solcore.Frontend.Parser.Stmt (bodyP)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

-- Top-level entry point

compUnitP :: Parser CompUnit
compUnitP = do
  sc
  items <- many (Left <$> try importP <|> Right <$> topDeclP)
  eof
  return $ CompUnit [i | Left i <- items] [d | Right d <- items]

expP :: Parser Exp
expP = exprP bodyP

importP :: Parser Import
importP = do
  keyword "import"
  choice
    [ try $ do
        _ <- symbol "*"
        keyword "as"
        aliasName <- simpleNameP
        keyword "from"
        path <- importPathP
        _ <- semicolon
        pure (ImportAlias path aliasName),
      try $ do
        entries <- braces (itemEntryP `sepBy` comma)
        keyword "from"
        path <- importPathP
        hiddenNames <- option [] hidingP
        _ <- semicolon
        pure (ImportOnly path (SelectItems entries hiddenNames)),
      do
        path <- importPathP
        ImportModule path <$ semicolon
    ]
  where
    hidingP = keyword "hiding" *> braces (simpleNameP `sepBy` comma)

importPathP :: Parser ModulePath
importPathP = try externalPathP <|> modulePathP

modulePathP :: Parser ModulePath
modulePathP = do
  h <- identifier
  ts <- many (try (char '.' *> identifier))
  return (classifyModulePath (foldl QualName (Name h) ts))

externalPathP :: Parser ModulePath
externalPathP = do
  lib <- symbol "@" *> identifier <* char '.'
  sc
  h <- identifier
  ts <- many (try (char '.' *> identifier))
  return (ExternalPath (Name lib) (foldl QualName (Name h) ts))

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

itemEntryP :: Parser ItemSelectorEntry
itemEntryP =
  SelectAllItems
    <$ symbol "*"
      <|> try (SelectItemAs <$> simpleNameP <* keyword "as" <*> simpleNameP)
      <|> SelectItem
    <$> simpleNameP

exportP :: Parser Export
exportP = do
  keyword "export"
  choice
    [ ExportList <$> braces (exportSpecP `sepBy` comma) <* semicolon,
      externalPathP >>= exportTailP,
      modulePathP >>= exportTailP
    ]

exportTailP :: ModulePath -> Parser Export
exportTailP path =
  choice
    [ symbol "." *> dotExportP,
      keyword "as" *> (ExportModuleAs path <$> simpleNameP) <* semicolon,
      ExportModule path <$ semicolon
    ]
  where
    dotExportP = ExportItemsFrom path . SelectExportItems <$> itemsP <* semicolon
    itemsP =
      braces (exportSelEntryP `sepBy` comma)
        <|> [SelectExportAllItems]
        <$ symbol "*"

exportSpecP :: Parser ExportSpec
exportSpecP =
  ExportAll
    <$ symbol "*"
      <|> ExportModuleAll
    <$> try moduleAllPathP
      <|> do
        n <- simpleNameP
        mSel <- optional (parens constrSelectorP)
        return $ case mSel of
          Nothing -> ExportName n
          Just sel -> ExportNameWithConstructors n sel
  where
    moduleAllPathP =
      (externalPathP <|> classifyModulePath <$> moduleNameP)
        <* symbol "."
        <* symbol "*"

moduleNameP :: Parser Name
moduleNameP = do
  h <- identifier
  ts <- many (try (char '.' *> notFollowedBy (char '*' <|> char '{') *> identifier))
  return (foldl QualName (Name h) ts)

exportSelEntryP :: Parser ExportSelectorEntry
exportSelEntryP =
  SelectExportAllItems
    <$ symbol "*"
      <|> do
        n <- simpleNameP
        mSel <- optional (parens constrSelectorP)
        return $ case mSel of
          Nothing -> SelectExportItem n
          Just sel -> SelectExportConstructors n sel

constrSelectorP :: Parser ConstructorSelector
constrSelectorP =
  SelectAllConstructors
    <$ symbol "*"
      <|> SelectConstructors
    <$> (simpleNameP `sepBy1` comma)

pragmaP :: Parser Pragma
pragmaP = do
  keyword "pragma"
  choice
    [ do
        keyword "solcore"
        ty <- pragmaTypeP
        st <- pragmaStatusForP ty
        _ <- semicolon
        pure (Pragma ty st),
      externalPragmaP "solidity" SolidityPragma,
      externalPragmaP "abicoder" AbiCoderPragma
    ]

externalPragmaP :: String -> (String -> PragmaType) -> Parser Pragma
externalPragmaP namespace pragmaConstructor = do
  keyword namespace
  pragmaValue <- unwords . words <$> manyTill anySingle (char ';')
  sc
  pure (Pragma (pragmaConstructor pragmaValue) Enabled)

pragmaTypeP :: Parser PragmaType
pragmaTypeP =
  NoCoverageCondition
    <$ keyword "noCoverageCondition"
      <|> NoPattersonCondition
    <$ keyword "noPattersonCondition"
      <|> NoBoundVariableCondition
    <$ keyword "noBoundVariableCondition"
      <|> NoGenericInstanceFor
    <$ keyword "noGenericInstanceFor"

-- | Parse the pragma status.  For 'NoGenericInstanceFor' a non-empty list of
-- type names is mandatory; for all other pragma types the list is optional and
-- defaults to 'DisableAll'.
pragmaStatusForP :: PragmaType -> Parser PragmaStatus
pragmaStatusForP NoGenericInstanceFor = do
  names <- simpleNameP `sepBy1` comma
  return (DisableFor (NE.fromList names))
pragmaStatusForP _ = option DisableAll $ do
  names <- simpleNameP `sepBy1` comma
  return (DisableFor (NE.fromList names))

enumP :: Parser DataTy
enumP = do
  keyword "enum"
  n <- simpleNameP
  params <- typeParamsP
  cs <- braces (constrP `sepEndBy` comma)
  return (DataTy n params cs)

structP :: Parser DataTy
structP = do
  keyword "struct"
  n <- simpleNameP
  params <- typeParamsP
  fields <- braces (many structFieldP)
  pure
    ( StructTy
        n
        params
        (map fst fields)
        (map snd fields)
    )

structFieldP :: Parser (Name, Ty)
structFieldP = do
  fieldName' <- simpleNameP
  _ <- colon
  fieldType <- typeP
  _ <- semicolon
  pure (fieldName', fieldType)

constrP :: Parser Constr
constrP = do
  n <- simpleNameP
  args <- option [] (parens (typeP `sepBy1` comma))
  return (Constr n args)

tySymP :: Parser TySym
tySymP = do
  keyword "type"
  n <- simpleNameP
  params <- typeParamsP
  keyword "is"
  t <- typeP
  _ <- semicolon
  return (TySym n params t)

functionModifierP :: Parser FunctionModifier
functionModifierP =
  choice
    [ VisibilityModifier VisibilityPublic <$ keyword "public",
      VisibilityModifier VisibilityExternal <$ keyword "external",
      VisibilityModifier VisibilityInternal <$ keyword "internal",
      VisibilityModifier VisibilityPrivate <$ keyword "private",
      MutabilityModifier MutabilityPure <$ keyword "pure",
      MutabilityModifier MutabilityView <$ keyword "view",
      MutabilityModifier MutabilityPayable <$ keyword "payable"
    ]

parseFunctionModifiers :: Bool -> Parser (Bool, [FunctionModifier])
parseFunctionModifiers allowContractModifiers = do
  modifiers <- many functionModifierP
  let visibility = [v | VisibilityModifier v <- modifiers]
      mutability = [m | MutabilityModifier m <- modifiers]
      isPublic =
        any
          (`elem` [VisibilityPublic, VisibilityExternal])
          visibility
      isPayable = MutabilityPayable `elem` mutability
  when (length visibility > 1) $
    fail "a function may declare at most one visibility modifier"
  when (length mutability > 1) $
    fail "a function may declare at most one mutability modifier"
  when (not allowContractModifiers && (not (null visibility) || isPayable)) $
    fail "visibility and `payable` modifiers are only allowed on contract functions"
  pure (isPublic, modifiers)

funDefP :: Parser FunDef
funDefP = funDefWithModifiers False

funDefWithModifiers :: Bool -> Parser FunDef
funDefWithModifiers allowContractModifiers = do
  (isPublic, sig) <- signatureP allowContractModifiers
  body <- braces bodyP
  pure (FunDef isPublic sig body)

signatureP :: Bool -> Parser (Bool, Signature)
signatureP allowContractModifiers = do
  keyword "function"
  n <- simpleNameP
  vars <- typeParamsP
  ps <- parens (paramP `sepBy` comma)
  (isPublic, modifiers) <- parseFunctionModifiers allowContractModifiers
  returnItems <- optional returnsClauseP
  ctx <- whereClauseP
  pure
    ( isPublic,
      SignatureWithSyntax vars ctx n ps returnItems modifiers
    )

returnsClauseP :: Parser [ReturnItem]
returnsClauseP = do
  keyword "returns"
  parens (returnItemP `sepBy` comma)

returnItemP :: Parser ReturnItem
returnItemP = do
  isComptime <- option False (True <$ keyword "comptime")
  returnName <- optional (try (simpleNameP <* colon))
  ReturnItem isComptime returnName <$> typeP

fallbackDefP :: Parser FunDef
fallbackDefP = do
  keyword "fallback"
  ps <- parens (paramP `sepBy` comma)
  when (not (null ps)) $
    fail "fallback function must not declare input parameters"
  modifiers <- many functionModifierP
  let visibility = [v | VisibilityModifier v <- modifiers]
      mutability = [m | MutabilityModifier m <- modifiers]
  when (visibility /= [VisibilityExternal]) $
    fail "fallback must declare exactly one `external` visibility modifier"
  when (length mutability > 1 || any (`elem` [MutabilityPure, MutabilityView]) mutability) $
    fail "fallback only supports the `payable` mutability modifier"
  body <- braces bodyP
  let sig =
        SignatureWithSyntax
          []
          []
          (Name "fallback")
          []
          Nothing
          modifiers
  pure (FunDef False sig body)

traitSignatureP :: Parser Signature
traitSignatureP = do
  (isPublic, sig) <- signatureP False
  when isPublic $
    fail "trait methods cannot have contract visibility"
  _ <- semicolon <?> "';' after function signature"
  pure sig

traitP :: Parser Class
traitP = do
  keyword "trait"
  traitName <- qualifiedName
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
  implName <- qualifiedName
  args <- between (symbol "<") (symbol ">") (typeP `sepBy1` comma)
  (primaryTy, params) <- case args of
    [] -> fail "an impl must supply at least one trait type argument"
    mainArg : extraArgs -> pure (mainArg, extraArgs)
  ctx <- whereClauseP
  funs <- braces (many funDefP)
  pure (Instance isDefault vars ctx implName params primaryTy funs)

contractP :: Parser Contract
contractP = do
  keyword "contract"
  n <- simpleNameP
  params <- typeParamsP
  ds <- braces (many contractDeclP)
  return (ContractShell ContractKind n params ds)

interfaceP :: Parser Contract
interfaceP = do
  keyword "interface"
  n <- simpleNameP
  params <- typeParamsP
  ds <- braces (many interfaceDeclP)
  return (ContractShell InterfaceKind n params ds)

libraryP :: Parser Contract
libraryP = do
  keyword "library"
  n <- simpleNameP
  params <- typeParamsP
  ds <- braces (many libraryDeclP)
  return (ContractShell LibraryKind n params ds)

contractDeclP :: Parser ContractDecl
contractDeclP =
  CDataDecl
    <$> (try structP <|> enumP)
      <|> CConstrDecl
    <$> try constructorDeclP
      <|> CFunDecl
    <$> try fallbackDefP
      <|> CFunDecl
    <$> try (funDefWithModifiers True)
      <|> CFieldDecl
    <$> fieldDeclP

interfaceDeclP :: Parser ContractDecl
interfaceDeclP = do
  (isPublic, sig) <- signatureP True
  _ <- semicolon <?> "';' after interface function signature"
  pure (CSignatureDecl isPublic sig)

libraryDeclP :: Parser ContractDecl
libraryDeclP =
  CDataDecl
    <$> (try structP <|> enumP)
      <|> CFunDecl
    <$> try (funDefWithModifiers True)
      <|> CFieldDecl
    <$> fieldDeclP

fieldDeclP :: Parser Field
fieldDeclP = do
  n <- simpleNameP
  _ <- colon
  ty <- typeP
  me <- optional (equalsP *> expP)
  _ <- semicolon
  return (Field n ty me)

constructorDeclP :: Parser Constructor
constructorDeclP = do
  keyword "constructor"
  ps <- parens (paramP `sepBy` comma)
  modifiers <- many functionModifierP
  when (any (/= MutabilityModifier MutabilityPayable) modifiers || length modifiers > 1) $
    fail "constructor only supports the `payable` modifier"
  body <- braces bodyP
  return (Constructor ps body (MutabilityModifier MutabilityPayable `elem` modifiers))

topDeclP :: Parser TopDecl
topDeclP =
  choice
    [ TPragmaDecl <$> pragmaP,
      TExportDecl <$> exportP,
      TDataDef <$> structP,
      TDataDef <$> enumP,
      TSym <$> tySymP,
      TContr <$> (contractP <|> interfaceP <|> libraryP),
      contractOnlyDeclP,
      TFunDef <$> try funDefP,
      TClassDef <$> try traitP,
      TInstDef <$> implP
    ]

-- | @constructor@ and @fallback@ declarations are only meaningful inside a
-- @contract@. Catch them at the top level so we report a clear error instead
-- of a confusing generic parse failure. Each branch commits (consumes the
-- keyword) before failing, so the surrounding 'choice' does not fall through
-- to the function/class/instance parser.
contractOnlyDeclP :: Parser TopDecl
contractOnlyDeclP =
  keyword "constructor"
    *> fail "a `constructor` may only be declared inside a contract"
      <|> keyword "fallback"
    *> fail "a `fallback` may only be declared inside a contract"

equalsP :: Parser ()
equalsP = void $ try (lexeme (char '=' <* notFollowedBy (char '=')))
