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
  ( atomTypeP,
    paramP,
    qualifiedName,
    sigPrefixP,
    simpleNameP,
    typeP,
  )
import Solcore.Frontend.Parser.Stmt (bodyP)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree
import Solcore.Frontend.Syntax.Ty (StorageLocation (..))

-- Top-level entry point

compUnitP :: Parser CompUnit
compUnitP = do
  sc
  items <- many (Left <$> try importP <|> Right <$> topDeclP)
  eof
  return $ CompUnit [i | Left i <- items] [d | Right d <- items]

expP :: Parser Exp
expP = exprP bodyP

withSigPrefix :: ([Ty] -> [Pred] -> Parser a) -> Parser a
withSigPrefix k = do
  (vars, ctx) <- option ([], []) (try sigPrefixP)
  k vars ctx

importP :: Parser Import
importP = do
  keyword "import"
  choice
    [ do
        path <- externalPathP
        choice
          [ do
              _ <- symbol "."
              entries <- braces (itemEntryP `sepBy` comma)
              hids <- option [] hidingP <* semicolon
              return (ImportOnly path (SelectItems entries hids)),
            do
              keyword "as"
              n <- simpleNameP
              _ <- semicolon
              return (ImportAlias path n),
            ImportModule path <$ semicolon
          ],
      do
        path <- modulePathP
        choice
          [ do
              _ <- symbol "."
              entries <- braces (itemEntryP `sepBy` comma)
              hids <- option [] hidingP
              _ <- semicolon
              return (ImportOnly path (SelectItems entries hids)),
            do
              keyword "as"
              n <- simpleNameP
              _ <- semicolon
              return (ImportAlias path n),
            ImportModule path <$ semicolon
          ]
    ]
  where
    hidingP = keyword "hiding" *> braces (simpleNameP `sepBy` comma)

modulePathP :: Parser ModulePath
modulePathP = do
  h <- identifier
  ts <- many (try (char '.' *> notFollowedBy (char '{') *> identifier))
  return (classifyModulePath (foldl QualName (Name h) ts))

externalPathP :: Parser ModulePath
externalPathP = do
  lib <- symbol "@" *> identifier <* char '.'
  sc
  h <- identifier
  ts <- many (try (char '.' *> notFollowedBy (char '{') *> identifier))
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
  ty <- pragmaTypeP
  st <- pragmaStatusForP ty
  _ <- semicolon
  return (Pragma ty st)

pragmaTypeP :: Parser PragmaType
pragmaTypeP =
  NoCoverageCondition
    <$ keyword "no-coverage-condition"
      <|> NoPattersonCondition
    <$ keyword "no-patterson-condition"
      <|> NoBoundVariableCondition
    <$ keyword "no-bounded-variable-condition"
      <|> NoGenericInstanceFor
    <$ keyword "no-generic-instance-for"

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

dataP :: Parser DataTy
dataP = do
  keyword "data"
  n <- simpleNameP
  params <- option [] (parens (typeP `sepBy1` comma))
  cs <- option [] (equalsP *> (constrP `sepBy1` symbol "|"))
  _ <- semicolon
  return (DataTy n params cs)

constrP :: Parser Constr
constrP = do
  n <- simpleNameP
  args <- option [] (parens (typeP `sepBy1` comma))
  return (Constr n args)

tySymP :: Parser TySym
tySymP = do
  keyword "type"
  n <- simpleNameP
  params <- option [] (parens (typeP `sepBy1` comma))
  _ <- equalsP
  t <- typeP
  _ <- semicolon
  return (TySym n params t)

-- Instance methods live outside a contract, so they may not carry the
-- contract-only modifiers ('public' / 'payable').
funDefP :: Parser FunDef
funDefP = try $ withSigPrefix (funDefAfterPrefix False)

-- | Parse a function definition after its optional signature prefix.
-- 'allowContractModifiers' controls whether the leading `public` and `payable`
-- modifiers are accepted: both are only meaningful inside a `contract { … }`
-- body, so callers outside a contract (top-level functions, instance methods)
-- pass 'False' and an explicit modifier is rejected with a clear error.
funDefAfterPrefix :: Bool -> [Ty] -> [Pred] -> Parser FunDef
funDefAfterPrefix allowContractModifiers vars ctx = do
  isPub <- publicModifierP allowContractModifiers
  sig <- signatureP allowContractModifiers vars ctx
  body <- braces bodyP
  return (FunDef isPub sig (implicitReturn body))

-- | Parse an optional `public` visibility modifier. When 'allowPublic' is
-- 'False' (anywhere outside a contract body), an explicit `public` is rejected
-- with a clear error rather than being silently accepted.
publicModifierP :: Bool -> Parser Bool
publicModifierP allowPublic = do
  isPub <- option False (True <$ try (keyword "public"))
  when (isPub && not allowPublic) $
    fail "'public' is only allowed on functions declared inside a contract"
  return isPub

implicitReturn :: Body -> Body
implicitReturn [StmtExp e] = [Return e]
implicitReturn stmts = stmts

-- | Parse an optional @payable@ modifier. @payable@ is only meaningful on a
-- function, the constructor, or the fallback *inside a contract*; callers in
-- any other context pass @allowPayable = False@ so we reject it with a clear
-- error instead of silently accepting it.
payableP :: Bool -> Parser Bool
payableP allowPayable =
  option False $ do
    keyword "payable"
    if allowPayable
      then pure True
      else fail "`payable` is only allowed on a function, constructor, or fallback inside a contract"

signatureP :: Bool -> [Ty] -> [Pred] -> Parser Signature
signatureP allowPayable vars ctx = do
  payable <- payableP allowPayable
  keyword "function"
  n <- simpleNameP
  ps <- parens (paramP `sepBy` comma)
  (rc, ret) <- option (False, Nothing) $ do
    _ <- symbol "->"
    ct <- option False (True <$ keyword "comptime")
    t <- typeP
    return (ct, Just t)
  return (Signature vars ctx n ps rc ret payable)

fallbackDefAfterPrefix :: [Ty] -> [Pred] -> Parser FunDef
fallbackDefAfterPrefix vars ctx = do
  sig <- fallbackSignatureP vars ctx
  body <- braces bodyP
  return (FunDef False sig (implicitReturn body))

fallbackSignatureP :: [Ty] -> [Pred] -> Parser Signature
fallbackSignatureP vars ctx = do
  payable <- payableP True
  keyword "fallback"
  ps <- parens (paramP `sepBy` comma)
  case ps of
    [] -> pure ()
    _ -> fail "fallback function must not declare input parameters"
  ret <- optional (symbol "->" *> typeP)
  case ret of
    Nothing -> pure ()
    Just (TyCon (Name "()") []) -> pure ()
    Just _ -> fail "fallback function must return unit (`()`)"
  return (Signature vars ctx (Name "fallback") ps False ret payable)

-- | One function signature inside a class body.
-- Commits to requiring ';' once the signature is parsed, so a missing
-- semicolon produces "expecting ';' after function signature" rather than
-- the confusing "unexpected 'f', expecting '}'".
classSigP :: Parser Signature
classSigP = do
  sig <- try (withSigPrefix (signatureP False))
  _ <- semicolon <?> "';' after function signature"
  return sig

classAfterPrefix :: [Ty] -> [Pred] -> Parser Class
classAfterPrefix vars ctx = do
  keyword "class"
  mty <- atomTypeP
  _ <- colon
  cname <- qualifiedName
  params <- option [] (parens (typeP `sepBy1` comma))
  sigs <- braces (many classSigP)
  return (Class vars ctx cname params mty sigs)

instanceAfterPrefix :: [Ty] -> [Pred] -> Parser Instance
instanceAfterPrefix vars ctx = do
  isDefault <- option False (True <$ keyword "default")
  keyword "instance"
  mty <- atomTypeP
  _ <- colon
  iname <- qualifiedName
  params <- option [] (parens (typeP `sepBy1` comma))
  funs <- braces (many funDefP)
  return (Instance isDefault vars ctx iname params mty funs)

contractP :: Parser Contract
contractP = do
  keyword "contract"
  n <- simpleNameP
  params <- option [] (parens (typeP `sepBy1` comma))
  ds <- braces (many contractDeclP)
  return (Contract n params ds)

contractDeclP :: Parser ContractDecl
contractDeclP =
  CDataDecl
    <$> dataP
      <|> CConstrDecl
    <$> try constructorDeclP
      <|> rejectPublicOnImplicitlyPublicP
      <|> withSigPrefix
        ( \vars ctx ->
            CFunDecl
              <$> (try (funDefAfterPrefix True vars ctx) <|> fallbackDefAfterPrefix vars ctx)
        )
      <|> CFieldDecl
    <$> fieldDeclP

-- | `fallback` and `constructor` are implicitly public; reject an explicit
-- `public` modifier on them with a clear error rather than a confusing
-- parser failure.
rejectPublicOnImplicitlyPublicP :: Parser a
rejectPublicOnImplicitlyPublicP = do
  kw <- try $ do
    _ <- keyword "public"
    _ <- optional (keyword "payable")
    ("fallback" <$ keyword "fallback") <|> ("constructor" <$ keyword "constructor")
  fail (kw ++ " is implicitly public; remove the 'public' keyword")

fieldDeclP :: Parser Field
fieldDeclP = do
  n <- simpleNameP
  _ <- colon
  loc <- option Storage (Transient <$ keyword "transient")
  ty <- typeP
  me <- optional (equalsP *> expP)
  _ <- semicolon
  return (Field n ty me loc)

constructorDeclP :: Parser Constructor
constructorDeclP = do
  payable <- option False (True <$ keyword "payable")
  keyword "constructor"
  ps <- parens (paramP `sepBy` comma)
  body <- braces bodyP
  return (Constructor ps body payable)

topDeclP :: Parser TopDecl
topDeclP =
  choice
    [ TPragmaDecl <$> pragmaP,
      TExportDecl <$> exportP,
      TDataDef <$> dataP,
      TSym <$> tySymP,
      TContr <$> contractP,
      contractOnlyDeclP,
      withSigPrefix
        ( \vars ctx ->
            choice
              [ TFunDef <$> funDefAfterPrefix False vars ctx,
                TClassDef <$> classAfterPrefix vars ctx,
                TInstDef <$> instanceAfterPrefix vars ctx
              ]
        )
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
