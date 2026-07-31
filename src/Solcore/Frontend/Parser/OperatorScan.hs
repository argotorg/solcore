module Solcore.Frontend.Parser.OperatorScan
  ( scanOperators,
    scanImports,
    scanExportModulePaths,
  )
where

import Common.LightYear
import Data.List (nubBy)
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

-- Lightweight scan of a source file collecting every operator declaration.
-- Tolerates arbitrary content between declarations.
scanOperators :: String -> [OperatorDecl]
scanOperators src =
  case runParser (sc *> scanP) "<scan-ops>" src of
    Left _ -> []
    Right ops -> nubBy (\a b -> opSymbol a == opSymbol b) ops
  where
    scanP :: Parser [OperatorDecl]
    scanP = do
      ops <- many (try opDeclP <|> (anySingle *> pure Nothing))
      eof
      pure [op | Just op <- ops]

    opDeclP :: Parser (Maybe OperatorDecl)
    opDeclP = do
      fix <- fixityP
      prec <- fromIntegral <$> integer
      sym <- parenOpP
      _ <- symbol "=>"
      fun <- qualFunP
      _ <- optional semicolon
      pure (Just (OperatorDecl fix prec sym fun))

    qualFunP :: Parser Name
    qualFunP = do
      h <- identifier
      mt <- optional (char '.' *> identifier)
      sc
      pure $ case mt of
        Nothing -> Name h
        Just t -> QualName (Name h) t

    fixityP :: Parser OpFixity
    fixityP =
      (OpInfixL <$ keyword "infixl")
        <|> (OpInfixR <$ keyword "infixr")
        <|> (OpInfixN <$ keyword "infix")
        <|> (OpPrefix <$ keyword "prefix")
        <|> (OpPostfix <$ keyword "postfix")

-- Lightweight scan of a source file collecting every import declaration.
-- Tolerates arbitrary content between imports (skips unknown tokens).
-- Import syntax uses only identifiers, dots, braces, and keywords — no
-- operator symbols — so this scan needs no operator table.
scanImports :: String -> [Import]
scanImports src =
  case runParser (sc *> scanP) "<scan-imports>" src of
    Left _ -> []
    Right imps -> imps
  where
    scanP :: Parser [Import]
    scanP = do
      imps <- many (try importDeclP <|> (anySingle *> pure Nothing))
      eof
      pure [i | Just i <- imps]

    importDeclP :: Parser (Maybe Import)
    importDeclP = do
      keyword "import"
      imp <-
        choice
          [ do
              path <- externalPathP
              choice
                [ do
                    _ <- symbol "."
                    entries <- braces (itemEntryP `sepBy` comma)
                    hids <- option [] hidingP <* semicolon
                    pure (ImportOnly path (SelectItems entries hids)),
                  do
                    keyword "as"
                    n <- Name <$> identifier
                    _ <- semicolon
                    pure (ImportAlias path n),
                  ImportModule path <$ semicolon
                ],
            do
              path <- modPathP
              choice
                [ do
                    _ <- symbol "."
                    entries <- braces (itemEntryP `sepBy` comma)
                    hids <- option [] hidingP <* semicolon
                    pure (ImportOnly path (SelectItems entries hids)),
                  do
                    keyword "as"
                    n <- Name <$> identifier
                    _ <- semicolon
                    pure (ImportAlias path n),
                  ImportModule path <$ semicolon
                ]
          ]
      pure (Just imp)
      where
        hidingP = keyword "hiding" *> braces (fmap Name identifier `sepBy` comma)

    modPathP :: Parser ModulePath
    modPathP = do
      h <- identifier
      ts <- many (try (char '.' *> notFollowedBy (char '{') *> identifier))
      pure (classifyModulePath (foldl QualName (Name h) ts))

    externalPathP :: Parser ModulePath
    externalPathP = do
      lib <- symbol "@" *> identifier <* char '.'
      sc
      h <- identifier
      ts <- many (try (char '.' *> notFollowedBy (char '{') *> identifier))
      pure (ExternalPath (Name lib) (foldl QualName (Name h) ts))

    classifyModulePath :: Name -> ModulePath
    classifyModulePath n = case splitQual n of
      ("lib" : rest@(_ : _)) -> LibraryPath (mkQualName rest)
      _ -> RelativePath n

    splitQual :: Name -> [String]
    splitQual (Name s) = [s]
    splitQual (QualName n s) = splitQual n ++ [s]

    mkQualName :: [String] -> Name
    mkQualName [] = error "mkQualName: empty"
    mkQualName (x : xs) = foldl QualName (Name x) xs

    itemEntryP :: Parser ItemSelectorEntry
    itemEntryP =
      SelectAllItems
        <$ symbol "*"
          <|> SelectOperator
        <$> try parenOpP
          <|> try (SelectItemAs <$> (Name <$> identifier) <* keyword "as" <*> (Name <$> identifier))
          <|> SelectItem
          . Name
        <$> identifier

-- Lightweight scan collecting module paths referenced in export declarations.
-- Handles: "export M;", "export M as N;", "export M.{..};", "export M.*;",
-- and the @-prefixed external variants.
-- Needed so the loader can visit export-referenced modules before the full parse.
scanExportModulePaths :: String -> [ModulePath]
scanExportModulePaths src =
  case runParser (sc *> scanP) "<scan-exports>" src of
    Left _ -> []
    Right paths -> paths
  where
    scanP :: Parser [ModulePath]
    scanP = do
      paths <- many (try exportPathP <|> (anySingle *> pure Nothing))
      eof
      pure [p | Just p <- paths]

    exportPathP :: Parser (Maybe ModulePath)
    exportPathP = do
      keyword "export"
      -- skip the export body: skip until ';' is found (tolerant)
      path <- try (Just <$> exportModulePath) <|> pure Nothing
      _ <- manyTill anySingle (char ';')
      sc
      pure path

    -- Parse a module path that follows "export", stopping before
    -- '{', '*', 'as', or ';'.
    exportModulePath :: Parser ModulePath
    exportModulePath =
      choice
        [ do
            lib <- symbol "@" *> identifier <* char '.'
            sc
            h <- identifier
            ts <- many (try (char '.' *> notFollowedBy stopChar *> identifier))
            pure (ExternalPath (Name lib) (foldl QualName (Name h) ts)),
          do
            h <- identifier
            ts <- many (try (char '.' *> notFollowedBy stopChar *> identifier))
            let name' = foldl QualName (Name h) ts
            pure (classifyModulePath name')
        ]

    stopChar :: Parser ()
    stopChar = () <$ satisfy (\c -> c == '{' || c == '*' || c == ';')

    classifyModulePath :: Name -> ModulePath
    classifyModulePath n = case splitQual n of
      ("lib" : rest@(_ : _)) -> LibraryPath (mkQualName rest)
      _ -> RelativePath n

    splitQual :: Name -> [String]
    splitQual (Name s) = [s]
    splitQual (QualName n s) = splitQual n ++ [s]

    mkQualName :: [String] -> Name
    mkQualName [] = error "mkQualName: empty"
    mkQualName (x : xs) = foldl QualName (Name x) xs
