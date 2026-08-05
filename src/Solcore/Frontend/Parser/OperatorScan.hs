module Solcore.Frontend.Parser.OperatorScan
  ( scanOperators,
    scanRawOperators,
    scanOperatorsLocated,
    duplicateOperator,
    crossOperatorConflict,
    scanImports,
    scanExportModulePaths,
  )
where

import Common.LightYear
import Data.List (nubBy)
import Data.Maybe (listToMaybe)
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

-- Lightweight scan of a source file collecting every operator declaration
-- together with the source offset at which it starts. Tolerates arbitrary
-- content between declarations. Declarations are returned in source order, with
-- no deduplication (see scanOperators / duplicateOperator).
scanOperatorsLocated :: String -> [(Int, OperatorDecl)]
scanOperatorsLocated src =
  case runParser (sc *> scanP) "<scan-ops>" src of
    Left _ -> []
    Right ops -> ops
  where
    scanP :: Parser [(Int, OperatorDecl)]
    scanP = do
      ops <- many (try opDeclP <|> (anySingle *> pure Nothing))
      eof
      pure [op | Just op <- ops]

    opDeclP :: Parser (Maybe (Int, OperatorDecl))
    opDeclP = do
      offset <- getOffset
      fix <- fixityP
      prec <- fromIntegral <$> integer
      sym <- parenOpP
      _ <- symbol "=>"
      fun <- qualFunP
      _ <- optional semicolon
      pure (Just (offset, OperatorDecl fix prec sym fun))

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

-- Raw operator declarations of a source, in order, without deduplication.
scanRawOperators :: String -> [OperatorDecl]
scanRawOperators = map snd . scanOperatorsLocated

-- Operator declarations of a source, keeping the first declaration of each
-- symbol. Used to build the expression operator table. A well-formed module has
-- no duplicate symbols (they are rejected by duplicateOperator before parsing),
-- so this coincides with scanRawOperators there.
scanOperators :: String -> [OperatorDecl]
scanOperators = nubBy (\a b -> opSymbol a == opSymbol b) . scanRawOperators

-- The first operator symbol declared more than once in a single module: either
-- a redefinition (same symbol declared twice) or a second fixity for the same
-- symbol (e.g. infix and postfix). Returns the source offset of the offending
-- (second) declaration and the symbol; Nothing when every symbol is unique.
duplicateOperator :: [(Int, OperatorDecl)] -> Maybe (Int, String)
duplicateOperator = go []
  where
    go _ [] = Nothing
    go seen ((offset, od) : rest)
      | opSymbol od `elem` seen = Just (offset, opSymbol od)
      | otherwise = go (opSymbol od : seen) rest

-- Detect an operator symbol declared incompatibly across modules. Each list
-- tags a declaration with a provenance label (e.g. the importing module).
-- Reports the first symbol that has two structurally-different declarations
-- where at least one comes from the first (imported) list, returning the symbol
-- and the two provenance labels. Declarations that are identical (the same
-- operator reaching via several import paths, a diamond) are not a conflict.
crossOperatorConflict :: (Eq a) => [(a, OperatorDecl)] -> [(a, OperatorDecl)] -> Maybe (String, a, a)
crossOperatorConflict imported local =
  listToMaybe
    [ (opSymbol o1, l1, l2)
    | (l1, o1) <- imported,
      (l2, o2) <- imported ++ local,
      opSymbol o1 == opSymbol o2,
      o1 /= o2
    ]

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
