module Solcore.Frontend.Parser.OperatorScan
  ( scanOperators,
    scanRawOperators,
    scanOperatorsLocated,
    duplicateOperator,
    crossOperatorConflict,
    associativityConflict,
    scanExportedOperators,
    exportedOperators,
    scanImports,
  )
where

import Common.LightYear
import Control.Monad (void)
import Data.List (nubBy)
import Data.Maybe (listToMaybe)
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.Decl (externalPathP, itemEntryP, modulePathP, opTargetP)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

-- Repeatedly apply `declP` over the whole source, collecting every `Just`
-- result, while skipping whitespace, comments and string literals between
-- matches so that text inside a comment or a string literal is never scanned as
-- a declaration. `sc` runs at the start and after every item, so the choice
-- point is always at a code position: the fallback consumes a string literal as
-- a unit (via stringLit, honouring escapes) or a single code character, and
-- comments are consumed by `sc`, never entered character-by-character.
scanForDecls :: Parser (Maybe a) -> String -> [a]
scanForDecls declP src =
  either (const []) (\xs -> [x | Just x <- xs]) (runParser p "<scan>" src)
  where
    p = sc *> many item <* eof
    item =
      (try declP <* sc)
        <|> ((void stringLit <|> void anySingle) *> sc *> pure Nothing)

-- Lightweight scan of a source file collecting every operator declaration
-- together with the source offset at which it starts. Tolerates arbitrary
-- content between declarations. Declarations are returned in source order, with
-- no deduplication (see scanOperators / duplicateOperator).
scanOperatorsLocated :: String -> [(Int, OperatorDecl)]
scanOperatorsLocated = scanForDecls opDeclP
  where
    opDeclP :: Parser (Maybe (Int, OperatorDecl))
    opDeclP = do
      offset <- getOffset
      fix <- fixityP
      prec <- fromIntegral <$> integer
      sym <- parenOpP
      _ <- symbol "=>"
      target <- opTargetP fix qualFunP
      _ <- optional semicolon
      pure (Just (offset, OperatorDecl fix prec sym target))

    qualFunP :: Parser Name
    qualFunP = do
      h <- identifier
      ts <- many (char '.' *> identifier)
      sc
      pure (foldl QualName (Name h) ts)

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

-- Detect two infix operators that share a precedence level but disagree on
-- associativity. `makeExprParser` processes each precedence level with a single
-- associativity, so a level mixing (for example) infixl and infixr stops
-- mid-expression and leaves the rest unconsumed. Each declaration is tagged with
-- a provenance label (its module). Returns the earlier and the later (offending)
-- declaration; Nothing when every precedence level is internally consistent.
-- Prefix and postfix operators are unary and do not participate in a level's
-- infix associativity, so they never conflict here.
associativityConflict ::
  [(a, OperatorDecl)] -> Maybe ((a, OperatorDecl), (a, OperatorDecl))
associativityConflict = go []
  where
    go _ [] = Nothing
    go seen (cur@(_, od) : rest)
      | isInfixDecl od =
          case filter (conflictsWith od . snd) seen of
            (prev : _) -> Just (prev, cur)
            [] -> go (cur : seen) rest
      | otherwise = go seen rest
    conflictsWith a b = opPrec a == opPrec b && opFixity a /= opFixity b
    isInfixDecl od = opFixity od `elem` [OpInfixL, OpInfixR, OpInfixN]

-- Scan a module's `export { ... }` blocks for its operator-export policy:
--   Nothing            -- no `export { ... }` block: every declared operator is exported
--   Just (True,  _)    -- an `export { *, ... }`: every declared operator is exported
--   Just (False, syms) -- explicit `export { ... }`: only these operator symbols are exported
-- If the export block cannot be parsed, it is treated as absent (fail open, so a
-- valid operator is never wrongly hidden).
scanExportedOperators :: String -> Maybe (Bool, [String])
scanExportedOperators src =
  case scanForDecls exportBlockP src of
    [] -> Nothing
    blocks -> Just (any fst blocks, concatMap snd blocks)
  where
    exportBlockP :: Parser (Maybe (Bool, [String]))
    exportBlockP = do
      keyword "export"
      _ <- symbol "{"
      items <- exportItemP `sepBy` comma
      _ <- symbol "}"
      _ <- optional semicolon
      pure (Just (or [star | (star, _) <- items], [s | (_, Just s) <- items]))

    -- One export spec, classified into (is it `*` / ExportAll, is it an operator).
    exportItemP :: Parser (Bool, Maybe String)
    exportItemP =
      ((True, Nothing) <$ symbol "*")
        <|> (((,) False . Just) <$> try parenOpP)
        <|> ((False, Nothing) <$ otherItem)

    -- Any other spec: a (possibly qualified) name, optional `.*`, optional
    -- `(constructorSelector)`. Consumed and ignored.
    otherItem :: Parser ()
    otherItem = do
      _ <- identifier
      _ <- many (try (symbol "." *> identifier))
      _ <- optional (try (symbol "." *> symbol "*"))
      _ <- optional (parens (void (many (satisfy (/= ')')))))
      pure ()

-- Operators of a module that are visible to importers: its declared operators
-- filtered by its export list. A module with no `export { ... }` block, or with
-- an `export { * }`, exports all of its operators.
exportedOperators :: String -> [OperatorDecl]
exportedOperators src =
  case scanExportedOperators src of
    Nothing -> declared
    Just (True, _) -> declared
    Just (False, syms) -> filter ((`elem` syms) . opSymbol) declared
  where
    declared = scanOperators src

-- Lightweight scan of a source file collecting every import declaration.
-- Tolerates arbitrary content between imports (skips unknown tokens).
-- Import syntax uses only identifiers, dots, braces, and keywords — no
-- operator symbols — so this scan needs no operator table.
scanImports :: String -> [Import]
scanImports = scanForDecls importDeclP
  where
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
              path <- modulePathP
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
