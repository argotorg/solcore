module Solcore.Frontend.Lexer.SolcoreLexer
  ( sc,
    lexeme,
    symbol,
    keyword,
    reservedWords,
    identifier,
    integer,
    stringLit,
    parens,
    braces,
    brackets,
    comma,
    semicolon,
    colon,
    isOpChar,
    parenOpP,
  )
where

import Common.LightYear
import Text.Megaparsec.Char.Lexer qualified as L

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

identChar :: Parser Char
identChar = alphaNumChar <|> char '_'

keyword :: String -> Parser ()
keyword kw = lexeme (try (string kw *> notFollowedBy identChar))

reservedWords :: [String]
reservedWords =
  [ "contract",
    "import",
    "export",
    "hiding",
    "as",
    "let",
    "data",
    "forall",
    "class",
    "instance",
    "if",
    "else",
    "for",
    "switch",
    "case",
    "default",
    "leave",
    "continue",
    "break",
    "assembly",
    "match",
    "function",
    "fallback",
    "payable",
    "public",
    "constructor",
    "return",
    "lam",
    "type",
    "pragma",
    "infixl",
    "infixr",
    "infix",
    "prefix",
    "postfix"
  ]

isOpChar :: Char -> Bool
isOpChar c =
  c `elem` ("+-*/%<>=!&|^~#?" :: String)
    || (c >= '\x2200' && c <= '\x23FF')

parenOpP :: Parser String
parenOpP = lexeme $ do
  _ <- char '('
  sc
  sym <- some (satisfy isOpChar) <|> identSym
  sc
  _ <- char ')'
  pure sym
  where
    -- An operator symbol is normally made of operator characters (e.g. `+`,
    -- `^^`). It may also be an alphabetic identifier, used for postfix unit
    -- suffixes like `(ether)` or `(minutes)`. The two are disjoint (letters are
    -- never operator characters), so the choice is unambiguous. A reserved word
    -- (`if`, `let`, `match`, …) is not accepted as an operator symbol, since it
    -- would wreck parsing wherever the keyword appears.
    identSym = do
      w <- (:) <$> letterChar <*> many identChar
      if w `elem` reservedWords
        then fail ("reserved word used as operator symbol: " ++ w)
        else pure w

identifier :: Parser String
identifier = lexeme go <?> "identifier"
  where
    go = do
      h <- letterChar
      t <- many identChar
      let w = h : t
      if w `elem` reservedWords
        then fail ("reserved word used as identifier: " ++ w)
        else pure w

integer :: Parser Integer
integer = lexeme (try hexLit <|> L.decimal) <?> "integer literal"
  where
    hexLit = string "0x" *> L.hexadecimal

stringLit :: Parser String
stringLit =
  lexeme (char '"' *> manyTill charLit (char '"'))
    <?> "string literal"
  where
    charLit = escaped <|> anySingle
    escaped = char '\\' *> escapeChar
    escapeChar =
      choice
        [ char 'n' *> pure '\n',
          char 't' *> pure '\t',
          char '"' *> pure '"',
          char '\\' *> pure '\\'
        ]

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

brackets :: Parser a -> Parser a
brackets = between (symbol "[") (symbol "]")

comma :: Parser String
comma = symbol ","

semicolon :: Parser String
semicolon = symbol ";"

colon :: Parser String
colon = symbol ":"
