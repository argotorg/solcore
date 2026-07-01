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
    "pragma"
  ]

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
