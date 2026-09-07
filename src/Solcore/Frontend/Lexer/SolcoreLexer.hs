module Solcore.Frontend.Lexer.SolcoreLexer
  ( sc,
    lexeme,
    symbol,
    keyword,
    reservedWords,
    identifier,
    pragmaIdentifier,
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
import Data.Char (isNumber)
import Data.List (isPrefixOf)
import Text.Megaparsec.Char.Lexer qualified as L

sc :: Parser ()
sc =
  L.space
    (skipSome (oneOf [' ', '\t', '\n', '\r', '\f']))
    (L.skipLineComment "//")
    (L.skipBlockCommentNested "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol symbolText = lexeme (try (string symbolText <* notFollowedBy longerToken))
  where
    -- Match whole lexer tokens: an assignment must not consume the first
    -- character of an equality or compound assignment, for example.
    longerToken =
      choice
        [ string (drop (length symbolText) longer)
        | longer <- multiCharacterTokens,
          symbolText /= longer,
          symbolText `isPrefixOf` longer
        ]

multiCharacterTokens :: [String]
multiCharacterTokens =
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
    "//",
    "/*"
  ]

identChar :: Parser Char
identChar = letterChar <|> satisfy isNumber <|> char '_'

identifierContinuation :: Parser ()
identifierContinuation =
  ()
    <$ (identChar <|> char '$')
      <|> ()
    <$ try (char '-' *> letterChar)

keyword :: String -> Parser ()
keyword kw = lexeme (try (string kw *> notFollowedBy identifierContinuation))

reservedWords :: [String]
reservedWords =
  [ "contract",
    "import",
    "export",
    "as",
    "let",
    "data",
    "class",
    "forall",
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
    "true",
    "false",
    "lam",
    "type",
    "pragma"
  ]

identifier :: Parser String
identifier = lexeme (try go) <?> "identifier"
  where
    go = do
      w <- rawIdentifier
      if '-' `elem` w
        then fail ("identifier cannot contain hyphens: " ++ w)
        else pure w

pragmaIdentifier :: Parser String
pragmaIdentifier = lexeme (try rawIdentifier) <?> "pragma name"

rawIdentifier :: Parser String
rawIdentifier = do
  h <- letterChar
  t <- many identChar
  hyphenated <- many (try ((:) <$> char '-' <*> ((:) <$> letterChar <*> many identChar)))
  let w = h : t ++ concat hyphenated
  if w `elem` reservedWords
    then fail ("reserved word used as identifier: " ++ w)
    else w <$ notFollowedBy (char '$')

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
