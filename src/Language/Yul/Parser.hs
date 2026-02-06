module Language.Yul.Parser (parseYul, yulBlock, yulStmt, yulExp) where

import Common.LightYear
import Language.Yul
import Solcore.Frontend.Syntax.Name (Name (..))
import Text.Megaparsec.Char.Lexer qualified as L

parseYul :: String -> Yul
parseYul = runMyParser "yul" yulProgram

sc :: Parser ()
sc =
  L.space
    space1
    (L.skipLineComment "//")
    (L.skipBlockComment "/*" "*/")

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: String -> Parser String
symbol = L.symbol sc

startIdentChar :: Parser Char
startIdentChar = letterChar <|> char '_' <|> char '$'

identChar :: Parser Char
identChar = alphaNumChar <|> char '_' <|> char '$'

identifier :: Parser String
identifier = lexeme ((:) <$> startIdentChar <*> many identChar)

pName :: Parser Name
pName = Name <$> identifier

integer :: Parser Integer
integer = lexeme L.decimal

stringLiteral :: Parser String
stringLiteral = char '"' *> manyTill L.charLiteral (char '"')

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

pKeyword :: String -> Parser String
pKeyword w = lexeme (string w <* notFollowedBy identChar)

pMeta :: Parser String
pMeta =
  (char '`' *> many (satisfy (/= '`')) <* char '`')
    <|> (string "${" *> many (satisfy (/= '}')) <* char '}')

yulExp :: Parser YulExp
yulExp =
  sc
    *> choice
      [ YLit <$> yulLiteral,
        try (YCall <$> pName <*> parens (commaSep yulExp)),
        try (YMeta <$> pMeta),
        YIdent <$> pName
      ]
    <* sc

yulLiteral :: Parser YLiteral
yulLiteral =
  sc
    *> choice
      [ YulNumber <$> integer,
        YulString <$> stringLiteral,
        YulTrue <$ pKeyword "true",
        YulFalse <$ pKeyword "false"
      ]

yulStmt :: Parser YulStmt
yulStmt =
  sc
    *> choice
      [ YBlock <$> yulBlock,
        yulFun,
        YLet <$> (pKeyword "let" *> commaSep pName) <*> optional (symbol ":=" *> yulExp),
        YIf <$> (pKeyword "if" *> yulExp) <*> yulBlock,
        YFor <$> (pKeyword "for" *> yulBlock) <*> yulExp <*> yulBlock <*> yulBlock,
        YSwitch
          <$> (pKeyword "switch" *> yulExp)
          <*> many yulCase
          <*> optional (pKeyword "default" *> yulBlock),
        try (YAssign <$> commaSep pName <*> (symbol ":=" *> yulExp)),
        YExp <$> yulExp
      ]

yulBlock :: Parser [YulStmt]
yulBlock = sc *> between (symbol "{") (symbol "}") (many yulStmt)

yulCase :: Parser (YLiteral, [YulStmt])
yulCase = do
  _ <- pKeyword "case"
  lit <- yulLiteral
  stmts <- yulBlock
  return (lit, stmts)

yulFun :: Parser YulStmt
yulFun = do
  _ <- symbol "function"
  name <- pName
  args <- parens (commaSep pName)
  rets <- optional (symbol "->" *> commaSep pName)
  YFun name args rets <$> yulBlock

yulProgram :: Parser Yul
yulProgram = sc *> (Yul <$> many yulStmt) <* eof
