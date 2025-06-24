module Language.Yul.Parser(parseYul, yulBlock) where
{-
import Text.Megaparsec
import Text.Megaparsec.Char
import Data.Void
-}
import Common.LightYear
import Text.Megaparsec.Char.Lexer qualified as L
import Language.Yul
import Solcore.Frontend.Syntax.Name(Name(..))

parseYul :: String -> Yul
parseYul = runMyParser "yul" yulProgram

sc :: Parser ()
sc = L.space space1
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

yulExpression :: Parser YulExp
yulExpression = choice
    [ YLit <$> yulLiteral
    , try (YCall <$> pName<*> parens (commaSep yulExpression))
    , YIdent <$> pName
    , parens yulExpression
    ]

yulLiteral :: Parser YLiteral
yulLiteral = choice
    [ YulNumber <$> integer
    , YulString <$> stringLiteral
    , YulTrue <$ pKeyword "true"
    , YulFalse <$ pKeyword "false"
    ]

yulStmt :: Parser YulStmt
yulStmt = choice
    [ YBlock <$> yulBlock
    , yulFun
    , YLet <$> (pKeyword "let" *> commaSep pName) <*> optional (symbol ":=" *> yulExpression)
    , YIf <$> (pKeyword "if" *> yulExpression) <*> yulBlock
    , YSwitch <$>
        (pKeyword "switch" *> yulExpression) <*>
        many yulCase <*>
        optional (pKeyword "default" *> yulBlock)
    , try (YAssign <$> commaSep pName <*> (symbol ":=" *> yulExpression))
    , YExp <$> yulExpression
    ]

yulBlock :: Parser [YulStmt]
yulBlock = between (symbol "{") (symbol "}") (many yulStmt)

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
