module Language.Hull.Parser where
import Language.Hull
    ( Object(..),
      Body,
      Alt(..),
      Pat(..),
      Arg(..),
      Con(..),
      Stmt(SExpr, SAlloc, SReturn, SBlock, SMatch,
           SFunction, SAssign, SAssembly, SRevert),
      Expr(..),
      Type(..) )
import Common.LightYear
import Text.Megaparsec.Char.Lexer qualified as L
import Control.Monad.Combinators.Expr
import Language.Yul.Parser(yulBlock)


parseObject :: String -> String -> Object
parseObject filename = runMyParser filename hullObject

-- Note: this module repeats some definitions from YulParser.Name
-- This is intentional as we may want to make different syntax choices

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

integer :: Parser Integer
integer = lexeme L.decimal

int :: Parser Int
int = fromInteger <$> integer

stringLiteral :: Parser String
stringLiteral = lexeme (char '"' *> manyTill L.charLiteral (char '"'))

parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

braces :: Parser a -> Parser a
braces = between (symbol "{") (symbol "}")

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

commaSep :: Parser a -> Parser [a]
commaSep p = p `sepBy` symbol ","

pKeyword :: String -> Parser String
pKeyword w = try $ lexeme (string w <* notFollowedBy identChar)

pPrimaryType :: Parser Type
pPrimaryType = choice
    [ try $ TNamed <$> identifier <*> braces hullType
    , TWord <$ pKeyword "word"
    , TBool <$ pKeyword "bool"
    , TUnit <$ pKeyword "unit"
    , TSumN <$> ( pKeyword "sum" *> parens (commaSep hullType))
    , parens hullType
    ]

hullType :: Parser Type
hullType = makeExprParser pPrimaryType hullTypeTable

hullTypeTable :: [[Operator Parser Type]]
hullTypeTable = [[InfixR (TPair <$ symbol "*")]
                ,[InfixR (TSum <$ symbol "+")]]

pPrimaryExpr :: Parser Expr
pPrimaryExpr = choice
    [ EWord <$> integer
    , EBool True <$ pKeyword "true"
    , EBool False <$ pKeyword "false"
    , pTuple
    , try (ECall <$> identifier <*> parens (commaSep hullExpr))
    , EVar <$> (identifier  <* notFollowedBy (symbol "("))
    , parens hullExpr
    ]

pTuple :: Parser Expr
pTuple = go <$> parens (commaSep hullExpr) where
    go [] = EUnit
    go [e] = e
    go [e1, e2] = EPair e1 e2
    go (e:es) = EPair e (go es)


hullExpr :: Parser Expr
hullExpr = choice
    [ pKeyword "inl" *> (EInl <$> angles hullType <*> pPrimaryExpr)
    , pKeyword "inr" *> (EInr <$> angles hullType <*> pPrimaryExpr)
    , pKeyword "in" *> (EInK <$> parens int <*> hullType <*> pPrimaryExpr)
    , pKeyword "fst" *> (EFst <$> pPrimaryExpr)
    , pKeyword "snd" *> (ESnd <$> pPrimaryExpr)
    , condExpr
    , pPrimaryExpr
    ]

condExpr = do
  pKeyword "if"
  t <- angles hullType
  e1 <- hullExpr
  pKeyword "then"
  e2 <- hullExpr
  pKeyword "else"
  e3 <- hullExpr
  pure (ECond t e1 e2 e3)

hullStmt :: Parser Stmt
hullStmt = choice
    [ SAlloc <$> (pKeyword "let" *> identifier) <*> (symbol ":" *> hullType)
    , SReturn <$> (pKeyword "return" *> hullExpr)
    , SBlock <$> braces(many hullStmt)
    , SMatch <$> (pKeyword "match" *> angles hullType) <*> (hullExpr <* pKeyword "with") <*> braces(many hullAlt)
    -- , SMatch <$> (pKeyword "match" *> hullExpr <* pKeyword "with") <*> (symbol "{" *> many hullAlt <* symbol "}")
    , SFunction <$> (pKeyword "function" *> identifier) <*> (parens (commaSep hullArg)) <*> (symbol "->" *> hullType)
                <*> hullBody
    , SAssembly <$> (pKeyword "assembly" *> yulBlock)
    , SRevert <$> (pKeyword "revert" *> stringLiteral)
    , try (SAssign <$> (hullExpr <* symbol ":=") <*> hullExpr)
    , SExpr <$> hullExpr
    ]

hullBody :: Parser Body
hullBody = braces(many hullStmt)

hullArg :: Parser Arg
hullArg = TArg <$> identifier <*> (symbol ":" *> hullType)

hullAlt :: Parser Alt
hullAlt = Alt <$> hullPat <*> identifier <* symbol "=>" <*> hullBody

hullPat :: Parser Pat
hullPat = choice
    [ PIntLit <$> integer
    , PCon CInl <$ pKeyword "inl"
    , PCon CInr <$ pKeyword "inr"
    , pKeyword "in" >> PCon . CInK <$> parens int
    , PVar <$> identifier
    , PWildcard <$ pKeyword "_"
    ]

hullObject :: Parser Object
hullObject = sc *> (Object <$> (pKeyword "object" *> identifier  <* symbol "{")
               <*> hullCode <*> many hullObject) <* symbol "}"

hullCode :: Parser Body
hullCode = sc *> (Object <$> pKeyword "code" *> hullBody)
