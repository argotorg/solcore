module Language.Core.Parser where
import Language.Core
    ( Core(..), Contract(..), Body,
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

parseCore :: String -> Core
parseCore = runMyParser "core" coreProgram

parseContract :: String -> String -> Contract
parseContract filename = runMyParser filename coreContract

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
    [ try $ TNamed <$> identifier <*> braces coreType
    , TWord <$ pKeyword "word"
    , TBool <$ pKeyword "bool"
    , TUnit <$ pKeyword "unit"
    , TSumN <$> ( pKeyword "sum" *> parens (commaSep coreType))
    , parens coreType
    ]

coreType :: Parser Type
coreType = makeExprParser pPrimaryType coreTypeTable

coreTypeTable :: [[Operator Parser Type]]
coreTypeTable = [[InfixR (TPair <$ symbol "*")]
                ,[InfixR (TSum <$ symbol "+")]]

pPrimaryExpr :: Parser Expr
pPrimaryExpr = choice
    [ EWord <$> integer
    , EBool True <$ pKeyword "true"
    , EBool False <$ pKeyword "false"
    , pTuple
    , try (ECall <$> identifier <*> parens (commaSep coreExpr))
    , EVar <$> (identifier  <* notFollowedBy (symbol "("))
    , parens coreExpr
    ]

pTuple :: Parser Expr
pTuple = go <$> parens (commaSep coreExpr) where
    go [] = EUnit
    go [e] = e
    go [e1, e2] = EPair e1 e2
    go (e:es) = EPair e (go es)


coreExpr :: Parser Expr
coreExpr = choice
    [ pKeyword "inl" *> (EInl <$> angles coreType <*> pPrimaryExpr)
    , pKeyword "inr" *> (EInr <$> angles coreType <*> pPrimaryExpr)
    , pKeyword "in" *> (EInK <$> parens int <*> coreType <*> pPrimaryExpr)
    , pKeyword "fst" *> (EFst <$> pPrimaryExpr)
    , pKeyword "snd" *> (ESnd <$> pPrimaryExpr)
    , pPrimaryExpr
    ]

coreStmt :: Parser Stmt
coreStmt = choice
    [ SAlloc <$> (pKeyword "let" *> identifier) <*> (symbol ":" *> coreType)
    , SReturn <$> (pKeyword "return" *> coreExpr)
    , SBlock <$> braces(many coreStmt)
    , SMatch <$> (pKeyword "match" *> angles coreType) <*> (coreExpr <* pKeyword "with") <*> braces(many coreAlt)
    -- , SMatch <$> (pKeyword "match" *> coreExpr <* pKeyword "with") <*> (symbol "{" *> many coreAlt <* symbol "}")
    , SFunction <$> (pKeyword "function" *> identifier) <*> (parens (commaSep coreArg)) <*> (symbol "->" *> coreType)
                <*> coreBody
    , SAssembly <$> (pKeyword "assembly" *> yulBlock)
    , SRevert <$> (pKeyword "revert" *> stringLiteral)
    , try (SAssign <$> (coreExpr <* symbol ":=") <*> coreExpr)
    , SExpr <$> coreExpr
    ]

coreBody :: Parser Body
coreBody = braces(many coreStmt)

coreArg :: Parser Arg
coreArg = TArg <$> identifier <*> (symbol ":" *> coreType)

coreAlt :: Parser Alt
coreAlt = Alt <$> corePat <*> identifier <* symbol "=>" <*> coreBody

corePat :: Parser Pat
corePat = choice
    [ PIntLit <$> integer
    , PCon CInl <$ pKeyword "inl"
    , PCon CInr <$ pKeyword "inr"
    , pKeyword "in" >> PCon . CInK <$> parens int
    , PVar <$> identifier
    , PWildcard <$ pKeyword "_"
    ]

coreProgram :: Parser Core
coreProgram = sc *> (Core <$> many coreStmt) <* eof

coreContract :: Parser Contract
coreContract = sc *> (Contract <$> (pKeyword "contract" *> identifier )
                  <*> braces (many coreStmt)) <* eof
