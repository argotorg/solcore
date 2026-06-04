module Solcore.Frontend.Parser.Expr
  ( exprP,
  )
where

import Common.LightYear
import Control.Monad.Combinators.Expr
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.SolcoreTypes (atomTypeP, paramP, typeP)
import Solcore.Frontend.Syntax.Name (Name (..))
import Solcore.Frontend.Syntax.SyntaxTree (Exp (..), Literal (..), Stmt)

type BodyP = Parser [Stmt]

exprP :: BodyP -> Parser Exp
exprP bp = tyAnnP bp

tyAnnP :: BodyP -> Parser Exp
tyAnnP bp = do
  e <- ternaryP bp
  option e $ TyExp e <$> (colon *> typeP)

ternaryP :: BodyP -> Parser Exp
ternaryP bp =
  try (ifThenElseP bp) <|> do
    e1 <- binaryP bp
    option e1 $ do
      _ <- symbol "?"
      e2 <- ternaryP bp
      _ <- symbol ":"
      e3 <- ternaryP bp
      return (ExpCond e1 e2 e3)

ifThenElseP :: BodyP -> Parser Exp
ifThenElseP bp = do
  keyword "if"
  e1 <- ternaryP bp
  keyword "then"
  e2 <- ternaryP bp
  keyword "else"
  e3 <- ternaryP bp
  return (ExpCond e1 e2 e3)

binaryP :: BodyP -> Parser Exp
binaryP bp = makeExprParser (postfixP bp) opTable

opTable :: [[Operator Parser Exp]]
opTable =
  [ [ Prefix
        ( ExpLNot
            <$ try (lexeme (char '!' <* notFollowedBy (char '=')))
        )
    ],
    [ InfixL (ExpTimes <$ try (symbol "*")),
      InfixL (ExpDivide <$ try (symbol "/")),
      InfixL (ExpModulo <$ try (symbol "%"))
    ],
    [ InfixL
        ( ExpPlus
            <$ try (lexeme (char '+' <* notFollowedBy (char '=')))
        ),
      InfixL
        ( ExpMinus
            <$ try (lexeme (char '-' <* notFollowedBy (char '=')))
        )
    ],
    [ InfixN (ExpLE <$ try (symbol "<=")),
      InfixN (ExpGE <$ try (symbol ">=")),
      InfixN
        ( ExpLT
            <$ try (lexeme (char '<' <* notFollowedBy (char '=')))
        ),
      InfixN
        ( ExpGT
            <$ try (lexeme (char '>' <* notFollowedBy (char '=')))
        )
    ],
    [ InfixN (ExpEE <$ try (symbol "==")),
      InfixN (ExpNE <$ try (symbol "!="))
    ],
    [InfixL (ExpLAnd <$ try (symbol "&&"))],
    [InfixL (ExpLOr <$ try (symbol "||"))]
  ]

postfixP :: BodyP -> Parser Exp
postfixP bp = do
  e0 <- atomP bp
  ops <- many (postfixOp bp)
  return (foldl (\acc f -> f acc) e0 ops)

postfixOp :: BodyP -> Parser (Exp -> Exp)
postfixOp bp = dotOp bp <|> idxOp bp

dotOp :: BodyP -> Parser (Exp -> Exp)
dotOp bp = do
  _ <- char '.'
  sc
  n <- identifier
  mArgs <- optional (parens (exprP bp `sepBy` comma))
  return $ case mArgs of
    Just args -> \e -> ExpName (Just e) (Name n) args
    Nothing -> \e -> ExpVar (Just e) (Name n)

idxOp :: BodyP -> Parser (Exp -> Exp)
idxOp bp = do
  idx <- brackets (exprP bp)
  return (\e -> ExpIndexed e idx)

atomP :: BodyP -> Parser Exp
atomP bp = litP <|> try (lamP bp) <|> proxyP <|> try (dotNameP bp) <|> parenP bp <|> nameP bp

litP :: Parser Exp
litP =
  Lit . IntLit
    <$> integer
      <|> Lit
      . StrLit
    <$> stringLit

lamP :: BodyP -> Parser Exp
lamP bp = do
  keyword "lam"
  ps <- parens (paramP `sepBy` comma)
  retTy <- optional (symbol "->" *> typeP)
  body <- braces bp
  return (Lam ps body retTy)

proxyP :: Parser Exp
proxyP = ExpAt <$> (symbol "@" *> atomTypeP)

dotNameP :: BodyP -> Parser Exp
dotNameP bp = do
  _ <- char '.'
  sc
  n <- identifier
  args <- option [] (parens (exprP bp `sepBy` comma))
  return (ExpDotName (Name n) args)

parenP :: BodyP -> Parser Exp
parenP bp = parens $ do
  es <- exprP bp `sepBy` comma
  return $ case es of
    [] -> ExpName Nothing (Name "()") []
    [e] -> e
    _ -> foldr1 pairE es
  where
    pairE e1 e2 = ExpName Nothing (Name "pair") [e1, e2]

nameP :: BodyP -> Parser Exp
nameP bp = do
  n <- identifier
  mArgs <- optional (parens (exprP bp `sepBy` comma))
  return $ case mArgs of
    Just args -> ExpName Nothing (Name n) args
    Nothing -> ExpVar Nothing (Name n)
