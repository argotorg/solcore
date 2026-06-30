module Solcore.Frontend.Parser.Expr
  ( exprP,
  )
where

import Common.LightYear
import Control.Monad.Combinators.Expr
import Solcore.Diagnostics (SourceSpan)
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.SolcoreTypes (atomTypeP, locatedFromSpans, locatedP, paramP, simpleNameP, typeP)
import Solcore.Frontend.Syntax.Location (sourceSpanOf)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

type BodyP = Parser [Stmt]

exprP :: BodyP -> Parser Exp
exprP bp = tyAnnP bp

tyAnnP :: BodyP -> Parser Exp
tyAnnP bp = do
  e <- ternaryP bp
  option e $ do
    t <- colon *> typeP
    pure (locatedExpFrom [sourceSpanOf e, sourceSpanOf t] (TyExp e t))

ternaryP :: BodyP -> Parser Exp
ternaryP bp =
  try (ifThenElseP bp) <|> do
    e1 <- binaryP bp
    option e1 $ do
      _ <- symbol "?"
      e2 <- ternaryP bp
      _ <- symbol ":"
      e3 <- ternaryP bp
      return (locatedExpFrom (map sourceSpanOf [e1, e2, e3]) (ExpCond e1 e2 e3))

ifThenElseP :: BodyP -> Parser Exp
ifThenElseP bp = locatedP locatedExp $ do
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
        ( unaryExp ExpLNot
            <$ try (lexeme (char '!' <* notFollowedBy (char '=')))
        )
    ],
    [ InfixL (binaryExp ExpTimes <$ try (symbol "*")),
      InfixL (binaryExp ExpDivide <$ try (symbol "/")),
      InfixL (binaryExp ExpModulo <$ try (symbol "%"))
    ],
    [ InfixL
        ( binaryExp ExpPlus
            <$ try (lexeme (char '+' <* notFollowedBy (char '=')))
        ),
      InfixL
        ( binaryExp ExpMinus
            <$ try (lexeme (char '-' <* notFollowedBy (char '=')))
        )
    ],
    [ InfixN (binaryExp ExpLE <$ try (symbol "<=")),
      InfixN (binaryExp ExpGE <$ try (symbol ">=")),
      InfixN
        ( binaryExp ExpLT
            <$ try (lexeme (char '<' <* notFollowedBy (char '=')))
        ),
      InfixN
        ( binaryExp ExpGT
            <$ try (lexeme (char '>' <* notFollowedBy (char '=')))
        )
    ],
    [ InfixN (binaryExp ExpEE <$ try (symbol "==")),
      InfixN (binaryExp ExpNE <$ try (symbol "!="))
    ],
    [InfixL (binaryExp ExpLAnd <$ try (symbol "&&"))],
    [InfixL (binaryExp ExpLOr <$ try (symbol "||"))]
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
  n <- simpleNameP
  mArgs <- optional (parens (exprP bp `sepBy` comma))
  return $ case mArgs of
    Just args -> \e -> locatedExpFrom [sourceSpanOf e, sourceSpanOf n, sourceSpanOf args] (ExpName (Just e) n args)
    Nothing -> \e -> locatedExpFrom [sourceSpanOf e, sourceSpanOf n] (ExpVar (Just e) n)

idxOp :: BodyP -> Parser (Exp -> Exp)
idxOp bp = do
  idx <- brackets (exprP bp)
  return (\e -> locatedExpFrom [sourceSpanOf e, sourceSpanOf idx] (ExpIndexed e idx))

atomP :: BodyP -> Parser Exp
atomP bp = litP <|> try (lamP bp) <|> proxyP <|> try (dotNameP bp) <|> parenP bp <|> nameP bp

litP :: Parser Exp
litP =
  locatedP locatedExp $
    Lit . IntLit
      <$> integer
        <|> Lit
        . StrLit
      <$> stringLit

lamP :: BodyP -> Parser Exp
lamP bp = locatedP locatedExp $ do
  keyword "lam"
  ps <- parens (paramP `sepBy` comma)
  retTy <- optional (symbol "->" *> typeP)
  body <- braces bp
  return (Lam ps body retTy)

proxyP :: Parser Exp
proxyP = locatedP locatedExp (ExpAt <$> (symbol "@" *> atomTypeP))

dotNameP :: BodyP -> Parser Exp
dotNameP bp = locatedP locatedExp $ do
  _ <- char '.'
  sc
  n <- simpleNameP
  args <- option [] (parens (exprP bp `sepBy` comma))
  return (ExpDotName n args)

parenP :: BodyP -> Parser Exp
parenP bp = locatedP locatedExp $ parens $ do
  es <- exprP bp `sepBy` comma
  return $ case es of
    [] -> ExpName Nothing (Name "()") []
    [e] -> e
    _ -> foldr1 pairE es
  where
    pairE e1 e2 = locatedExpFrom [sourceSpanOf e1, sourceSpanOf e2] (ExpName Nothing (Name "pair") [e1, e2])

nameP :: BodyP -> Parser Exp
nameP bp = locatedP locatedExp $ do
  n <- simpleNameP
  mArgs <- optional (parens (exprP bp `sepBy` comma))
  return $ case mArgs of
    Just args -> ExpName Nothing n args
    Nothing -> ExpVar Nothing n

binaryExp :: (Exp -> Exp -> Exp) -> Exp -> Exp -> Exp
binaryExp con left right =
  locatedExpFrom [sourceSpanOf left, sourceSpanOf right] (con left right)

unaryExp :: (Exp -> Exp) -> Exp -> Exp
unaryExp con operand =
  locatedExpFrom [sourceSpanOf operand] (con operand)

locatedExpFrom :: [Maybe SourceSpan] -> Exp -> Exp
locatedExpFrom = locatedFromSpans locatedExp
