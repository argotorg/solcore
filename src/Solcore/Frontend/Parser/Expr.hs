module Solcore.Frontend.Parser.Expr
  ( exprP,
  )
where

import Common.LightYear
import Control.Monad.Combinators.Expr
import Solcore.Diagnostics (SourceSpan)
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.SolcoreTypes (booleanNameP, locatedFromSpans, locatedP, paramP, simpleNameP, typeP)
import Solcore.Frontend.Syntax.Location (sourceSpanOf)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

type BodyP = Parser [Stmt]

exprP :: BodyP -> Parser Exp
exprP = ternaryP

castP :: BodyP -> Parser Exp
castP bp = do
  e <- unaryP bp
  targets <- many (keyword "as" *> typeP)
  pure (foldl cast e targets)
  where
    cast value target =
      locatedExpFrom [sourceSpanOf value, sourceSpanOf target] (TyExp value target)

unaryP :: BodyP -> Parser Exp
unaryP bp = do
  operators <- many logicalNotP
  operand <- postfixP bp
  pure (foldr ($) operand operators)
  where
    logicalNotP =
      unaryExp ExpLNot
        <$ try (lexeme (char '!' <* notFollowedBy (char '=')))

ternaryP :: BodyP -> Parser Exp
ternaryP bp = do
  e1 <- binaryP bp
  option e1 $ do
    _ <- symbol "?"
    e2 <- ternaryP bp
    _ <- symbol ":"
    e3 <- ternaryP bp
    return (locatedExpFrom (map sourceSpanOf [e1, e2, e3]) (ExpCond e1 e2 e3))

binaryP :: BodyP -> Parser Exp
binaryP bp = makeExprParser (castP bp) opTable

opTable :: [[Operator Parser Exp]]
opTable =
  [ [InfixR (binaryExp ExpPower <$ try (symbol "**"))],
    [ InfixL (binaryExp ExpTimes <$ try (symbol "*")),
      InfixL (binaryExp ExpDivide <$ try (symbol "/")),
      InfixL
        ( binaryExp ExpModulo
            <$ try (lexeme (char '%' <* notFollowedBy (char '=')))
        )
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
    [ InfixL (binaryExp ExpShiftL <$ try (symbol "<<")),
      InfixL (binaryExp ExpShiftR <$ try (symbol ">>"))
    ],
    [ InfixL
        ( binaryExp ExpBAnd
            <$ try (lexeme (char '&' <* notFollowedBy (char '&') <* notFollowedBy (char '=')))
        )
    ],
    [ InfixL
        ( binaryExp ExpBXor
            <$ try (lexeme (char '^' <* notFollowedBy (char '=')))
        )
    ],
    [ InfixL
        ( binaryExp ExpBOr
            <$ try
              (lexeme (char '|' <* notFollowedBy (char '|') <* notFollowedBy (char '=')))
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
postfixOp bp = dotOp bp <|> idxOp bp <|> callOp bp

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

callOp :: BodyP -> Parser (Exp -> Exp)
callOp bp = do
  args <- parens (exprP bp `sepBy` comma)
  pure $ \callee ->
    locatedExpFrom [sourceSpanOf callee, sourceSpanOf args] $
      case callee of
        -- Keep the established source shape for direct and member calls,
        -- including redundant parentheses such as `(f)(x)`.
        ExpVar receiver memberName -> ExpName receiver memberName args
        _ -> ExpApply callee args

atomP :: BodyP -> Parser Exp
atomP bp = litP <|> try (lamP bp) <|> try (dotNameP bp) <|> parenP bp <|> nameP bp

litP :: Parser Exp
litP =
  locatedP locatedExp $
    ExpVar Nothing
      <$> booleanNameP
        <|> Lit
        . IntLit
      <$> integer
        <|> Lit
        . StrLit
      <$> stringLit

lamP :: BodyP -> Parser Exp
lamP bp = locatedP locatedExp $ do
  keyword "lam"
  ps <- parens (paramP `sepBy` comma)
  retTy <- optional $ do
    keyword "returns"
    ts <- parens (typeP `sepBy` comma)
    pure $ case ts of
      [] -> TyCon "()" []
      [t] -> t
      _ -> foldr1 pairTy ts
  body <- braces bp
  return (Lam ps body retTy)

dotNameP :: BodyP -> Parser Exp
dotNameP bp = locatedP locatedExp $ do
  _ <- char '.'
  sc
  n <- booleanNameP <|> simpleNameP
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
