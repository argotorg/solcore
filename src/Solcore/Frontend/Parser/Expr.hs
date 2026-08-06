module Solcore.Frontend.Parser.Expr
  ( exprP,
  )
where

import Common.LightYear
import Control.Monad.Combinators.Expr
import Data.List (groupBy, isPrefixOf, sortOn)
import Data.Ord (Down (..))
import Solcore.Diagnostics (SourceSpan)
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.Patterns (patListP)
import Solcore.Frontend.Parser.SolcoreTypes (atomTypeP, locatedFromSpans, locatedP, paramP, simpleNameP, typeP)
import Solcore.Frontend.Syntax.Location (sourceSpanOf)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

type BodyP = Parser [Stmt]

-- The expression parser is parameterised by the set of user-defined operators
-- in scope (collected by a pre-scan, see Parser.OperatorScan). They extend the
-- built-in operator table; a use of a user operator is desugared here to an
-- ordinary function call so the rest of the pipeline is unaffected.
exprP :: [OperatorDecl] -> BodyP -> Parser Exp
exprP ops bp = tyAnnP ops bp

tyAnnP :: [OperatorDecl] -> BodyP -> Parser Exp
tyAnnP ops bp = do
  e <- ternaryP ops bp
  option e $ do
    t <- colon *> typeP
    pure (locatedExpFrom [sourceSpanOf e, sourceSpanOf t] (TyExp e t))

ternaryP :: [OperatorDecl] -> BodyP -> Parser Exp
ternaryP ops bp =
  try (ifThenElseP ops bp) <|> do
    e1 <- binaryP ops bp
    option e1 $ do
      _ <- symbol "?"
      e2 <- ternaryP ops bp
      _ <- symbol ":"
      e3 <- ternaryP ops bp
      return (locatedExpFrom (map sourceSpanOf [e1, e2, e3]) (ExpCond e1 e2 e3))

ifThenElseP :: [OperatorDecl] -> BodyP -> Parser Exp
ifThenElseP ops bp = locatedP locatedExp $ do
  keyword "if"
  e1 <- ternaryP ops bp
  keyword "then"
  e2 <- ternaryP ops bp
  keyword "else"
  e3 <- ternaryP ops bp
  return (ExpCond e1 e2 e3)

binaryP :: [OperatorDecl] -> BodyP -> Parser Exp
binaryP ops bp = makeExprParser (postfixP ops bp) (mergedOpTable ops)

-- Build the operator table from the user-declared operators in scope (the
-- built-in operators are now ordinary declarations in the standard library).
-- Rows are grouped into precedence levels (highest first) so operators
-- interleave purely by their numeric precedence.
mergedOpTable :: [OperatorDecl] -> [[Operator Parser Exp]]
mergedOpTable ops =
  map (map snd)
    . groupBy (\a b -> fst a == fst b)
    . sortOn (Down . fst)
    $ map (userRow ops) ops

-- Turn a user operator declaration into a table row. A use of the operator is
-- desugared to a plain call of the bound function, so name resolution and the
-- rest of the pipeline handle it like any other call. The full set of operators
-- in scope is threaded through so the symbol parser can reason about longer
-- operators and operator-using match patterns.
userRow :: [OperatorDecl] -> OperatorDecl -> (Int, Operator Parser Exp)
userRow ops od@(OperatorDecl fix prec _ fun) =
  ( prec,
    case fix of
      OpInfixL -> InfixL (mkBin <$ opTok)
      OpInfixR -> InfixR (mkBin <$ opTok)
      OpInfixN -> InfixN (mkBin <$ opTok)
      OpPrefix -> Prefix (mkPre <$ opTok)
      OpPostfix -> Postfix (mkPre <$ opTok)
  )
  where
    opTok = try (opSymP ops od)
    mkBin l r = locatedExpFrom [sourceSpanOf l, sourceSpanOf r] (ExpName Nothing fun [l, r])
    mkPre e = locatedExpFrom [sourceSpanOf e] (ExpName Nothing fun [e])

-- Match an exact operator symbol under a maximal-munch guard, consulting every
-- operator in scope. A symbolic operator must not be consumed when the following
-- characters would complete either a longer declared operator (`^^` is matched
-- whole, not as `^` then `^`, when both are declared) or an assignment token
-- (`+` before `=` is the compound assignment `+=`, not the operator `+`). It may
-- still be immediately followed by an unrelated operator: `a & ~b` parses when
-- no `&~` operator is declared. An identifier operator (a postfix unit suffix
-- like `ether`) must not be a prefix of a longer identifier (`ether` must not
-- match inside `ethereum`), so it must not be followed by an identifier
-- character. The `|` symbol additionally must not be a match-arm separator
-- (`| pat => …`): the guard parses the arm pattern with the operators in scope,
-- so operator-using patterns (e.g. `comptime 1 + 1`) are recognised rather than
-- misparsed as bitwise-or.
opSymP :: [OperatorDecl] -> OperatorDecl -> Parser String
opSymP ops (OperatorDecl _ _ sym _) =
  lexeme (string sym <* munchGuard) <* matchArmGuard
  where
    munchGuard
      | all isOpChar sym = notFollowedBy longerContinuation
      | otherwise = notFollowedBy (alphaNumChar <|> char '_')
    -- Continuations that must block this symbol: the '=' of an assignment or
    -- compound-assignment token, and the tail of any longer symbolic operator
    -- that has `sym` as a strict prefix.
    longerContinuation = choice ((() <$ char '=') : map (\suf -> () <$ string suf) longerSuffixes)
    longerSuffixes =
      [ drop (length sym) s
      | OperatorDecl _ _ s _ <- ops,
        all isOpChar s,
        s /= sym,
        sym `isPrefixOf` s
      ]
    matchArmGuard
      | sym == "|" = notFollowedBy (try (patListP ops *> symbol "=>"))
      | otherwise = pure ()

postfixP :: [OperatorDecl] -> BodyP -> Parser Exp
postfixP ops bp = do
  e0 <- atomP ops bp
  fs <- many (postfixOp ops bp)
  return (foldl (\acc f -> f acc) e0 fs)

postfixOp :: [OperatorDecl] -> BodyP -> Parser (Exp -> Exp)
postfixOp ops bp = dotOp ops bp <|> idxOp ops bp

dotOp :: [OperatorDecl] -> BodyP -> Parser (Exp -> Exp)
dotOp ops bp = do
  _ <- char '.'
  sc
  n <- simpleNameP
  mArgs <- optional (parens (exprP ops bp `sepBy` comma))
  return $ case mArgs of
    Just args -> \e -> locatedExpFrom [sourceSpanOf e, sourceSpanOf n, sourceSpanOf args] (ExpName (Just e) n args)
    Nothing -> \e -> locatedExpFrom [sourceSpanOf e, sourceSpanOf n] (ExpVar (Just e) n)

idxOp :: [OperatorDecl] -> BodyP -> Parser (Exp -> Exp)
idxOp ops bp = do
  idx <- brackets (exprP ops bp)
  return (\e -> locatedExpFrom [sourceSpanOf e, sourceSpanOf idx] (ExpIndexed e idx))

atomP :: [OperatorDecl] -> BodyP -> Parser Exp
atomP ops bp = litP <|> try (lamP bp) <|> proxyP <|> try (dotNameP ops bp) <|> parenP ops bp <|> arrayLitP ops bp <|> nameP ops bp

-- Array literal, e.g. [1, 2, 3].  A leading '[' is unambiguous: postfix
-- indexing (idxOp) only consumes '[' after an atom has been parsed, so
-- `arr[0]` still parses as atom+postfix and `[1,2][0]` as literal+postfix.
arrayLitP :: [OperatorDecl] -> BodyP -> Parser Exp
arrayLitP ops bp = locatedP locatedExp (ExpArray <$> brackets (exprP ops bp `sepBy` comma))

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

dotNameP :: [OperatorDecl] -> BodyP -> Parser Exp
dotNameP ops bp = locatedP locatedExp $ do
  _ <- char '.'
  sc
  n <- simpleNameP
  args <- option [] (parens (exprP ops bp `sepBy` comma))
  return (ExpDotName n args)

parenP :: [OperatorDecl] -> BodyP -> Parser Exp
parenP ops bp = locatedP locatedExp $ parens $ do
  es <- exprP ops bp `sepBy` comma
  return $ case es of
    [] -> ExpName Nothing (Name "()") []
    [e] -> e
    _ -> foldr1 pairE es
  where
    pairE e1 e2 = locatedExpFrom [sourceSpanOf e1, sourceSpanOf e2] (ExpName Nothing (Name "pair") [e1, e2])

nameP :: [OperatorDecl] -> BodyP -> Parser Exp
nameP ops bp = locatedP locatedExp $ do
  n <- simpleNameP
  mArgs <- optional (parens (exprP ops bp `sepBy` comma))
  return $ case mArgs of
    Just args -> ExpName Nothing n args
    Nothing -> ExpVar Nothing n

locatedExpFrom :: [Maybe SourceSpan] -> Exp -> Exp
locatedExpFrom = locatedFromSpans locatedExp
