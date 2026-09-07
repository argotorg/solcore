module Solcore.Frontend.Parser.Stmt
  ( stmtP,
    bodyP,
    namedBodyP,
  )
where

import Common.LightYear
import Control.Monad (void, when)
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.Expr (exprP)
import Solcore.Frontend.Parser.Patterns (matchPatListP)
import Solcore.Frontend.Parser.SolcoreTypes (locatedP, simpleNameP, typeP)
import Solcore.Frontend.Parser.Yul (sourceYulBlock)
import Solcore.Frontend.Syntax.SyntaxTree

bodyP :: Parser Body
bodyP = many stmtP

-- Only a named function's final expression may omit its semicolon. Nested
-- blocks and lambda, constructor, and fallback bodies use the ordinary body.
namedBodyP :: Parser Body
namedBodyP = do
  stmts <- bodyP
  tailReturn <- optional $ locatedP locatedStmt $ do
    value <- expP
    _ <- lookAhead (symbol "}")
    pure (Return value)
  pure (stmts ++ maybe [] (: []) tailReturn)

expP :: Parser Exp
expP = exprP bodyP

stmtP :: Parser Stmt
stmtP =
  letP
    <|> returnP
    <|> try ifP
    <|> forP
    <|> try whileP
    <|> breakP
    <|> continueP
    <|> matchP
    <|> asmP
    <|> blockP
    <|> try exprOrAssignP

breakP :: Parser Stmt
breakP = locatedP locatedStmt (Break <$ (keyword "break" *> semicolon))

continueP :: Parser Stmt
continueP = locatedP locatedStmt (Continue <$ (keyword "continue" *> semicolon))

letP :: Parser Stmt
letP = locatedP locatedStmt $ do
  keyword "let"
  stmt <- letRemainderP
  _ <- semicolon
  pure stmt

letRemainderP :: Parser Stmt
letRemainderP = do
  n <- simpleNameP
  mt <- optional (colon *> typeP)
  me <- optional (equalsP *> expP)
  pure $ case mt of
    Just (TyCon "comptime" [ty]) -> Let True n (Just ty) me
    _ -> Let False n mt me

returnP :: Parser Stmt
returnP = locatedP locatedStmt $ do
  keyword "return"
  value <- optional expP
  _ <- semicolon
  pure (maybe BareReturn Return value)

ifP :: Parser Stmt
ifP = locatedP locatedStmt $ do
  keyword "if"
  cond <- parens expP
  thenBody <- braces bodyP
  elseBody <- option [] (keyword "else" *> braces bodyP)
  return (If cond thenBody elseBody)

forP :: Parser Stmt
forP = locatedP locatedStmt $ do
  keyword "for"
  (initS, cond, postS) <- parens $ do
    initS <- forInitP
    _ <- semicolon
    cond <- expP
    _ <- semicolon
    postS <- forPostP
    return (initS, cond, postS)
  body <- braces bodyP
  return (For initS cond postS body)

whileP :: Parser Stmt
whileP = locatedP locatedStmt $ do
  keyword "while"
  cond <- parens expP
  body <- braces bodyP
  pure (While cond body)

matchP :: Parser Stmt
matchP = locatedP locatedStmt $ do
  keyword "match"
  scrutinees <- parens (expP `sepEndBy1` comma)
  eqns <- braces $ do
    cases <- many (caseEquationP (length scrutinees))
    defaultCase <- optional (defaultEquationP (length scrutinees))
    let eqns = cases ++ maybe [] (: []) defaultCase
    when (null eqns) $
      fail "match requires at least one case or default arm"
    pure eqns
  return (Match scrutinees eqns)

asmP :: Parser Stmt
asmP = locatedP locatedStmt (Asm <$> (keyword "assembly" *> sourceYulBlock))

blockP :: Parser Stmt
blockP = locatedP locatedStmt (Block <$> braces bodyP)

exprOrAssignP :: Parser Stmt
exprOrAssignP = locatedP locatedStmt $ do
  stmt <- assignOrExprP
  _ <- semicolon
  pure stmt

forInitP :: Parser Stmt
forInitP = locatedP locatedStmt $ do
  stmts <- (forLetP <|> forAssignP) `sepBy` comma
  return $ case stmts of
    [] -> EmptyStmt
    [s] -> s
    ss -> Block ss

forPostP :: Parser Stmt
forPostP = forInitP

forLetP :: Parser Stmt
forLetP = locatedP locatedStmt $ do
  keyword "let"
  letRemainderP

forAssignP :: Parser Stmt
forAssignP = locatedP locatedStmt assignOrExprP

assignOrExprP :: Parser Stmt
assignOrExprP = do
  lhs <- expP
  choice
    [ do rhs <- equalsP *> expP; return (Assign lhs rhs),
      do rhs <- symbol "+=" *> expP; return (StmtPlusEq lhs rhs),
      do rhs <- symbol "-=" *> expP; return (StmtMinusEq lhs rhs),
      do rhs <- symbol "*=" *> expP; return (StmtTimesEq lhs rhs),
      do rhs <- symbol "/=" *> expP; return (StmtDivideEq lhs rhs),
      do rhs <- symbol "^=" *> expP; return (StmtBXorEq lhs rhs),
      do rhs <- symbol "&=" *> expP; return (StmtBAndEq lhs rhs),
      do rhs <- symbol "|=" *> expP; return (StmtBOrEq lhs rhs),
      do rhs <- symbol "%=" *> expP; return (StmtModEq lhs rhs),
      StmtBNotEq lhs <$ symbol "~=",
      return (StmtExp lhs)
    ]

caseEquationP :: Int -> Parser Equation
caseEquationP arity = do
  keyword "case"
  pats <- matchPatListP bodyP arity
  when (length pats /= arity) $
    fail "case pattern count must match the number of match scrutinees"
  body <- braces bodyP
  pure (pats, body)

defaultEquationP :: Int -> Parser Equation
defaultEquationP arity = do
  keyword "default"
  body <- braces bodyP
  pure (replicate arity PWildcard, body)

equalsP :: Parser ()
equalsP = void $ try (lexeme (char '=' <* notFollowedBy (char '=')))
