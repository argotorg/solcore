module Solcore.Frontend.Parser.Stmt
  ( stmtP,
    bodyP,
  )
where

import Common.LightYear
import Control.Monad (void, when)
import Language.Yul.Parser (yulBlock)
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.Expr (exprP)
import Solcore.Frontend.Parser.Patterns (bindingTuplePatP, patP)
import Solcore.Frontend.Parser.SolcoreTypes (locatedP, simpleNameP, typeP)
import Solcore.Frontend.Syntax.SyntaxTree

bodyP :: Parser Body
bodyP = many stmtP

expP :: Parser Exp
expP = exprP bodyP

stmtP :: Parser Stmt
stmtP =
  letP
    <|> returnP
    <|> try ifP
    <|> forP
    <|> whileP
    <|> breakP
    <|> continueP
    <|> matchP
    <|> asmP
    <|> uncheckedP
    <|> try revertP
    <|> blockP
    <|> try exprOrAssignP

breakP :: Parser Stmt
breakP = locatedP locatedStmt (Break <$ (keyword "break" *> semicolon))

continueP :: Parser Stmt
continueP = locatedP locatedStmt (Continue <$ (keyword "continue" *> semicolon))

letP :: Parser Stmt
letP = locatedP locatedStmt $ do
  keyword "let"
  ct <- option False (True <$ keyword "comptime")
  stmt <- letRemainderP ct
  _ <- semicolon
  pure stmt

letRemainderP :: Bool -> Parser Stmt
letRemainderP ct =
  try tupleLetRemainder <|> simpleLetRemainder
  where
    simpleLetRemainder = do
      n <- simpleNameP
      mt <- optional (colon *> typeP)
      me <- optional (equalsP *> expP)
      pure (Let ct n mt me)

    tupleLetRemainder = do
      pat <- bindingTuplePatP
      mt <- optional (colon *> typeP)
      value <- equalsP *> expP
      pure (LetPattern ct pat mt value)

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
  scrutinees <- parens (expP `sepBy1` comma)
  eqns <- braces (many (equationP (length scrutinees)))
  return (Match scrutinees eqns)

asmP :: Parser Stmt
asmP = locatedP locatedStmt (Asm <$> (keyword "assembly" *> yulBlock)) -- yulBlock includes the surrounding braces

uncheckedP :: Parser Stmt
uncheckedP =
  locatedP locatedStmt (Unchecked <$> (keyword "unchecked" *> braces bodyP))

revertP :: Parser Stmt
revertP =
  locatedP
    locatedStmt
    (Revert <$ (keyword "revert" *> semicolon))

blockP :: Parser Stmt
blockP = locatedP locatedStmt (Block <$> braces bodyP)

exprOrAssignP :: Parser Stmt
exprOrAssignP = locatedP locatedStmt $ do
  lhs <- expP
  choice
    [ do rhs <- equalsP *> expP; _ <- semicolon; return (Assign lhs rhs),
      do rhs <- symbol "+=" *> expP; _ <- semicolon; return (StmtPlusEq lhs rhs),
      do rhs <- symbol "-=" *> expP; _ <- semicolon; return (StmtMinusEq lhs rhs),
      do rhs <- symbol "^=" *> expP; _ <- semicolon; return (StmtBXorEq lhs rhs),
      do rhs <- symbol "&=" *> expP; _ <- semicolon; return (StmtBAndEq lhs rhs),
      do rhs <- symbol "|=" *> expP; _ <- semicolon; return (StmtBOrEq lhs rhs),
      do rhs <- symbol "%=" *> expP; _ <- semicolon; return (StmtModEq lhs rhs),
      StmtExp lhs <$ semicolon
    ]

forInitP :: Parser Stmt
forInitP = locatedP locatedStmt $ do
  stmts <- (forLetP <|> forAssignP) `sepBy` comma
  return $ case stmts of
    [] -> EmptyStmt
    [s] -> s
    ss -> Block ss

forPostP :: Parser Stmt
forPostP = locatedP locatedStmt $ do
  stmts <- forAssignP `sepBy` comma
  return $ case stmts of
    [] -> EmptyStmt
    [s] -> s
    ss -> Block ss

forLetP :: Parser Stmt
forLetP = locatedP locatedStmt $ do
  keyword "let"
  ct <- option False (True <$ keyword "comptime")
  letRemainderP ct

forAssignP :: Parser Stmt
forAssignP = locatedP locatedStmt $ do
  lhs <- expP
  choice
    [ do rhs <- equalsP *> expP; return (Assign lhs rhs),
      do rhs <- symbol "+=" *> expP; return (StmtPlusEq lhs rhs),
      do rhs <- symbol "-=" *> expP; return (StmtMinusEq lhs rhs),
      do rhs <- symbol "^=" *> expP; return (StmtBXorEq lhs rhs),
      do rhs <- symbol "&=" *> expP; return (StmtBAndEq lhs rhs),
      do rhs <- symbol "|=" *> expP; return (StmtBOrEq lhs rhs),
      do rhs <- symbol "%=" *> expP; return (StmtModEq lhs rhs),
      return (StmtExp lhs)
    ]

equationP :: Int -> Parser Equation
equationP arity =
  caseEquationP arity <|> defaultEquationP arity

caseEquationP :: Int -> Parser Equation
caseEquationP arity = do
  keyword "case"
  pats <-
    if arity == 1
      then (: []) <$> patP
      else parens (patP `sepBy1` comma)
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
