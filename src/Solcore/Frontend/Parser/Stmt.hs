module Solcore.Frontend.Parser.Stmt
  ( stmtP,
    bodyP,
  )
where

import Common.LightYear
import Control.Monad (void)
import Language.Yul.Parser (yulBlock)
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.Expr (exprP)
import Solcore.Frontend.Parser.Patterns (patListP)
import Solcore.Frontend.Parser.SolcoreTypes (typeP)
import Solcore.Frontend.Syntax.Name (Name (..))
import Solcore.Frontend.Syntax.SyntaxTree
  ( Body,
    Equation,
    Exp (..),
    Stmt (..),
  )

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
    <|> breakP
    <|> matchP
    <|> asmP
    <|> blockP
    <|> try exprOrAssignP

breakP :: Parser Stmt
breakP = Break <$ (keyword "break" *> semicolon)

letP :: Parser Stmt
letP = do
  keyword "let"
  n <- identifier
  (ct, mt) <- option (False, Nothing) $ do
    _ <- colon
    ct <- option False (True <$ keyword "comptime")
    t <- typeP
    return (ct, Just t)
  me <- optional (equalsP *> expP)
  _ <- semicolon
  return (Let ct (Name n) mt me)

returnP :: Parser Stmt
returnP = Return <$> (keyword "return" *> expP <* semicolon)

ifP :: Parser Stmt
ifP = do
  keyword "if"
  cond <- parens expP
  thenBody <- braces bodyP
  elseBody <- option [] (keyword "else" *> braces bodyP)
  return (If cond thenBody elseBody)

forP :: Parser Stmt
forP = do
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

matchP :: Parser Stmt
matchP = do
  keyword "match"
  scrutinees <- expP `sepBy1` comma
  eqns <- braces (many equationP)
  return (Match scrutinees eqns)

asmP :: Parser Stmt
asmP = Asm <$> (keyword "assembly" *> yulBlock) -- yulBlock includes the surrounding braces

blockP :: Parser Stmt
blockP = Block <$> braces bodyP

exprOrAssignP :: Parser Stmt
exprOrAssignP = do
  lhs <- expP
  choice
    [ do rhs <- equalsP *> expP; _ <- semicolon; return (Assign lhs rhs),
      do rhs <- symbol "+=" *> expP; _ <- semicolon; return (StmtPlusEq lhs rhs),
      do rhs <- symbol "-=" *> expP; _ <- semicolon; return (StmtMinusEq lhs rhs),
      StmtExp lhs <$ optional semicolon
    ]

forInitP :: Parser Stmt
forInitP = do
  stmts <- (forLetP <|> forAssignP) `sepBy` comma
  return $ case stmts of
    [] -> EmptyStmt
    [s] -> s
    ss -> Block ss

forPostP :: Parser Stmt
forPostP = do
  stmts <- forAssignP `sepBy` comma
  return $ case stmts of
    [] -> EmptyStmt
    [s] -> s
    ss -> Block ss

forLetP :: Parser Stmt
forLetP = do
  keyword "let"
  n <- identifier
  (ct, mt) <- option (False, Nothing) $ do
    _ <- colon
    ct <- option False (True <$ keyword "comptime")
    t <- typeP
    return (ct, Just t)
  me <- optional (equalsP *> expP)
  return (Let ct (Name n) mt me)

forAssignP :: Parser Stmt
forAssignP = do
  lhs <- expP
  choice
    [ do rhs <- equalsP *> expP; return (Assign lhs rhs),
      do rhs <- symbol "+=" *> expP; return (StmtPlusEq lhs rhs),
      do rhs <- symbol "-=" *> expP; return (StmtMinusEq lhs rhs),
      return (StmtExp lhs)
    ]

equationP :: Parser Equation
equationP = (,) <$> (symbol "|" *> patListP) <*> (symbol "=>" *> bodyP)

equalsP :: Parser ()
equalsP = void $ try (lexeme (char '=' <* notFollowedBy (char '=')))
