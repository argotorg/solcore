module Solcore.Frontend.Parser.Stmt
  ( stmtP,
    bodyP,
  )
where

import Common.LightYear
import Control.Monad (void)
import Language.Yul.Parser (yulBlock)
import Solcore.Diagnostics (SourceSpan)
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.Expr (exprP)
import Solcore.Frontend.Parser.Patterns (patListP)
import Solcore.Frontend.Parser.SolcoreTypes (locatedFromSpans, locatedP, simpleNameP, typeP)
import Solcore.Frontend.Syntax.Location (sourceSpanOf)
import Solcore.Frontend.Syntax.Name (Name)
import Solcore.Frontend.Syntax.SyntaxTree

bodyP :: [OperatorDecl] -> Parser Body
bodyP ops = many (stmtP ops)

expP :: [OperatorDecl] -> Parser Exp
expP ops = exprP ops (bodyP ops)

stmtP :: [OperatorDecl] -> Parser Stmt
stmtP ops =
  letP ops
    <|> returnP ops
    <|> try (ifP ops)
    <|> forP ops
    <|> breakP
    <|> continueP
    <|> matchP ops
    <|> asmP
    <|> blockP ops
    <|> try (exprOrAssignP ops)

breakP :: Parser Stmt
breakP = locatedP locatedStmt (Break <$ (keyword "break" *> semicolon))

continueP :: Parser Stmt
continueP = locatedP locatedStmt (Continue <$ (keyword "continue" *> semicolon))

letP :: [OperatorDecl] -> Parser Stmt
letP ops = locatedP locatedStmt $ do
  keyword "let"
  n <- simpleNameP
  (ct, mt) <- option (False, Nothing) $ do
    _ <- colon
    ct <- option False (True <$ keyword "comptime")
    t <- typeP
    return (ct, Just t)
  me <- optional (equalsP *> expP ops)
  _ <- semicolon
  return (Let ct n mt me)

returnP :: [OperatorDecl] -> Parser Stmt
returnP ops = locatedP locatedStmt (Return <$> (keyword "return" *> expP ops <* semicolon))

ifP :: [OperatorDecl] -> Parser Stmt
ifP ops = locatedP locatedStmt $ do
  keyword "if"
  cond <- parens (expP ops)
  thenBody <- braces (bodyP ops)
  elseBody <- option [] (keyword "else" *> braces (bodyP ops))
  return (If cond thenBody elseBody)

forP :: [OperatorDecl] -> Parser Stmt
forP ops = locatedP locatedStmt $ do
  keyword "for"
  (initS, cond, postS) <- parens $ do
    initS <- forInitP ops
    _ <- semicolon
    cond <- expP ops
    _ <- semicolon
    postS <- forPostP ops
    return (initS, cond, postS)
  body <- braces (bodyP ops)
  return (For initS cond postS body)

matchP :: [OperatorDecl] -> Parser Stmt
matchP ops = locatedP locatedStmt $ do
  keyword "match"
  scrutinees <- expP ops `sepBy1` comma
  eqns <- braces (many (equationP ops))
  return (Match scrutinees eqns)

asmP :: Parser Stmt
asmP = locatedP locatedStmt (Asm <$> (keyword "assembly" *> yulBlock)) -- yulBlock includes the surrounding braces

blockP :: [OperatorDecl] -> Parser Stmt
blockP ops = locatedP locatedStmt (Block <$> braces (bodyP ops))

exprOrAssignP :: [OperatorDecl] -> Parser Stmt
exprOrAssignP ops = locatedP locatedStmt $ do
  lhs <- expP ops
  choice
    [ do rhs <- equalsP *> expP ops; _ <- semicolon; return (Assign lhs rhs),
      do rhs <- symbol "+=" *> expP ops; _ <- semicolon; compoundAssign ops "+" lhs rhs,
      do rhs <- symbol "-=" *> expP ops; _ <- semicolon; compoundAssign ops "-" lhs rhs,
      do rhs <- symbol "*=" *> expP ops; _ <- semicolon; compoundAssign ops "*" lhs rhs,
      do rhs <- symbol "/=" *> expP ops; _ <- semicolon; compoundAssign ops "/" lhs rhs,
      do rhs <- symbol "^=" *> expP ops; _ <- semicolon; compoundAssign ops "^" lhs rhs,
      do rhs <- symbol "&=" *> expP ops; _ <- semicolon; compoundAssign ops "&" lhs rhs,
      do rhs <- symbol "|=" *> expP ops; _ <- semicolon; compoundAssign ops "|" lhs rhs,
      do rhs <- symbol "%=" *> expP ops; _ <- semicolon; compoundAssign ops "%" lhs rhs,
      do _ <- symbol "~="; _ <- semicolon; compoundAssignUnary ops "~" lhs,
      StmtExp lhs <$ optional semicolon
    ]

-- A compound assignment lhs <op>= rhs desugars to lhs = <op>(lhs, rhs)
-- using the operator bound to <op> in scope; there are no built-in operators,
-- so the base operator must be declared (e.g. imported from the standard
-- library). lhs is duplicated into both the assignment target and the call.
-- The base operator of a binary compound assignment (`+=`, `%=`, …) must be an
-- infix operator: it is applied to two arguments (lhs and rhs). Rejecting a
-- prefix/postfix operator here avoids emitting a two-argument call to a
-- one-argument function, which would only surface as an obscure arity error
-- much later in the pipeline.
compoundAssign :: [OperatorDecl] -> String -> Exp -> Exp -> Parser Stmt
compoundAssign ops sym lhs rhs =
  case filter ((== sym) . opSymbol) ops of
    (od : _)
      | isInfixFixity (opFixity od) ->
          pure (Assign lhs (opCall (opFunction od) [lhs, rhs]))
      | otherwise ->
          fail ("operator (" ++ sym ++ ") is not infix, so it cannot be used with '" ++ sym ++ "='")
    [] -> fail ("operator (" ++ sym ++ ") must be in scope to use '" ++ sym ++ "='")

-- A unary compound assignment `lhs <op>=` desugars to `lhs = <op>(lhs)` using
-- the (prefix/postfix) operator bound to <op> in scope. Used for `~=`, the
-- in-place bitwise NOT: `lhs ~=` becomes `lhs = ~lhs`. As with compoundAssign,
-- the base operator must be declared (there are no built-in operators) and, so
-- that the single-argument call is well formed, it must be unary.
compoundAssignUnary :: [OperatorDecl] -> String -> Exp -> Parser Stmt
compoundAssignUnary ops sym lhs =
  case filter ((== sym) . opSymbol) ops of
    (od : _)
      | isUnaryFixity (opFixity od) ->
          pure (Assign lhs (opCall (opFunction od) [lhs]))
      | otherwise ->
          fail ("operator (" ++ sym ++ ") is not prefix/postfix, so it cannot be used with '" ++ sym ++ "='")
    [] -> fail ("operator (" ++ sym ++ ") must be in scope to use '" ++ sym ++ "='")

-- Build the desugared operator call, keeping the source span of the operands so
-- diagnostics point back at the compound assignment rather than losing location.
opCall :: Name -> [Exp] -> Exp
opCall fun args = locatedExpFrom (map sourceSpanOf args) (ExpName Nothing fun args)

locatedExpFrom :: [Maybe SourceSpan] -> Exp -> Exp
locatedExpFrom = locatedFromSpans locatedExp

isInfixFixity :: OpFixity -> Bool
isInfixFixity OpInfixL = True
isInfixFixity OpInfixR = True
isInfixFixity OpInfixN = True
isInfixFixity _ = False

isUnaryFixity :: OpFixity -> Bool
isUnaryFixity OpPrefix = True
isUnaryFixity OpPostfix = True
isUnaryFixity _ = False

forInitP :: [OperatorDecl] -> Parser Stmt
forInitP ops = locatedP locatedStmt $ do
  stmts <- (forLetP ops <|> forAssignP ops) `sepBy` comma
  return $ case stmts of
    [] -> EmptyStmt
    [s] -> s
    ss -> Block ss

forPostP :: [OperatorDecl] -> Parser Stmt
forPostP ops = locatedP locatedStmt $ do
  stmts <- forAssignP ops `sepBy` comma
  return $ case stmts of
    [] -> EmptyStmt
    [s] -> s
    ss -> Block ss

forLetP :: [OperatorDecl] -> Parser Stmt
forLetP ops = locatedP locatedStmt $ do
  keyword "let"
  n <- simpleNameP
  (ct, mt) <- option (False, Nothing) $ do
    _ <- colon
    ct <- option False (True <$ keyword "comptime")
    t <- typeP
    return (ct, Just t)
  me <- optional (equalsP *> expP ops)
  return (Let ct n mt me)

forAssignP :: [OperatorDecl] -> Parser Stmt
forAssignP ops = locatedP locatedStmt $ do
  lhs <- expP ops
  choice
    [ do rhs <- equalsP *> expP ops; return (Assign lhs rhs),
      do rhs <- symbol "+=" *> expP ops; compoundAssign ops "+" lhs rhs,
      do rhs <- symbol "-=" *> expP ops; compoundAssign ops "-" lhs rhs,
      do rhs <- symbol "*=" *> expP ops; compoundAssign ops "*" lhs rhs,
      do rhs <- symbol "/=" *> expP ops; compoundAssign ops "/" lhs rhs,
      do rhs <- symbol "^=" *> expP ops; compoundAssign ops "^" lhs rhs,
      do rhs <- symbol "&=" *> expP ops; compoundAssign ops "&" lhs rhs,
      do rhs <- symbol "|=" *> expP ops; compoundAssign ops "|" lhs rhs,
      do rhs <- symbol "%=" *> expP ops; compoundAssign ops "%" lhs rhs,
      do _ <- symbol "~="; compoundAssignUnary ops "~" lhs,
      return (StmtExp lhs)
    ]

equationP :: [OperatorDecl] -> Parser Equation
equationP ops = (,) <$> (symbol "|" *> patListP ops) <*> (symbol "=>" *> bodyP ops)

equalsP :: Parser ()
equalsP = void $ try (lexeme (char '=' <* notFollowedBy (char '=')))
