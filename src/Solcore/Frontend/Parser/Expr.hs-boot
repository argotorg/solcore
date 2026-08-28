module Solcore.Frontend.Parser.Expr
  ( exprP,
  )
where

import Common.LightYear (Parser)
import Solcore.Frontend.Syntax.SyntaxTree (Exp, OperatorDecl, Stmt)

exprP :: [OperatorDecl] -> Parser [Stmt] -> Parser Exp
