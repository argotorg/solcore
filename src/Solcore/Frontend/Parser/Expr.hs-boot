module Solcore.Frontend.Parser.Expr
  ( exprP,
  )
where

import Common.LightYear (Parser)
import Solcore.Frontend.Syntax.SyntaxTree (Exp, Stmt)

exprP :: Parser [Stmt] -> Parser Exp
