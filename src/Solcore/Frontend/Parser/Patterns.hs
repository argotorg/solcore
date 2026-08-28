module Solcore.Frontend.Parser.Patterns
  ( patP,
    patListP,
  )
where

import Common.LightYear
import Solcore.Frontend.Lexer.SolcoreLexer
import {-# SOURCE #-} Solcore.Frontend.Parser.Expr (exprP)
import Solcore.Frontend.Parser.SolcoreTypes (locatedP, qualifiedName, simpleNameP)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

-- Patterns are parameterised by the user-defined operators in scope so that a
-- comptime pattern (which embeds an expression) can use operators.
patP :: [OperatorDecl] -> Parser Pat
patP ops =
  locatedP locatedPat (wildcardP <|> litP <|> dotPatP ops <|> parenPatP ops <|> try (comptimePatP ops) <|> namedPatP ops)

patListP :: [OperatorDecl] -> Parser [Pat]
patListP ops = patP ops `sepBy1` comma

wildcardP :: Parser Pat
wildcardP =
  PWildcard <$ lexeme (string "_" <* notFollowedBy (alphaNumChar <|> char '_'))

litP :: Parser Pat
litP =
  PLit . IntLit
    <$> integer
      <|> PLit
      . StrLit
    <$> stringLit

dotPatP :: [OperatorDecl] -> Parser Pat
dotPatP ops = do
  _ <- char '.'
  sc
  n <- simpleNameP
  args <- option [] (parens (patP ops `sepBy1` comma))
  return (PatDot n args)

parenPatP :: [OperatorDecl] -> Parser Pat
parenPatP ops = parens insideP
  where
    insideP = do
      ps <- patP ops `sepBy` comma
      return $ case ps of
        [] -> Pat (Name "()") []
        [p] -> p
        _ -> Pat (Name "pair") ps

namedPatP :: [OperatorDecl] -> Parser Pat
namedPatP ops = do
  n <- qualifiedName
  args <- option [] (parens (patP ops `sepBy1` comma))
  return (Pat n args)

comptimePatP :: [OperatorDecl] -> Parser Pat
comptimePatP ops = PExp <$> (keyword "comptime" *> exprP ops (return []))
