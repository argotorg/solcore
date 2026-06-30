module Solcore.Frontend.Parser.Patterns
  ( patP,
    patListP,
  )
where

import Common.LightYear
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.Expr (exprP)
import Solcore.Frontend.Parser.SolcoreTypes (locatedP, qualifiedName, simpleNameP)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

patP :: Parser Pat
patP = locatedP locatedPat (wildcardP <|> litP <|> dotPatP <|> parenPatP <|> try comptimePatP <|> namedPatP)

patListP :: Parser [Pat]
patListP = patP `sepBy1` comma

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

dotPatP :: Parser Pat
dotPatP = do
  _ <- char '.'
  sc
  n <- simpleNameP
  args <- option [] (parens (patP `sepBy1` comma))
  return (PatDot n args)

parenPatP :: Parser Pat
parenPatP = parens insideP
  where
    insideP = do
      ps <- patP `sepBy` comma
      return $ case ps of
        [] -> Pat (Name "()") []
        [p] -> p
        _ -> Pat (Name "pair") ps

namedPatP :: Parser Pat
namedPatP = do
  n <- qualifiedName
  args <- option [] (parens (patP `sepBy1` comma))
  return (Pat n args)

comptimePatP :: Parser Pat
comptimePatP = PExp <$> (keyword "comptime" *> exprP (return []))
