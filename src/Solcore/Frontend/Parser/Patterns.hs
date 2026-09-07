module Solcore.Frontend.Parser.Patterns
  ( patP,
    patWithBodyP,
    patListP,
    matchPatListP,
  )
where

import Common.LightYear
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.Expr (exprP)
import Solcore.Frontend.Parser.SolcoreTypes (booleanNameP, locatedP, qualifiedName, simpleNameP)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

patP :: Parser Pat
patP = patWithBodyP (pure [])

patWithBodyP :: Parser Body -> Parser Pat
patWithBodyP body = fst <$> patShapeP body

-- Preserve tuple syntax until a multi-scrutinee case has split its patterns.
-- A constructor actually named `pair` must remain one pattern.
patShapeP :: Parser Body -> Parser (Pat, Maybe [Pat])
patShapeP body =
  locatedP locateShape $
    parenPatShapeP body
      <|> ((,Nothing) <$> (wildcardP <|> litP <|> dotPatP body <|> try (comptimePatP body) <|> namedPatP body))
  where
    locateShape sourceSpan (pat, tupleItems) = (locatedPat sourceSpan pat, tupleItems)

matchPatListP :: Parser Body -> Int -> Parser [Pat]
matchPatListP body arity = do
  (pat, tupleItems) <- patShapeP body
  pure $ if arity > 1 then maybe [pat] id tupleItems else [pat]

patListP :: Parser [Pat]
patListP = patP `sepBy1` comma

wildcardP :: Parser Pat
wildcardP =
  PWildcard
    <$ lexeme
      (try (string "_" <* notFollowedBy (alphaNumChar <|> char '_')))

litP :: Parser Pat
litP =
  (\n -> Pat n [])
    <$> booleanNameP
      <|> PLit
      . IntLit
    <$> integer
      <|> PLit
      . StrLit
    <$> stringLit

dotPatP :: Parser Body -> Parser Pat
dotPatP body = do
  _ <- char '.'
  sc
  n <- booleanNameP <|> simpleNameP
  args <- option [] (parens (patWithBodyP body `sepBy1` comma))
  return (PatDot n args)

parenPatShapeP :: Parser Body -> Parser (Pat, Maybe [Pat])
parenPatShapeP body = parens insideP
  where
    insideP = do
      shapes <- patShapeP body `sepEndBy` comma
      return $ case shapes of
        [] -> (Pat (Name "()") [], Just [])
        [shape] -> shape
        _ -> let ps = map fst shapes in (Pat (Name "pair") ps, Just ps)

namedPatP :: Parser Body -> Parser Pat
namedPatP body = do
  n <- qualifiedName
  args <- option [] (parens (patWithBodyP body `sepBy1` comma))
  return (Pat n args)

comptimePatP :: Parser Body -> Parser Pat
comptimePatP body = PExp <$> (keyword "comptime" *> exprP body)
