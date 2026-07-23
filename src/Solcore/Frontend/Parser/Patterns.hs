module Solcore.Frontend.Parser.Patterns
  ( patP,
    patListP,
    bindingTuplePatP,
  )
where

import Common.LightYear
import Control.Monad (when)
import Data.Set qualified as Set
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.Expr (exprP)
import Solcore.Frontend.Parser.SolcoreTypes (locatedP, qualifiedName, simpleNameP)
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

patP :: Parser Pat
patP = locatedP locatedPat (wildcardP <|> litP <|> dotPatP <|> parenPatP <|> try comptimePatP <|> namedPatP)

patListP :: Parser [Pat]
patListP = patP `sepBy1` comma

-- | A local destructuring binding is deliberately narrower than a match
-- pattern: every leaf is a fresh name (or @_@), and the outer shape must be a
-- tuple.  In particular, constructors and literal patterns are not accepted.
bindingTuplePatP :: Parser Pat
bindingTuplePatP = do
  pat <- locatedP locatedPat bindingTupleRawP
  let names = bindingNames pat
  when (length names /= Set.size (Set.fromList names)) $
    fail "duplicate names in destructuring binding"
  pure pat

bindingNames :: Pat -> [Name]
bindingNames PWildcard = []
bindingNames (Pat n []) = [n]
bindingNames (Pat _ ps) = concatMap bindingNames ps
bindingNames _ = []

bindingPatP :: Parser Pat
bindingPatP =
  locatedP locatedPat (wildcardP <|> bindingTupleRawP <|> bindingNameP)

bindingTupleRawP :: Parser Pat
bindingTupleRawP = parens $ do
  ps <- bindingPatP `sepBy1` comma
  when (length ps < 2) $
    fail "a destructuring binding requires at least two tuple elements"
  pure (Pat (Name "pair") ps)

bindingNameP :: Parser Pat
bindingNameP = do
  n <- simpleNameP
  pure (Pat n [])

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
