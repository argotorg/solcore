module Solcore.Frontend.Parser.SolcoreTypes
  ( qualifiedName,
    typeP,
    atomTypeP,
    predP,
    predListP,
    paramP,
    sigPrefixP,
  )
where

import Common.LightYear
import Control.Monad.Combinators.Expr
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Syntax.Name (Name (..))
import Solcore.Frontend.Syntax.SyntaxTree
  ( Param (..),
    Pred (..),
    Ty (..),
    pairTy,
  )

qualifiedName :: Parser Name
qualifiedName = do
  h <- identifier
  ts <- many (try (char '.' *> identifier))
  return (foldl QualName (Name h) ts)

typeP :: Parser Ty
typeP = makeExprParser atomTypeP [[InfixR (mkArrowTy <$ symbol "->")]]
  where
    mkArrowTy t1 t2 = TyCon "->" [t1, t2]

atomTypeP :: Parser Ty
atomTypeP = proxyTypeP <|> parenTypeP <|> namedTypeP

proxyTypeP :: Parser Ty
proxyTypeP = TyCon "Proxy" . (: []) <$> (symbol "@" *> atomTypeP)

namedTypeP :: Parser Ty
namedTypeP = TyCon <$> qualifiedName <*> option [] (parens (typeP `sepBy1` comma))

parenTypeP :: Parser Ty
parenTypeP = parens (mkParenTy <$> (typeP `sepBy` comma))
  where
    mkParenTy [] = TyCon "()" []
    mkParenTy [t] = t
    mkParenTy ts = foldr1 pairTy ts

predP :: Parser Pred
predP = do
  mainTy <- atomTypeP
  _ <- colon
  cls <- qualifiedName
  params <- option [] (parens (typeP `sepBy1` comma))
  return (InCls cls mainTy params)

predListP :: Parser [Pred]
predListP = predP `sepBy1` comma

paramP :: Parser Param
paramP = do
  n <- identifier
  mt <- optional (colon *> typeP)
  return $ case mt of
    Just t -> Typed (Name n) t
    Nothing -> Untyped (Name n)

sigPrefixP :: Parser ([Ty], [Pred])
sigPrefixP = do
  keyword "forall"
  vars <- some (tyVar <* optional comma)
  _ <- symbol "."
  ctx <- option [] $ try (predListP <* symbol "=>")
  return (vars, ctx)
  where
    tyVar = (\n -> TyCon (Name n) []) <$> identifier
