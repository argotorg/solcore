module Solcore.Frontend.Parser.SolcoreTypes
  ( qualifiedName,
    typeP,
    atomTypeP,
    predP,
    predListP,
    paramP,
    sigPrefixP,
    simpleNameP,
    locatedP,
    locatedFromSpans,
  )
where

import Common.LightYear
import Control.Monad.Combinators.Expr
import Data.Foldable (foldlM)
import Solcore.Diagnostics (SourceSpan (..))
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Syntax.Location
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

qualifiedName :: Parser Name
qualifiedName = do
  h <- simpleNameP
  foldlM segment h =<< many (try (char '.' *> locatedIdentifierP))
  where
    segment qualifier (sourceSpan, leaf) =
      pure (locatedQualName qualifier sourceSpan leaf)

simpleNameP :: Parser Name
simpleNameP =
  uncurry (\sourceSpan identifierText -> locatedName sourceSpan (Name identifierText)) <$> locatedIdentifierP

locatedIdentifierP :: Parser (SourceSpan, String)
locatedIdentifierP = do
  startPos <- getSourcePos
  startOffset <- getOffset
  identifierText <- identifier
  endPos <- getSourcePos
  endOffset <- getOffset
  pure (sourceSpanBetween startOffset startPos endOffset endPos, identifierText)

typeP :: Parser Ty
typeP = locatedP locatedTy (makeExprParser atomTypeP [[InfixR (mkArrowTy <$ symbol "->")]])
  where
    mkArrowTy t1 t2 =
      locatedFromSpans locatedTy [sourceSpanOf t1, sourceSpanOf t2] (TyCon "->" [t1, t2])

atomTypeP :: Parser Ty
atomTypeP = locatedP locatedTy (proxyTypeP <|> parenTypeP <|> namedTypeP)

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
  subjectTy <- atomTypeP
  _ <- colon
  cls <- qualifiedName
  params <- option [] (parens (typeP `sepBy1` comma))
  return (InCls cls subjectTy params)

predListP :: Parser [Pred]
predListP = predP `sepBy1` comma

paramP :: Parser Param
paramP = do
  ct <- option False (True <$ keyword "comptime")
  n <- simpleNameP
  mt <- optional (colon *> typeP)
  return $ case mt of
    Just t -> Typed ct n t
    Nothing -> Untyped ct n

sigPrefixP :: Parser ([Ty], [Pred])
sigPrefixP = do
  keyword "forall"
  vars <- some (tyVar <* optional comma)
  _ <- symbol "."
  ctx <- option [] $ try (predListP <* symbol "=>")
  return (vars, ctx)
  where
    tyVar = locatedP locatedTy (flip TyCon [] <$> simpleNameP)

locatedP :: (SourceSpan -> a -> a) -> Parser a -> Parser a
locatedP locate parser = do
  startPos <- getSourcePos
  startOffset <- getOffset
  value <- parser
  endPos <- getSourcePos
  endOffset <- getOffset
  pure (locate (sourceSpanBetween startOffset startPos endOffset endPos) value)

locatedFromSpans :: (SourceSpan -> a -> a) -> [Maybe SourceSpan] -> a -> a
locatedFromSpans locate spans value =
  maybe value (`locate` value) (foldr combineMaybeSourceSpans Nothing spans)

sourceSpanBetween :: Int -> SourcePos -> Int -> SourcePos -> SourceSpan
sourceSpanBetween startOffset startPos endOffset endPos =
  SourceSpan
    { spanFile = sourceName startPos,
      spanStartByte = startOffset,
      spanEndByte = endOffset,
      spanStartLine = unPos (sourceLine startPos),
      spanStartColumn = unPos (sourceColumn startPos),
      spanEndLine = unPos (sourceLine endPos),
      spanEndColumn = unPos (sourceColumn endPos)
    }
