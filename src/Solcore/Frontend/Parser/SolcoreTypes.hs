module Solcore.Frontend.Parser.SolcoreTypes
  ( qualifiedName,
    typeP,
    atomTypeP,
    predP,
    predListP,
    paramP,
    namedParamP,
    lambdaParamP,
    typeParamsP,
    whereClauseP,
    simpleNameP,
    booleanNameP,
    locatedP,
    locatedFromSpans,
  )
where

import Common.LightYear
import Data.Foldable (foldlM)
import Solcore.Diagnostics (SourceSpan (..))
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Syntax.Location
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.SyntaxTree

qualifiedName :: Parser Name
qualifiedName = do
  h <- simpleNameP
  foldlM segment h =<< many (try (symbol "." *> locatedIdentifierP))
  where
    segment qualifier (sourceSpan, leaf) =
      pure (locatedQualName qualifier sourceSpan leaf)

simpleNameP :: Parser Name
simpleNameP =
  uncurry (\sourceSpan identifierText -> locatedName sourceSpan (Name identifierText)) <$> locatedIdentifierP

booleanNameP :: Parser Name
booleanNameP =
  locatedP locatedName $
    Name "true"
      <$ keyword "true"
        <|> Name "false"
      <$ keyword "false"

locatedIdentifierP :: Parser (SourceSpan, String)
locatedIdentifierP = do
  startPos <- getSourcePos
  startOffset <- getOffset
  identifierText <- identifier
  endPos <- getSourcePos
  endOffset <- getOffset
  pure (sourceSpanBetween startOffset startPos endOffset endPos, identifierText)

typeP :: Parser Ty
typeP =
  locatedP
    locatedTy
    (functionTypeP <|> try comptimeTypeP <|> mappingTypeP <|> proxyTypeP <|> parenTypeP <|> namedTypeP)

atomTypeP :: Parser Ty
atomTypeP = typeP

namedTypeP :: Parser Ty
namedTypeP = do
  name <- qualifiedName
  args <- option [] (try (angles (typeP `sepEndBy1` comma)))
  if name == Name "mapping"
    then fail "the mapping type uses mapping(Key => Value)"
    else pure (TyCon name args)

parenTypeP :: Parser Ty
parenTypeP = parens (mkParenTy <$> (typeP `sepEndBy` comma))
  where
    mkParenTy [] = TyCon "()" []
    mkParenTy [t] = t
    mkParenTy ts = foldr1 pairTy ts

mappingTypeP :: Parser Ty
mappingTypeP = do
  _ <- try (keyword "mapping" <* lookAhead (symbol "("))
  (keyTy, valueTy) <- parens $ do
    keyTy <- typeP
    _ <- symbol "=>"
    valueTy <- typeP
    pure (keyTy, valueTy)
  pure (TyCon "mapping" [keyTy, valueTy])

comptimeTypeP :: Parser Ty
comptimeTypeP = do
  _ <- try (keyword "comptime" <* lookAhead (symbol "<"))
  inner <- angles typeP
  pure (TyCon "comptime" [inner])

proxyTypeP :: Parser Ty
proxyTypeP = TyCon "Proxy" . (: []) <$> (symbol "@" *> typeP)

functionTypeP :: Parser Ty
functionTypeP = do
  keyword "function"
  args <- parens (typeP `sepEndBy` comma)
  results <- optional returnsTypeP
  pure (FunctionTy args Nothing results)

returnsTypeP :: Parser [Ty]
returnsTypeP = do
  keyword "returns"
  parens (typeP `sepEndBy` comma)

predP :: Parser Pred
predP = do
  subjectTy <- typeP
  _ <- colon
  cls <- simpleNameP
  params <- option [] (angles (typeP `sepEndBy1` comma))
  return (InCls cls subjectTy params)

predListP :: Parser [Pred]
predListP = try (parens barePredListP) <|> barePredListP
  where
    barePredListP = predP `sepEndBy1` comma

paramP :: Parser Param
paramP = do
  ct <- option False (True <$ keyword "comptime")
  n <- simpleNameP
  mt <- optional (colon *> typeP)
  case mt of
    Just (TyCon "comptime" [_]) ->
      fail "comptime<T> is not a parameter type; write comptime name: T"
    Just t -> pure (Typed ct n t)
    Nothing -> pure (Untyped ct n)

namedParamP :: Parser Param
namedParamP = do
  parameter <- paramP
  case parameter of
    Untyped _ _ -> fail "named function parameter requires an explicit type"
    _ -> pure parameter

lambdaParamP :: Parser Param
lambdaParamP = do
  parameter <- paramP
  case parameter of
    Untyped True _ -> fail "comptime parameter requires an explicit type"
    _ -> pure parameter

typeParamsP :: Parser [Ty]
typeParamsP =
  option [] (angles (tyVar `sepEndBy1` comma))
  where
    tyVar = locatedP locatedTy (flip TyCon [] <$> simpleNameP)

whereClauseP :: Parser [Pred]
whereClauseP =
  option [] (keyword "where" *> predListP)

angles :: Parser a -> Parser a
angles = between (symbol "<") (symbol ">")

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
