module Solcore.Frontend.Parser.SolcoreTypes
  ( qualifiedName,
    typeP,
    atomTypeP,
    predP,
    predListP,
    paramP,
    typeParamsP,
    whereClauseP,
    simpleNameP,
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
typeP = locatedP locatedTy postfixTypeP

atomTypeP :: Parser Ty
atomTypeP = locatedP locatedTy (mappingTypeP <|> parenTypeP <|> namedTypeP)

postfixTypeP :: Parser Ty
postfixTypeP = do
  base <- functionTypeP <|> atomTypeP
  suffixes <- many typeSuffixP
  pure (foldl (flip ($)) base suffixes)

typeSuffixP :: Parser (Ty -> Ty)
typeSuffixP =
  choice
    [ do
        size <- brackets (optional arraySizeP)
        pure $ \elementTy ->
          case size of
            Nothing -> TyCon "array" [elementTy]
            Just sizeTy -> TyCon "array" [sizeTy, elementTy],
      TyCon "memory" . (: []) <$ keyword "memory",
      TyCon "storage" . (: []) <$ keyword "storage",
      TyCon "calldata" . (: []) <$ keyword "calldata"
    ]

arraySizeP :: Parser Ty
arraySizeP =
  (do n <- integer; pure (TyCon (Name (show n)) []))
    <|> typeP

namedTypeP :: Parser Ty
namedTypeP = TyCon <$> qualifiedName <*> option [] (angles (typeP `sepBy1` comma))

parenTypeP :: Parser Ty
parenTypeP = parens (mkParenTy <$> (typeP `sepBy` comma))
  where
    mkParenTy [] = TyCon "()" []
    mkParenTy [t] = t
    mkParenTy ts = foldr1 pairTy ts

mappingTypeP :: Parser Ty
mappingTypeP = do
  keyword "mapping"
  (keyTy, valueTy) <- parens $ do
    keyTy <- typeP
    _ <- symbol "=>"
    valueTy <- typeP
    pure (keyTy, valueTy)
  pure (TyCon "mapping" [keyTy, valueTy])

functionTypeP :: Parser Ty
functionTypeP = do
  keyword "function"
  args <- parens (typeP `sepBy` comma)
  visibility <-
    optional
      ( FunctionTypeInternal <$ keyword "internal"
          <|> FunctionTypeExternal <$ keyword "external"
      )
  results <- optional returnsTypeP
  pure (FunctionTy args visibility results)

returnsTypeP :: Parser [Ty]
returnsTypeP = do
  keyword "returns"
  parens (typeP `sepBy` comma)

predP :: Parser Pred
predP = do
  subjectTy <- typeP
  _ <- colon
  cls <- qualifiedName
  params <- option [] (angles (typeP `sepBy1` comma))
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

typeParamsP :: Parser [Ty]
typeParamsP =
  option [] (angles (tyVar `sepBy1` comma))
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
