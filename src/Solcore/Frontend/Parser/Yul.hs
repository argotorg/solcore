module Solcore.Frontend.Parser.Yul (sourceYulBlock) where

import Common.LightYear
import Control.Monad (when)
import Data.Char (isNumber)
import Language.Yul
import Solcore.Frontend.Lexer.SolcoreLexer
import Solcore.Frontend.Parser.SolcoreTypes (locatedP)
import Solcore.Frontend.Syntax.Name

-- Source assembly shares the Core lexer. Compiler templates use the separate
-- Language.Yul.Parser grammar, where quasiquote interpolation is available.
sourceYulBlock :: Parser YulBlock
sourceYulBlock = braces (many sourceYulStmt)

sourceYulStmt :: Parser YulStmt
sourceYulStmt = statement <* optional semicolon
  where
    statement =
      choice
        [ YBlock <$> sourceYulBlock,
          letP,
          YIf <$> (keyword "if" *> sourceYulExp) <*> sourceYulBlock,
          YFor <$> (keyword "for" *> sourceYulBlock) <*> sourceYulExp <*> sourceYulBlock <*> sourceYulBlock,
          switchP,
          functionP,
          try (YAssign <$> (sourceYulName `sepBy1` comma) <*> (symbol ":=" *> sourceYulExp)),
          YExp . YCall "return" <$> (keyword "return" *> parens (sourceYulExp `sepEndBy` comma)),
          YLeave <$ keyword "leave",
          YBreak <$ keyword "break",
          YContinue <$ keyword "continue",
          YExp <$> sourceYulExp
        ]

    letP = do
      keyword "let"
      names <- sourceYulName `sepBy1` comma
      value <- optional (symbol ":=" *> sourceYulExp)
      pure (YLet names value)

    switchP = do
      keyword "switch"
      value <- sourceYulExp
      cases <- some ((,) <$> (keyword "case" *> sourceYulLiteral) <*> sourceYulBlock)
      fallback <- optional (keyword "default" *> sourceYulBlock)
      pure (YSwitch value cases fallback)

    functionP = do
      keyword "function"
      funName <- sourceYulName
      args <- parens (sourceYulName `sepEndBy` comma)
      results <- optional (symbol "->" *> (sourceYulName `sepBy1` comma))
      YFun funName args results <$> sourceYulBlock

sourceYulExp :: Parser YulExp
sourceYulExp = YLit <$> sourceYulLiteral <|> nameOrCall
  where
    nameOrCall = do
      n <- sourceYulName
      args <- optional (parens (sourceYulExp `sepEndBy` comma))
      pure (maybe (YIdent n) (YCall n) args)

sourceYulLiteral :: Parser YLiteral
sourceYulLiteral =
  choice
    [ YulNumber <$> integer,
      YulString <$> stringLit,
      YulTrue <$ keyword "true",
      YulFalse <$ keyword "false"
    ]

sourceYulName :: Parser Name
sourceYulName = locatedP locatedName $ lexeme $ try $ do
  first <- letterChar <|> char '_' <|> char '$'
  rest <- many (letterChar <|> satisfy isNumber <|> char '_' <|> char '$')
  let spelling = first : rest
  when (spelling == "$") $ notFollowedBy (char '{')
  when (spelling /= "fallback" && spelling `elem` reservedWords) $
    fail ("reserved word used as Yul identifier: " ++ spelling)
  pure (Name spelling)
