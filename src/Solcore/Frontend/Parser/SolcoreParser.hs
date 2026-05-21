module Solcore.Frontend.Parser.SolcoreParser
  ( parseCompUnit,
    moduleParser,
  )
where

import Common.LightYear
import Solcore.Frontend.Parser.Decl (compUnitP)
import Solcore.Frontend.Syntax.SyntaxTree (CompUnit)

parseCompUnit :: String -> IO (Either String CompUnit)
parseCompUnit = moduleParser []

moduleParser :: [String] -> String -> IO (Either String CompUnit)
moduleParser _dirs src =
  pure (runParserE compUnitP "<input>" src)
