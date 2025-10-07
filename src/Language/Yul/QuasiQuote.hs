{-# LANGUAGE TemplateHaskell #-}
module Language.Yul.QuasiQuote where
import Common.LightYear
import Data.Data
import Data.Generics.Aliases
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Yul
import qualified Language.Yul.Parser as Parser

yulBlock :: QuasiQuoter
yulBlock = simpleQuoterUsing Parser.yulBlock

yulStmt :: QuasiQuoter
yulStmt = simpleQuoterUsing Parser.yulStmt

yulExp :: QuasiQuoter
yulExp = simpleQuoterUsing Parser.yulExp

simpleQuoterUsing :: Data a => Parser a -> QuasiQuoter
simpleQuoterUsing = simpleQuoter . quoteUsingParser

simpleQuoter :: (String -> Q Exp) -> QuasiQuoter
simpleQuoter q = QuasiQuoter
  { quoteExp = q
  , quotePat = undefined
  , quoteDec = undefined
  , quoteType = undefined
  }

quoteUsingParser :: Data a => Parser a -> String -> Q Exp
quoteUsingParser p s = do
  (file, _l, _c) <- getPosition
  let ast = runMyParser file (p <* eof) s
  dataToExpQ (const Nothing `extQ` antiYulExp) ast

getPosition = fmap transPos location where
  transPos loc = (loc_filename loc,
                  fst (loc_start loc),
                  snd (loc_start loc))

antiYulExp :: YulExp -> Maybe (Q Exp)
antiYulExp (YMeta v) = Just $ varE (mkName v)
antiYulExp _ = Nothing
