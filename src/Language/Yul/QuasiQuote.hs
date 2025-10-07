module Language.Yul.QuasiQuote where
import Common.LightYear
import Data.Data
import Language.Haskell.TH as TH
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Language.Yul
import qualified Language.Yul.Parser as Parser

yulBlock :: QuasiQuoter
yulBlock = simpleQuoterUsing Parser.yulBlock

yulStmt :: QuasiQuoter
yulStmt = simpleQuoterUsing Parser.yulStmt

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
  let ast = runMyParser file p s
  dataToExpQ (const Nothing) ast

getPosition = fmap transPos location where
  transPos loc = (loc_filename loc,
                  fst (loc_start loc),
                  snd (loc_start loc))
