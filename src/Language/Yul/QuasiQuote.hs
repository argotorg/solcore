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
simpleQuoterUsing p = simpleQuoter (quoteUsingLift liftYulExp p) (quoteUsingLift liftYulPat p)

simpleQuoter :: (String -> Q Exp) -> (String -> Q Pat) -> QuasiQuoter
simpleQuoter qExp qPat = QuasiQuoter
  { quoteExp = qExp
  , quotePat = qPat
  , quoteDec = error "Cannot generate Haskell declarations from Yul code"
  , quoteType = error "Cannot generate Haskell types from Yul code"
  }

quoteUsingLift :: (a -> Q b) -> Parser a -> String -> Q b
quoteUsingLift lift p s = do
  (file, _l, _c) <- getPosition
  let ast = runMyParser file (p <* eof) s
  lift ast

liftYulExp :: Data a => a -> Q Exp
liftYulExp = dataToExpQ (const Nothing `extQ` antiYulExp)

liftYulPat :: Data a => a -> Q Pat
liftYulPat = dataToPatQ (const Nothing `extQ` antiYulPat)

getPosition = fmap transPos location where
  transPos loc = (loc_filename loc,
                  fst (loc_start loc),
                  snd (loc_start loc))

antiYulExp :: YulExp -> Maybe (Q Exp)
antiYulExp (YMeta v) = Just $ varE (mkName v)
antiYulExp _ = Nothing

antiYulPat :: YulExp -> Maybe (Q Pat)
antiYulPat (YMeta v) = Just $ varP (mkName v)
antiYulPat _ = Nothing
