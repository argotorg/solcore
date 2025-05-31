{-# LANGUAGE InstanceSigs #-}
module Language.Yul where
import Data.Generics (Data, Typeable)

import Common.Pretty
import Solcore.Frontend.Syntax.Name

import Solcore.Frontend.Pretty.Name


newtype Yul = Yul { yulStmts :: [YulStmt] }
instance Show Yul where show = render . ppr
instance Show YulStmt where show = render . ppr
instance Show YulExp where show = render . ppr
instance Show YLiteral where show = render . ppr

instance Semigroup Yul where
  Yul a <> Yul b = Yul (a <> b)

instance Monoid Yul where
  mempty = Yul []

type YArg = Name
type YReturns = Maybe [Name]
pattern YNoReturn :: Maybe a
pattern YNoReturn = Nothing
pattern YReturns :: a -> Maybe a
pattern YReturns a = Just a
pattern YulAlloc :: Name -> YulStmt
pattern YulAlloc name = YLet [name] Nothing
pattern YAssign1 :: Name -> YulExp -> YulStmt
pattern YAssign1 name expr = YAssign [name] expr

type YulCases = [YulCase]
type YulCase = (YLiteral, YulBlock)
type YulDefault = Maybe YulBlock
type YulBlock = [YulStmt]


data YulStmt
  = YBlock YulBlock
  | YFun Name [YArg] YReturns [YulStmt]
  | YLet [Name] (Maybe YulExp)
  | YAssign [Name] YulExp
  | YIf YulExp YulBlock
  | YSwitch YulExp YulCases YulDefault
  | YFor YulBlock YulExp YulBlock YulBlock
  | YBreak
  | YContinue
  | YLeave
  | YComment String
  | YExp YulExp
  deriving (Eq, Ord, Data, Typeable)

data YulExp
  = YCall Name [YulExp]
  | YIdent Name
  | YLit YLiteral
   deriving (Eq, Ord, Data, Typeable)

data YLiteral
  = YulNumber Integer
  | YulString String
  | YulTrue
  | YulFalse
  deriving (Eq, Ord, Data, Typeable)

yulInt :: Integral i => i -> YulExp
yulInt = YLit . YulNumber . fromIntegral

yulBool :: Bool -> YulExp
yulBool True = YLit YulTrue
yulBool False = YLit YulFalse

instance Pretty Yul where
  ppr (Yul stmts) = vcat (map ppr stmts)

instance Pretty YulStmt where
  ppr (YBlock stmts) =
    lbrace
      $$ nest 4 (vcat (map ppr stmts))
      $$ rbrace
  ppr (YFun name args rets stmts) =
    text "function"
      <+> ppr name
      <+> prettyargs
      <+> prettyrets rets
      <+> lbrace
      $$ nest 4 (vcat (map ppr stmts))
      $$ rbrace
    where
        prettyargs = parens (commaSepList args)
        prettyrets Nothing = empty
        prettyrets (Just rs) = text "->" <+> commaSepList rs
  ppr (YLet vars expr) =
    text "let" <+> commaSepList vars
               <+> maybe empty (\e -> text ":=" <+> ppr e) expr
  ppr (YAssign vars expr) = commaSepList vars <+> text ":=" <+> ppr expr
  ppr (YIf cond stmts) = text "if" <+> parens (ppr cond) <+> ppr (YBlock stmts)
  ppr (YSwitch expr cases def) =
    text "switch"
      <+> ppr expr
      $$ nest 4 (vcat (map (\(lit, stmts) -> text "case" <+> ppr lit <+> ppr (YBlock stmts)) cases))
      $$ maybe empty (\stmts -> text "default" <+> ppr (YBlock stmts)) def
  ppr (YFor pre cond post stmts) =
    text "for" <+> braces (hsep  (map ppr pre))
               <+> ppr cond
               <+> hsep (map ppr post) <+> ppr (YBlock stmts)
  ppr YBreak = text "break"
  ppr YContinue = text "continue"
  ppr YLeave = text "leave"
  ppr (YComment c) = text "/*" <+> text c <+> text "*/"
  ppr (YExp e) = ppr e

instance Pretty YulExp where
  ppr :: YulExp -> Doc
  ppr (YCall name args) = ppr name >< parens (hsep (punctuate comma (map ppr args)))
  ppr (YIdent name) = ppr name
  ppr (YLit lit) = ppr lit

instance Pretty YLiteral where
  ppr (YulNumber n) = integer n
  ppr (YulString s) = doubleQuotes (text s)
  ppr YulTrue = text "true"
  ppr YulFalse = text "false"

-- commaSepList :: Pretty a => [a] -> Doc
-- commaSepList = hsep . punctuate comma . map ppr

{- | wrap a Yul chunk in a Solidity function with the given name
   assumes result is in a variable named "_result"
-}
wrapInSolFunction :: Pretty a => Name -> a -> Doc
wrapInSolFunction name yul = text "function" <+> ppr name <+> prettyargs <+> text " public pure returns (uint256 _wrapresult)" <+> lbrace
  $$ nest 2 assembly
  $$ rbrace
  where
    assembly = text "assembly" <+> lbrace
      $$ nest 2 (ppr yul)
      $$ rbrace
    prettyargs = parens empty

wrapInContract :: Name -> Name -> Doc -> Doc
wrapInContract name entry body = empty
  $$ text "// SPDX-License-Identifier: UNLICENSED"
  $$ text "pragma solidity ^0.8.23;"
  $$ text "import {console,Script} from \"forge-std/Script.sol\";"
  $$ text "contract" <+> ppr name <+> text "is Script"<+> lbrace
  $$ nest 2 run
  $$ nest 2 body
  $$ rbrace

  where
    run = text "function run() public view" <+> lbrace
      $$ nest 2 (text "console.log(\"RESULT --> \","<+> ppr entry >< text ");")
      $$ rbrace $$ text ""
