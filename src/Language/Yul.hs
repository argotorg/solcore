{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE UndecidableInstances #-} -- for generic Pretty a => Show a


module Language.Yul where
import Data.Generics (Data, Typeable)

import Common.Pretty
import Solcore.Frontend.Syntax.Name

import Solcore.Frontend.Pretty.Name
data YulObject = YulObject String YulCode [YulInner]
data YulInner = InnerObject YulObject | InnerData YulData
data YulData =  YulData String HexOrString
data HexOrString = DHex String | DString String

newtype Yul = Yul { yulStmts :: [YulStmt] }
newtype YulCode = YulCode YulBlock

instance {-# OVERLAPPABLE #-} Pretty a => Show a where show = render . ppr
{-
instance Show Yul where show = render . ppr
instance Show YulStmt where show = render . ppr
instance Show YulExp where show = render . ppr
instance Show YLiteral where show = render . ppr
instance Show YulData where show = render . ppr
-}

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


-- auxilliary functions

hlist, vlist, nvlist :: Pretty a => [a] -> Doc
hlist = hsep . map ppr
vlist = vcat . map ppr
nvlist = nest 2 . vlist
pprBlock stmts = lbrace $$ nvlist stmts $$ rbrace


instance Pretty YulObject where
  ppr (YulObject name code inners) = vcat
    [ text "object" <+> doubleQuotes(text name) <+> lbrace
    , nest 2 $ ppr code
    , nvlist inners
    , rbrace
    ]

instance Pretty YulInner where
  ppr (InnerObject obj) = ppr obj
  ppr (InnerData dat) = ppr dat

instance Pretty Yul where
  ppr (Yul stmts) = vcat (map ppr stmts)

instance Pretty YulCode where
  ppr (YulCode block) = (text "code" <+> lbrace) $$ nvlist block $$ rbrace

instance Pretty YulStmt where
  ppr (YBlock stmts) = pprBlock stmts
  ppr (YFun name args rets stmts) = sep
    [ hsep [text "function", ppr name, pprArgs, pprRets rets, lbrace]
    , nest 2 (vlist stmts)
    , rbrace
    ]
    where
        pprArgs = parens (commaSepList args)
        pprRets Nothing = empty
        pprRets (Just rs) = text "->" <+> commaSepList rs
  ppr (YLet vars expr) =
    text "let" <+> commaSepList vars
               <+> maybe empty (\e -> text ":=" <+> ppr e) expr
  ppr (YAssign vars expr) = commaSepList vars <+> text ":=" <+> ppr expr
  ppr (YIf cond stmts) = text "if" <+> parens (ppr cond) <+> pprBlock stmts
  ppr (YSwitch expr cases def) =
    text "switch"
      <+> ppr expr
      $$ nest 2 (vcat (map pprCase cases))
      $$ maybe empty (\stmts -> text "default" <+> pprBlock stmts) def
    where pprCase (lit, stmts) = text "case" <+> ppr lit <+> pprBlock stmts
  ppr (YFor pre cond post stmts) =
    text "for" <+> braces (hlist pre)
               <+> ppr cond
               <+> hlist post <+> pprBlock stmts
  ppr YBreak = text "break"
  ppr YContinue = text "continue"
  ppr YLeave = text "leave"
  ppr (YComment c) = text "/*" <+> text c <+> text "*/"
  ppr (YExp e) = ppr e

instance Pretty YulExp where
  ppr :: YulExp -> Doc
  ppr (YCall name args) = ppr name >< parens (commaSepList args)
  ppr (YIdent name) = ppr name
  ppr (YLit lit) = ppr lit

instance Pretty YLiteral where
  ppr (YulNumber n) = integer n
  ppr (YulString s) = doubleQuotes (text s)
  ppr YulTrue = text "true"
  ppr YulFalse = text "false"

instance Pretty YulData where
  ppr (YulData name val) = hsep [text "data", doubleQuotes $ text name , ppr val]

instance Pretty HexOrString where
  ppr (DHex s) = text "hex" <> doubleQuotes (text s)
  ppr (DString s) = doubleQuotes (text s)

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
  $$ text "import {console,Script} from \"lib/stdlib.sol\";"
  $$ text "contract" <+> ppr name <+> text "is Script"<+> lbrace
  $$ nest 2 run
  $$ nest 2 body
  $$ rbrace

  where
    run = text "function run() public view" <+> lbrace
      $$ nest 2 (text "console.log(\"RESULT --> \","<+> ppr entry >< text ");")
      $$ rbrace $$ text ""

-- sample code
fnUsrAdd :: YulStmt
fnUsrAdd = YFun "usr$add" ["x", "y"] (YReturns ["result"])
        [ YAssign ["result"] (YCall "add" [YIdent "x", YIdent "y"])]

fnUsrMain :: YulStmt
fnUsrMain = YFun "usr$main" [] (YReturns ["result"])
        [ YAssign1 "result" (YCall "usr$add" [yulInt 40, yulInt 2])]

sampleCode :: YulCode
sampleCode = YulCode
            [ fnUsrAdd
            , fnUsrMain
            , YulAlloc "z"
            , YAssign1 "z" (YCall "usr$main" [])
            , YExp $ YCall "mstore" [yulInt 0, YIdent "z"]
            , YExp $ YCall "return" [yulInt 0, yulInt 32]
            ]

sampleObject :: YulObject
sampleObject = YulObject "Add" sampleCode []


sampleNestedObject :: YulObject
sampleNestedObject = YulObject "Nested" sampleCode
                        [  InnerObject sampleObject
                        , InnerData $ YulData "Table1" (DHex "4123")]
