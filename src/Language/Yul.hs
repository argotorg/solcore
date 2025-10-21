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

yulIntegral :: Integral i => i -> YulExp
yulIntegral = YLit . YulNumber . fromIntegral

yulInt :: Integer -> YulExp
yulInt = YLit . YulNumber

yulBool :: Bool -> YulExp
yulBool True = YLit YulTrue
yulBool False = YLit YulFalse

yulString :: String -> YulExp
yulString = YLit . YulString

-- auxilliary functions

hlist, vlist, nvlist, pprBlock :: Pretty a => [a] -> Doc
hlist = hsep . map ppr
vlist = vcat . map ppr
nvlist = nest 2 . vlist
pprBlock stmts = braces(nvlist stmts)


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
  ppr (YIf cond stmts) = text "if" <+> (ppr cond) <+> pprBlock stmts
  ppr (YSwitch expr cases def) =
    text "switch"
      <+> ppr expr
      $$ nest 2 (vcat (map pprCase cases))
      $$ maybe empty (\stmts -> text "default" <+> pprBlock stmts) def
    where pprCase (lit, stmts) = text "case" <+> ppr lit <+> pprBlock stmts
  ppr (YFor pre cond post stmts) =
    text "for" <+> braces (hlist pre) <+> ppr cond <+> braces (hlist post)
               $$ pprBlock stmts
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
