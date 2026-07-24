{-# LANGUAGE InstanceSigs #-}
-- for generic Pretty a => Show a
{-# LANGUAGE UndecidableInstances #-}

module Language.Yul where

import Common.Pretty
import Data.ByteString qualified as BS
import Data.Generics (Data, Typeable)
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text
import Solcore.Frontend.Syntax.Name

data YulObject = YulObject String YulCode [YulInner]

data YulInner = InnerObject YulObject | InnerData YulData

data YulData = YulData String HexOrString

data HexOrString = DHex String | DString String

-- we need these two decls because they are printed differently
newtype Yul = Yul {yulStmts :: [YulStmt]}

newtype YulCode = YulCode {ycStmts :: [YulStmt]}

instance Show YulObject where show = render . ppr

instance Show YulInner where show = render . ppr

instance Show YulData where show = render . ppr

instance Show HexOrString where show = render . ppr

instance Show Yul where show = render . ppr

instance Show YulCode where show = render . ppr

instance Show YulStmt where show = render . ppr

instance Show YulExp where show = render . ppr

instance Show YLiteral where show = render . ppr

instance Semigroup Yul where
  Yul a <> Yul b = Yul (a <> b)

instance Monoid Yul where
  mempty = Yul mempty

instance Semigroup YulCode where
  YulCode a <> YulCode b = YulCode (a <> b)

instance Monoid YulCode where
  mempty = YulCode mempty

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
  | YMeta String
  deriving (Eq, Ord, Data, Typeable)

-- | Check the lexical placement rules for Yul control-transfer statements.
-- A Yul function starts a fresh loop context: @break@/@continue@ cannot target
-- a loop outside that function, and @leave@ is only meaningful in its body.
validateYulControlFlow :: YulBlock -> Either String ()
validateYulControlFlow = validateBlock 0 False
  where
    validateBlock :: Int -> Bool -> YulBlock -> Either String ()
    validateBlock loopDepth inFunction = mapM_ (validateStmt loopDepth inFunction)

    validateStmt :: Int -> Bool -> YulStmt -> Either String ()
    validateStmt loopDepth inFunction stmt =
      case stmt of
        YBlock body ->
          validateBlock loopDepth inFunction body
        YFun _ _ _ body ->
          validateBlock 0 True body
        YIf _ body ->
          validateBlock loopDepth inFunction body
        YSwitch _ cases defaultBody -> do
          mapM_ (validateBlock loopDepth inFunction . snd) cases
          mapM_ (validateBlock loopDepth inFunction) defaultBody
        YFor pre _ post body -> do
          validateBlock loopDepth inFunction pre
          validateBlock loopDepth inFunction post
          validateBlock (loopDepth + 1) inFunction body
        YBreak
          | loopDepth == 0 ->
              Left "Yul break is only valid inside a for-loop body"
        YContinue
          | loopDepth == 0 ->
              Left "Yul continue is only valid inside a for-loop body"
        YLeave
          | not inFunction ->
              Left "Yul leave is only valid inside a Yul function"
        _ ->
          Right ()

-- | Validate the target-level representation of Yul literals. Ordinary string
-- literals are word values and therefore contain at most 32 UTF-8 bytes.
-- Object-name operands are handled specially by Yul and are not word-sized.
validateYulLiterals :: YulBlock -> Either String ()
validateYulLiterals = validateBlock
  where
    validateBlock :: YulBlock -> Either String ()
    validateBlock = mapM_ validateStmt

    validateStmt :: YulStmt -> Either String ()
    validateStmt stmt =
      case stmt of
        YBlock body ->
          validateBlock body
        YFun _ _ _ body ->
          validateBlock body
        YLet _ initializer ->
          mapM_ validateExp initializer
        YAssign _ value ->
          validateExp value
        YIf condition body -> do
          validateExp condition
          validateBlock body
        YSwitch scrutinee cases defaultBody -> do
          validateExp scrutinee
          mapM_ validateCase cases
          mapM_ validateBlock defaultBody
        YFor pre condition post body -> do
          validateBlock pre
          validateExp condition
          validateBlock post
          validateBlock body
        YExp expression ->
          validateExp expression
        YBreak ->
          pure ()
        YContinue ->
          pure ()
        YLeave ->
          pure ()
        YComment _ ->
          pure ()

    validateCase :: YulCase -> Either String ()
    validateCase (literal, body) = do
      validateLiteral False literal
      validateBlock body

    validateExp :: YulExp -> Either String ()
    validateExp expression =
      case expression of
        YCall function arguments ->
          mapM_
            (uncurry (validateArgument function))
            (zip [0 ..] arguments)
        YLit literal ->
          validateLiteral False literal
        YIdent _ ->
          pure ()
        YMeta _ ->
          pure ()

    validateArgument :: Name -> Int -> YulExp -> Either String ()
    validateArgument function index expression =
      case expression of
        YLit literal@(YulString _)
          | isYulObjectNameArgument function index ->
              validateLiteral True literal
        _ ->
          validateExp expression

    validateLiteral :: Bool -> YLiteral -> Either String ()
    validateLiteral _ (YulNumber number)
      | number < 0 || number >= 2 ^ (256 :: Int) =
          Left
            ( "Yul numeric literal is outside the 256-bit word range: "
                ++ show number
            )
    validateLiteral allowLongString (YulString value)
      | not allowLongString && byteLength > 32 =
          Left
            ( "Yul string literal exceeds 32 UTF-8 bytes (got "
                ++ show byteLength
                ++ ")"
            )
      where
        byteLength = BS.length (Text.encodeUtf8 (Text.pack value))
    validateLiteral _ _ =
      pure ()

-- | Whether an argument is interpreted by Yul as an object or immutable name
-- rather than as an ordinary word value.
isYulObjectNameArgument :: Name -> Int -> Bool
isYulObjectNameArgument function index =
  (function `elem` ["datasize", "dataoffset", "loadimmutable", "linkersymbol"] && index == 0)
    || (function == "setimmutable" && index == 1)

data YLiteral
  = YulNumber Integer
  | YulString String
  | YulTrue
  | YulFalse
  deriving (Eq, Ord, Data, Typeable)

yulIntegral :: (Integral i) => i -> YulExp
yulIntegral = YLit . YulNumber . fromIntegral

yulInt :: Integer -> YulExp
yulInt = YLit . YulNumber

yulBool :: Bool -> YulExp
yulBool True = YLit YulTrue
yulBool False = YLit YulFalse

yulString :: String -> YulExp
yulString = YLit . YulString

-- auxilliary functions

hlist, vlist, nvlist, pprBlock :: (Pretty a) => [a] -> Doc
hlist = hsep . map ppr
vlist = vcat . map ppr
nvlist = nest 2 . vlist
pprBlock stmts = braces (nvlist stmts)

instance Pretty YulObject where
  ppr (YulObject name code inners) =
    vcat
      [ text "object" <+> doubleQuotes (text name) <+> lbrace,
        nest 2 $ ppr code,
        nvlist inners,
        rbrace
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
  ppr (YFun name args rets stmts) =
    sep
      [ hsep [text "function", ppr name, pprArgs, pprRets rets, lbrace],
        nest 2 (vlist stmts),
        rbrace
      ]
    where
      pprArgs = parens (commaSepList args)
      pprRets Nothing = empty
      pprRets (Just rs) = text "->" <+> commaSepList rs
  ppr (YLet vars expr) =
    text "let"
      <+> commaSepList vars
      <+> maybe empty (\e -> text ":=" <+> ppr e) expr
  ppr (YAssign vars expr) = commaSepList vars <+> text ":=" <+> ppr expr
  ppr (YIf cond stmts) = text "if" <+> (ppr cond) <+> pprBlock stmts
  ppr (YSwitch expr cases def) =
    text "switch"
      <+> ppr expr
      $$ nest 2 (vcat (map pprCase cases))
      $$ maybe empty (\stmts -> text "default" <+> pprBlock stmts) def
    where
      pprCase (lit, stmts) = text "case" <+> ppr lit <+> pprBlock stmts
  ppr (YFor pre cond post stmts) =
    text "for"
      <+> braces (hlist pre)
      <+> ppr cond
      <+> braces (hlist post)
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
  ppr (YMeta s)
    | '`' `elem` s = text "${" <> text s <> char '}'
    | otherwise = char '`' <> text s <> char '`'

instance Pretty YLiteral where
  ppr (YulNumber n) = integer n
  ppr (YulString s) = pprQuotedString s
  ppr YulTrue = text "true"
  ppr YulFalse = text "false"

instance Pretty YulData where
  ppr (YulData name val) = hsep [text "data", pprQuotedString name, ppr val]

instance Pretty HexOrString where
  ppr (DHex s) = text "hex" <> doubleQuotes (text s)
  ppr (DString s) = pprQuotedString s

pprQuotedString :: String -> Doc
pprQuotedString = text . show
