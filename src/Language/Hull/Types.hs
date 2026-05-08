module Language.Hull.Types where

import Common.Pretty

data Type
  = TWord
  | TBool
  | TPair Type Type -- binary product, e.g. (word * word)
  | TSum Type Type -- binary sum, e.g. (unit + word)
  | TSumN [Type] -- n-ary sum
  | TFun [Type] Type
  | TUnit
  | TNamed String Type -- named type, e.g. Option{unit + word}
  deriving (Show, Eq)

stripTypeName :: Type -> Type
stripTypeName (TNamed _ t) = stripTypeName t
stripTypeName t = t

isWordType :: Type -> Bool
isWordType TWord = True
isWordType (TNamed _ t) = isWordType t
isWordType (TSumN [t]) = isWordType t
isWordType _ = False

zeroSizedType :: Type -> Bool
zeroSizedType TUnit = True
zeroSizedType (TNamed _ t) = zeroSizedType t
zeroSizedType (TPair t1 t2) = zeroSizedType t1 && zeroSizedType t2
zeroSizedType _ = False

instance Pretty Type where
  ppr TWord = text "word"
  ppr TBool = text "bool"
  ppr TUnit = text "unit"
  ppr (TPair t1 t2) = parens (ppr t1 <+> text "*" <+> ppr t2)
  ppr (TSum t1 t2) = parens (ppr t1 <+> text "+" <+> ppr t2)
  ppr (TSumN ts) = text "sum" >< parens (commaSepList ts)
  ppr (TFun ts t) = parens (hsep (map ppr ts) <+> text "->" <+> ppr t)
  ppr (TNamed n t) = text n >< braces (ppr t)
