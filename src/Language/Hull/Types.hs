module Language.Hull.Types where

data Type
  = TWord
  | TBool
  | TPair Type Type -- binary product, e.g. (word * word)
  | TSum Type Type -- binary sum, e.g. (unit + word)
  | TSumN [Type] -- n-ary sum
  | TFun [Type] Type
  | TUnit
  | TNamed String Type -- named type, e.g. Option{unit + word}
  deriving (Show)

stripTypeName :: Type -> Type
stripTypeName (TNamed _ t) = stripTypeName t
stripTypeName t = t

zeroSizedType :: Type -> Bool
zeroSizedType TUnit = True
zeroSizedType (TNamed _ t) = zeroSizedType t
zeroSizedType (TPair t1 t2) = zeroSizedType t1 && zeroSizedType t2
zeroSizedType _ = False
