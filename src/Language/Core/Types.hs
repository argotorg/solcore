module Language.Core.Types where

data Type
    = TWord
    | TBool
    | TPair Type Type     -- binary product, e.g. (word * word)
    | TSum Type Type      -- binary sum, e.g. (unit + word)
    | TSumN [Type]        -- n-ary sum
    | TFun [Type] Type
    | TUnit
    | TNamed String Type  -- named type, e.g. Option{unit + word}
    deriving (Show)

stripTypeName :: Type -> Type
stripTypeName (TNamed _ t) = stripTypeName t
stripTypeName t = t
