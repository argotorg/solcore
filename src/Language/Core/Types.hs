module Language.Core.Types where

data Type
    = TWord
    | TBool
    | TPair Type Type
    | TSum Type Type
    | TSumN [Type]
    | TFun [Type] Type
    | TUnit
    | TNamed String Type
    deriving (Show)