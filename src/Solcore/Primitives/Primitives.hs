module Solcore.Primitives.Primitives where

import Prelude hiding (words)

import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.TypeInference.Id

-- basic type classes

selfVar :: Tyvar
selfVar = TVar selfName

argsVar :: Tyvar
argsVar = TVar argsName

retVar :: Tyvar
retVar = TVar retName

invokeClass :: Class Name
invokeClass
  = Class [selfVar, argsVar, retVar]
          []
          invokableName
          [argsVar, retVar]
          selfVar
          [invokeSignature]

invokePred :: Pred
invokePred
  = InCls (Name "invokable")
          (TyVar selfVar)
          (TyVar <$> [argsVar, retVar])

argsName :: Name
argsName = Name "args"

retName :: Name
retName = Name "ret"

selfName :: Name
selfName = Name "self"

invokableName :: Name
invokableName = Name "invokable"

invokeName :: Name
invokeName = Name "invoke"

invokeSignature :: Signature Name
invokeSignature
  = Signature [selfVar, argsVar]
              []
              invokeName
              [ Typed selfName (TyVar selfVar)
              , Typed argsName (TyVar argsVar)
              ]
              (Just (TyVar retVar))
-- basic types

primAddWord :: (Name, Scheme)
primAddWord = ("primAddWord", monotype (word :-> word :-> word))

primEqWord :: (Name, Scheme)
primEqWord = ("primEqWord", monotype (word :-> word :-> word))

primInvoke :: (Name, Scheme)
primInvoke = ( QualName invokableName "invoke"
             , Forall [selfVar, argsVar, retVar]
                ([invokePred] :=> (TyVar selfVar :->
                                   TyVar argsVar :->
                                   TyVar retVar))
             )

-- pairs, strings, unit and word types.

word :: Ty
word = TyCon "word" []

primPair :: (Name, Scheme)
primPair = (Name "pair", Forall [aVar, bVar] ([] :=> (pairTy at bt)))

aVar = TVar (Name "a")
bVar = TVar (Name "b")
at = TyVar aVar
bt = TyVar bVar

primUnit :: (Name, Scheme)
primUnit = (Name "()", monotype unit)

pairTy :: Ty -> Ty -> Ty
pairTy t1 t2 = t1 :-> t2 :-> pair t1 t2

string :: Ty
string = TyCon "string" []

stack :: Ty -> Ty
stack t = TyCon "stack" [t]

unit :: Ty
unit = TyCon "()" []

pair :: Ty -> Ty -> Ty
pair t1 t2 = TyCon "pair" [t1, t2]

epair :: Exp Name -> Exp Name -> Exp Name
epair e1 e2 = Con (Name "pair") [e1, e2]

arr :: Name
arr = "->"

-- sum type

sumTy :: Ty -> Ty -> Ty
sumTy t1 t2 = TyCon "sum" [t1, t2]

inlTy :: Ty -> Ty -> Ty
inlTy t1 t2 = t1 :-> sumTy t1 t2

inrTy :: Ty -> Ty -> Ty
inrTy t1 t2 = t2 :-> sumTy t1 t2

inlName :: Name
inlName = Name "inl"

inrName :: Name
inrName = Name "inr"

primInl :: (Name, Scheme)
primInl = (inlName, Forall [aVar, bVar] ([]:=> inlTy at bt))

primInr :: (Name, Scheme)
primInr = (inrName, Forall [aVar, bVar] ([] :=> inrTy at bt))

-- boolean type constructor

boolName :: Name
boolName = Name "bool"

boolTy :: Ty
boolTy = TyCon boolName []

trueName :: Name
trueName = Name "true"

falseName :: Name
falseName = Name "false"

primTrue :: (Name, Scheme)
primTrue = (trueName, monotype boolTy)

primFalse :: (Name, Scheme)
primFalse = (falseName, monotype boolTy)

-- tuple utils

tupleExpFromList :: [Exp Name] -> Exp Name
tupleExpFromList [] = Con (Name "()") []
tupleExpFromList [e] = e
tupleExpFromList [e1,e2] = epair e1 e2
tupleExpFromList (e1 : es) = epair e1 (tupleExpFromList es)

tupleTyFromList :: [Ty] -> Ty
tupleTyFromList [] = unit
tupleTyFromList [t] = t
tupleTyFromList [t1,t2] = pair t1 t2
tupleTyFromList (t1 : ts) = pair t1 (tupleTyFromList ts)

-- definition of yul primops

yulPrimOps :: [(Name, Scheme)]
yulPrimOps = [ (Name "stop", monotype unit)
             , (Name "add", monotype (word :-> word :-> word))
             , (Name "sub", monotype (word :-> word :-> word))
             , (Name "mul", monotype (word :-> word :-> word))
             , (Name "div", monotype (word :-> word :-> word))
             , (Name "sdiv", monotype (word :-> word :-> word))
             , (Name "mod", monotype (word :-> word :-> word))
             , (Name "smod", monotype (word :-> word :-> word))
             , (Name "exp", monotype (word :-> word :-> word))
             , (Name "not", monotype (word :-> word))
             , (Name "lt", monotype (word :-> word :-> word))
             , (Name "gt", monotype (word :-> word :-> word))
             , (Name "slt", monotype (word :-> word :-> word))
             , (Name "sgt", monotype (word :-> word :-> word))
             , (Name "eq", monotype (word :-> word :-> word))
             , (Name "iszero", monotype (word :-> word))
             , (Name "and", monotype (word :-> word :-> word))
             , (Name "or", monotype (word :-> word :-> word))
             , (Name "xor", monotype (word :-> word :-> word))
             , (Name "byte", monotype (word :-> word :-> word))
             , (Name "shl", monotype (word :-> word :-> word))
             , (Name "shr", monotype (word :-> word :-> word))
             , (Name "sar", monotype (word :-> word :-> word))
             , (Name "addmod", monotype (word :-> word :-> word :-> word))
             , (Name "mulmod", monotype (word :-> word :-> word :-> word))
             , (Name "signextend", monotype (word :-> word :-> word))
             , (Name "keccak256", monotype (word :-> word :-> word))
             , (Name "pc", monotype word)
             , (Name "pop", monotype (word :-> unit))
             , (Name "mload", monotype (word :-> word))
             , (Name "mstore", monotype (word :-> word :-> unit))
             , (Name "mstore8", monotype (word :-> word :-> unit))
             , (Name "sload", monotype (word :-> word))
             , (Name "sstore", monotype (word :-> word :-> unit))
             , (Name "msize", monotype word)
             , (Name "gas", monotype word)
             , (Name "address", monotype word)
             , (Name "balance", monotype (word :-> word))
             , (Name "selfbalance", monotype word)
             , (Name "caller", monotype word)
             , (Name "callvalue", monotype word)
             , (Name "calldataload", monotype (word :-> word))
             , (Name "calldatasize", monotype word)
             , (Name "calldatacopy", monotype (word :-> word :-> word :-> unit))
             , (Name "codesize", monotype word)
             , (Name "codecopy", monotype (word :-> word :-> word :-> unit))
             , (Name "datasize", monotype (string :-> word))
             , (Name "dataoffset", monotype (string :-> word))
             , (Name "extcodesize", monotype (word :-> word))
             , (Name "extcodecopy", monotype (word :-> word :-> word :-> unit))
             , (Name "returndatasize", monotype word)
             , (Name "returndatacopy", monotype (word :-> word :-> word :-> unit))
             , (Name "mcopy", monotype (word :-> word :-> word :-> unit))
             , (Name "extcodehash", monotype (word :-> word))
             , (Name "create", monotype (word :-> word :-> word :-> unit))
             , (Name "create2", monotype (word :-> word :-> word :-> unit))
             , (Name "call", monotype (funtype (words 7) word))
             , (Name "callcode", monotype (funtype (words 7) word))
             , (Name "delegatecall", monotype (funtype (words 6) word))
             , (Name "staticcall", monotype (funtype (words 6) word))
             , (Name "return", Forall [aVar] ([] :=> (word :-> word :-> (TyVar aVar))))
             , (Name "revert", Forall [aVar] ([] :=> (word :-> word :-> (TyVar aVar))))
             , (Name "selfdestruct", monotype (word :-> unit))
             , (Name "invalid", monotype unit)
             , (Name "log0", monotype (word :-> word :-> unit))
             , (Name "log1", monotype (word :-> word :-> word :-> unit))
             , (Name "log2", monotype (funtype (words 4) unit))
             , (Name "log3", monotype (funtype (words 5) unit))
             , (Name "log4", monotype (funtype (words 6) unit))
             , (Name "chainid", monotype word)
             , (Name "basefee", monotype word)
             , (Name "origin", monotype word)
             , (Name "gasprice", monotype word)
             , (Name "blockhash", monotype (word :-> word))
             , (Name "coinbase", monotype word)
             , (Name "timestamp", monotype word)
             , (Name "number", monotype word)
             , (Name "difficulty", monotype word)
             , (Name "prevrandao", monotype word)
             , (Name "gaslimit", monotype word)
             , (Name "memoryguard", monotype (word :-> word))
             ]

words :: Int -> [Ty]
words n = replicate n word
