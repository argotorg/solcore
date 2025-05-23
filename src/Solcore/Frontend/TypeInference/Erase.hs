module Solcore.Frontend.TypeInference.Erase where

import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id

-- erasing Id's

class Erase a where
  type EraseRes a
  erase :: a -> EraseRes a

instance Erase a => Erase [a] where
  type EraseRes [a] = [EraseRes a]
  erase = map erase

instance Erase a => Erase (Maybe a) where
  type EraseRes (Maybe a) = Maybe (EraseRes a)
  erase  = fmap erase

instance (Erase a, Erase b) => Erase (a,b) where
  type EraseRes (a,b) = (EraseRes a, EraseRes b)

  erase (x, y) = (erase x, erase y)

instance Erase (Instance Id) where 
  type EraseRes (Instance Id) = Instance Name

  erase (Instance d vs ctx n ts t funs)
    = Instance d vs ctx n ts t (erase funs)

instance Erase (FunDef Id) where 
  type EraseRes (FunDef Id) = FunDef Name 

  erase (FunDef sig bd)
    = FunDef (erase sig) (erase bd)

instance Erase (Signature Id) where 
  type EraseRes (Signature Id) = Signature Name 

  erase (Signature n ps t args rt)
    = Signature n ps t (erase args) rt 

instance Erase (Stmt Id) where
  type EraseRes (Stmt Id) = Stmt Name

  erase (e1 := e2)
    = (erase e1) := (erase e2)
  erase (Let n mt me)
    = Let (idName n) mt (erase me)
  erase (StmtExp e)
    = StmtExp (erase e)
  erase (Return e)
    = Return (erase e)
  erase (Match es eqns)
    = Match (erase es) (erase eqns)
  erase (Asm blk)
    = Asm blk

instance Erase (Exp Id) where
  type EraseRes (Exp Id) = Exp Name

  erase (Var v)
    = Var (idName v)
  erase (Con n es)
    = Con (idName n) (map erase es)
  erase (FieldAccess me n)
    = FieldAccess (erase me) (idName n)
  erase (Call me n es)
    = Call (erase me) (idName n) (erase es)
  erase (Lam ps bd mt)
    = Lam (erase ps) (erase bd) mt
  erase (TyExp e t)
    = TyExp (erase e) t
  erase (Lit l) = Lit l

instance Erase (Param Id) where
  type EraseRes (Param Id) = Param Name

  erase (Typed n t)
    = Typed (idName n) t
  erase (Untyped n)
    = Untyped (idName n)

instance Erase (Pat Id) where
  type EraseRes (Pat Id) = Pat Name

  erase (PVar n)
    = PVar (idName n)
  erase (PCon n ps)
    = PCon (idName n) (erase ps)
  erase PWildcard
    = PWildcard
  erase (PLit l)
    = PLit l


