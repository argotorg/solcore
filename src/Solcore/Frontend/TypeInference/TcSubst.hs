{-# LANGUAGE InstanceSigs #-}
module Solcore.Frontend.TypeInference.TcSubst where 

import Data.List

import Solcore.Frontend.Syntax

-- basic substitution infrastructure

newtype Subst 
  = Subst { unSubst :: [(Tyvar, Ty)] } deriving (Eq, Show)

restrict :: Subst -> [Tyvar] -> Subst
restrict (Subst s) vs 
  = Subst [(v,t) | (v,t) <- s, v `notElem` vs]

emptySubst :: Subst
emptySubst = Subst []

-- composition operators

instance Semigroup Subst where 
  s1 <> s2 = Subst (outer ++ inner) 
    where 
      outer = [(u, apply s1 t) | (u, t) <- unSubst s2]
      inner = [(v,t) | (v,t) <- unSubst s1, v `notElem` dom2]
      dom2 = map fst (unSubst s2)

instance Monoid Subst where 
  mempty = emptySubst 

(+->) :: Tyvar -> Ty -> Subst
u +-> t = Subst [(u, t)]

class HasType a where 
  apply :: Subst -> a -> a 
  fv :: a -> [Tyvar]

instance (HasType a, HasType b, HasType c) => HasType (a,b,c) where 
  apply s (z,x,y) = (apply s z, apply s x, apply s y)
  fv (z,x,y) = fv z `union` fv x `union` fv y

instance (HasType a, HasType b) => HasType (a,b) where 
  apply s (x,y) = (apply s x, apply s y)
  fv (x,y) = fv x `union` fv y

instance HasType a => HasType [a] where 
  apply s = map (apply s)
  fv = foldr (union . fv) []


instance HasType a => HasType (Maybe a) where
  apply :: HasType a => Subst -> Maybe a -> Maybe a
  apply s = fmap (apply s)
  fv = maybe [] fv

instance HasType Name where 
  apply _ n = n 
  fv _ = []

instance HasType Ty where
  apply (Subst s) t@(TyVar v)
    = maybe t id (lookup v s)
  apply s (TyCon n ts) 
    = TyCon n (map (apply s) ts)

  fv (TyVar v) = [v]
  fv (TyCon _ ts) = fv ts

instance HasType Constr where 
  apply s (Constr dn ts) 
    = Constr dn (apply s ts)
  fv (Constr _ ts) = fv ts

instance HasType Pred where 
  apply s (InCls n t ts) = InCls n (apply s t) (apply s ts) 
  apply s (t1 :~: t2) = (apply s t1) :~: (apply s t2)

  fv (InCls _ t ts) = fv (t : ts)
  fv (t1 :~: t2) = fv [t1,t2]

instance HasType a => HasType (Qual a) where 
  apply s (ps :=> t) = (apply s ps) :=> (apply s t)
  fv (ps :=> t) = fv ps `union` fv t 

instance HasType Scheme where 
  apply s (Forall vs t) 
    = Forall vs (apply s' t)   
      where 
        s' = restrict s vs
  fv (Forall vs t)
    = fv t \\ vs

instance HasType a => HasType (Signature a) where
  apply s (Signature vs ctx n p r) 
    = Signature vs (apply s ctx) n (apply s p) (apply s r)
  fv (Signature vs c _ p r) = fv (c,p,r) \\ vs 

instance HasType a => HasType (Param a) where
  apply s (Typed i t) = Typed (apply s i) (apply s t)
  apply s (Untyped i) = Untyped (apply s i)
  fv (Typed i t) = fv (i,t)
  fv (Untyped i) = fv i

instance HasType a => HasType (FunDef a) where 
  apply s (FunDef sig bd)
    = FunDef (apply s sig) (apply s bd)
  fv (FunDef sig bd)
    = fv sig `union` fv bd

instance HasType a => HasType (Instance a) where 
  apply s (Instance ctx n ts t funs)
    = Instance (apply s ctx)
               n 
               (apply s ts)
               (apply s t)
               (apply s funs)
  fv (Instance ctx n ts t funs)
    = fv ctx `union` fv (t : ts)

instance HasType a => HasType (Exp a) where
  apply s (Var v) = Var (apply s v)
  apply s (Con n es)
    = Con (apply s n) (apply s es)
  apply s (FieldAccess e v)
    = FieldAccess (apply s e) (apply s v)
  apply s (Call m v es)
    = Call (apply s <$> m) (apply s v) (apply s es)
  apply s (Lam ps bd mt)
    = Lam (apply s ps) (apply s bd) (apply s <$> mt)
  apply _ e = e

  fv (Var v) = fv v
  fv (Con n es)
    = fv n `union` fv es
  fv (FieldAccess e v)
    = fv e `union` fv v
  fv (Call m v es)
    = maybe [] fv m `union` fv v `union` fv es
  fv (Lam ps bd mt)
    = fv ps `union` fv bd `union` maybe [] fv mt

instance HasType a => HasType (Stmt a) where
  apply s (e1 := e2)
    = (apply s e1) := (apply s e2)
  apply s (Let v mt me)
    = Let (apply s v)
          (apply s <$> mt)
          (apply s <$> me)
  apply s (StmtExp e)
    = StmtExp (apply s e)
  apply s (Return e)
    = Return (apply s e)
  apply s (Match es eqns)
    = Match (apply s es) (apply s eqns)
  apply _ s
    = s

  fv (e1 := e2)
    = fv e1 `union` fv e2
  fv (Let v mt me)
    = fv v `union` (maybe [] fv mt)
           `union` (maybe [] fv me)
  fv (StmtExp e) = fv e
  fv (Return e) = fv e
  fv (Match es eqns)
    = fv es `union` fv eqns
  fv (Asm blk) = []

instance HasType a => HasType (Pat a) where
  apply s (PVar v) = PVar (apply s v)
  apply s (PCon v ps)
    = PCon (apply s v) (apply s ps)
  apply _ p = p

  fv (PVar v) = fv v
  fv (PCon v ps) = fv v `union` fv ps


