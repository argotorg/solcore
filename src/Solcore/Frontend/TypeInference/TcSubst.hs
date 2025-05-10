{-# LANGUAGE InstanceSigs #-}
module Solcore.Frontend.TypeInference.TcSubst where 

import Data.List

import Solcore.Frontend.Syntax

-- basic substitution infrastructure

newtype Subst 
  = Subst { unSubst :: [(MetaTv, Ty)] } deriving (Eq, Show)

restrict :: Subst -> [MetaTv] -> Subst
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

(+->) :: MetaTv -> Ty -> Subst
u +-> t = Subst [(u, t)]

class HasType a where 
  apply :: Subst -> a -> a 
  fv :: a -> [Tyvar]    -- free variables
  mv :: a -> [MetaTv]   -- meta variables
  bv :: a -> [Tyvar]    -- bound variables

instance (HasType a, HasType b, HasType c) => HasType (a,b,c) where 
  apply s (z,x,y) = (apply s z, apply s x, apply s y)
  fv (z,x,y) = fv z `union` fv x `union` fv y
  mv (z,x,y) = mv z `union` mv x `union` mv y
  bv (z,x,y) = bv z `union` bv x `union` bv y 

instance (HasType a, HasType b) => HasType (a,b) where 
  apply s (x,y) = (apply s x, apply s y)
  fv (x,y) = fv x `union` fv y
  mv (x,y) = mv x `union` mv y 
  bv (x,y) = bv x `union` bv y

instance HasType a => HasType [a] where 
  apply s = map (apply s)
  fv = foldr (union . fv) []
  mv = foldr (union . mv) []
  bv = foldr (union . bv) []


instance HasType a => HasType (Maybe a) where
  apply :: HasType a => Subst -> Maybe a -> Maybe a
  apply s = fmap (apply s)
  fv = maybe [] fv
  mv = maybe [] mv
  bv = maybe [] bv

instance HasType Name where 
  apply _ n = n 
  fv _ = []
  mv _ = []
  bv _ = []

instance HasType Ty where
  apply (Subst s) t@(Meta v)
    = maybe t id (lookup v s)
  apply s (TyCon n ts) 
    = TyCon n (apply s ts)
  apply _ t = t 

  fv (TyVar v@(Skolem _)) = [v]
  fv (TyCon _ ts) = fv ts
  fv _ = []

  mv (Meta v) = [v]
  mv (TyCon _ ts) = mv ts 
  mv _ = []

  bv (TyVar v@(TVar _)) = [v]
  bv (TyCon _ ts) = bv ts 
  bv _ = []

instance HasType Constr where 
  apply s (Constr dn ts) 
    = Constr dn (apply s ts)
  fv (Constr _ ts) = fv ts
  mv (Constr _ ts) = mv ts 
  bv (Constr _ ts) = bv ts 

instance HasType Pred where 
  apply s (InCls n t ts) = InCls n (apply s t) (apply s ts) 
  apply s (t1 :~: t2) = (apply s t1) :~: (apply s t2)

  fv (InCls _ t ts) = fv (t : ts)
  fv (t1 :~: t2) = fv [t1,t2]

  mv (InCls _ t ts) = mv (t : ts)
  mv (t1 :~: t2) = mv [t1, t2]

  bv (InCls _ t ts) = bv (t : ts)
  bv (t1 :~: t2) = bv [t1,t2]

instance HasType a => HasType (Qual a) where 
  apply s (ps :=> t) = (apply s ps) :=> (apply s t)
  fv (ps :=> t) = fv ps `union` fv t 
  mv (ps :=> t) = mv ps `union` mv t 
  bv (ps :=> t) = bv ps `union` bv t

instance HasType Scheme where 
  apply s (Forall vs t) 
    = Forall vs (apply s t)   
  fv (Forall vs t)
    = fv t \\ vs

  mv (Forall _ t) = mv t 
  bv (Forall vs qt) = vs `union` bv qt

instance HasType a => HasType (Signature a) where
  apply s (Signature vs ctx n p r) 
    = let 
        ctx' = apply s ctx
        p' = apply s p 
        r' = apply s r
        vs' = bv ctx' `union` bv p' `union` bv r'
      in Signature vs' ctx' n p' r'
  fv (Signature vs c _ p r) = fv (c,p,r) \\ vs 
  mv (Signature _ c _ p r) = mv (c,p,r)
  bv (Signature vs c _ p r) = vs `union` bv (c,p,r) 

instance HasType a => HasType (Param a) where
  apply s (Typed i t) = Typed (apply s i) (apply s t)
  apply s (Untyped i) = Untyped (apply s i)
  fv (Typed i t) = fv (i,t)
  fv (Untyped i) = fv i
  mv (Typed i t) = mv (i,t)
  mv (Untyped i) = mv i 
  bv (Typed i t) = bv (i,t)
  bv (Untyped i) = bv i

instance HasType a => HasType (FunDef a) where 
  apply s (FunDef sig bd)
    = FunDef (apply s sig) (apply s bd)
  fv (FunDef sig bd)
    = fv sig `union` fv bd
  mv (FunDef sig bd)
    = mv sig `union` mv bd  
  bv (FunDef sig bd)
    = bv sig `union` bv bd

instance HasType a => HasType (Instance a) where 
  apply s (Instance d ctx n ts t funs)
    = Instance d 
               (apply s ctx)
               n 
               (apply s ts)
               (apply s t)
               (apply s funs)
  fv (Instance _ ctx n ts t funs)
    = fv ctx `union` fv (t : ts)
  mv (Instance _ ctx n ts t funs)
    = mv ctx `union` mv (t : ts)
  bv (Instance _ ctx n ts t funs)
    = bv ctx `union` bv (t : ts)

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
  apply s (TyExp e ty) 
    = TyExp (apply s e) (apply s ty)
  apply s (Lit l) = Lit l

  fv (Var v) = fv v
  fv (Con n es)
    = fv n `union` fv es
  fv (FieldAccess e v)
    = fv e `union` fv v
  fv (Call m v es)
    = maybe [] fv m `union` fv v `union` fv es
  fv (Lam ps bd mt)
    = fv ps `union` fv bd `union` maybe [] fv mt
  fv (TyExp e ty) 
    = fv e `union` fv ty 

  mv (Var v) = mv v
  mv (Con n es)
    = mv n `union` mv es
  mv (FieldAccess e v)
    = mv e `union` mv v
  mv (Call m v es)
    = maybe [] mv m `union` mv v `union` mv es
  mv (Lam ps bd mt)
    = mv ps `union` mv bd `union` maybe [] mv mt
  mv (TyExp e ty) 
    = mv e `union` mv ty 

  bv (Var v) = bv v
  bv (Con n es)
    = bv n `union` bv es
  bv (FieldAccess e v)
    = bv e `union` bv v
  bv (Call m v es)
    = maybe [] bv m `union` bv v `union` bv es
  bv (Lam ps bd mt)
    = bv ps `union` bv bd `union` maybe [] bv mt
  bv (TyExp e ty) 
    = bv e `union` bv ty 



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

  mv (e1 := e2)
    = mv e1 `union` mv e2
  mv (Let v mt me)
    = mv v `union` (maybe [] mv mt)
           `union` (maybe [] mv me)
  mv (StmtExp e) = mv e
  mv (Return e) = mv e
  mv (Match es eqns)
    = mv es `union` mv eqns
  mv (Asm blk) = []

  bv (e1 := e2)
    = bv e1 `union` bv e2
  bv (Let v mt me)
    = bv v `union` (maybe [] bv mt)
           `union` (maybe [] bv me)
  bv (StmtExp e) = bv e
  bv (Return e) = bv e
  bv (Match es eqns)
    = bv es `union` bv eqns
  bv (Asm blk) = []


instance HasType a => HasType (Pat a) where
  apply s (PVar v) = PVar (apply s v)
  apply s (PCon v ps)
    = PCon (apply s v) (apply s ps)
  apply _ p = p

  fv (PVar v) = fv v
  fv (PCon v ps) = fv v `union` fv ps

  mv (PVar v) = mv v
  mv (PCon v ps) = mv v `union` mv ps

  bv (PVar v) = bv v
  bv (PCon v ps) = bv v `union` bv ps
