{-# LANGUAGE PatternSynonyms #-}

module Solcore.Frontend.Syntax.Ty where

import Data.Generics (Data, Typeable)
import Data.List
import Solcore.Diagnostics (SourceSpan)
import Solcore.Frontend.Syntax.Location
import Solcore.Frontend.Syntax.Name

-- basic typing infrastructure

data Tyvar
  = TVar {var :: Name} -- bound variable
  | Skolem Name -- skolem constant
  deriving (Eq, Ord, Show, Data, Typeable)

tyvarName :: Tyvar -> Name
tyvarName (TVar n) = n
tyvarName (Skolem n) = n

isBound :: Tyvar -> Bool
isBound (TVar _) = True
isBound _ = False

data Ty
  = TyVar Tyvar -- type variable
  | TyConWithLocation NodeLocation Name [Ty] -- type constructor
  | Meta MetaTv -- meta type variable
  deriving (Eq, Ord, Show, Data, Typeable)

pattern TyCon :: Name -> [Ty] -> Ty
pattern TyCon n ts <- TyConWithLocation _ n ts
  where
    TyCon n ts = TyConWithLocation unlocatedNode n ts

{-# COMPLETE TyVar, TyCon, Meta #-}

newtype MetaTv
  = MetaTv {metaName :: Name}
  deriving (Eq, Ord, Show, Data, Typeable)

locatedTy :: SourceSpan -> Ty -> Ty
locatedTy sourceSpan (TyCon n ts) = TyConWithLocation (locatedNode sourceSpan) n ts
locatedTy _ ty = ty

instance HasSourceSpan Tyvar where
  sourceSpanOf = sourceSpanOf . tyvarName

instance HasSourceSpan MetaTv where
  sourceSpanOf = sourceSpanOf . metaName

instance HasSourceSpan Ty where
  sourceSpanOf (TyVar tyvar) = sourceSpanOf tyvar
  sourceSpanOf (TyConWithLocation location n ts) =
    firstSourceSpan [sourceSpanOf location, sourceSpanOf n, sourceSpanOf ts]
  sourceSpanOf (Meta metaTv) = sourceSpanOf metaTv

tyconNames :: Ty -> [Name]
tyconNames (TyCon n ts) =
  nub (n : concatMap tyconNames ts)
tyconNames _ = []

gvar :: MetaTv -> Tyvar
gvar = TVar . metaName

elabTyvar :: Tyvar -> Tyvar
elabTyvar (Skolem v) = TVar v
elabTyvar v = v

gen :: Ty -> Ty
gen (Meta v) = TyVar (gvar v)
gen (TyCon n ts) = TyCon n (map gen ts)
gen t = t

infixr 4 :->

pattern (:->) :: Ty -> Ty -> Ty
pattern (:->) a b =
  TyCon (Name "->") [a, b]

argTy :: Ty -> [Ty]
argTy (t1 :-> t2) = t1 : argTy t2
argTy _ = []

retTy :: Ty -> Maybe Ty
retTy (TyVar _) = Nothing
retTy (Meta _) = Nothing
retTy (_ :-> t2) = ret t2
  where
    ret (_ :-> tb) = ret tb
    ret t = Just t
retTy t@(TyCon _ _) = Just t

splitTy :: Ty -> ([Ty], Ty)
splitTy (a :-> b) =
  let (as, r) = splitTy b
   in (a : as, r)
splitTy t = ([], t)

funtype :: [Ty] -> Ty -> Ty
funtype ts t = foldr (:->) t ts

class AlphaEq a where
  alphaEq :: a -> a -> Bool

instance (AlphaEq a) => AlphaEq [a] where
  alphaEq ts ts' = and $ zipWith alphaEq ts ts'

instance AlphaEq Ty where
  alphaEq (TyVar n) (TyVar n') =
    n == n'
  -- meta variables can unify with anything.
  alphaEq (Meta _) _ =
    True
  alphaEq _ (Meta _) = True
  alphaEq (TyCon n ts) (TyCon n' ts') =
    n == n' && (and (zipWith alphaEq ts ts'))
  alphaEq _ _ =
    False

instance AlphaEq Pred where
  alphaEq (InCls n t ts) (InCls n' t' ts') =
    n == n' && alphaEq t t' && alphaEq ts ts'
  alphaEq (t1 :~: t2) (t1' :~: t2') =
    alphaEq t1 t1' && alphaEq t2 t2'
  alphaEq _ _ = False

-- definition of constraints

data Pred
  = InCls
      { predName :: Name,
        predMain :: Ty,
        predParams :: [Ty]
      }
  | Ty :~: Ty
  deriving (Eq, Ord, Show, Data, Typeable)

instance HasSourceSpan Pred where
  sourceSpanOf (InCls n t ts) =
    firstSourceSpan [sourceSpanOf n, sourceSpanOf t, sourceSpanOf ts]
  sourceSpanOf (left :~: right) =
    firstSourceSpan [sourceSpanOf left, sourceSpanOf right]

-- qualified types

data Qual t
  = [Pred] :=> t
  deriving (Eq, Ord, Show, Data, Typeable)

instance (HasSourceSpan t) => HasSourceSpan (Qual t) where
  sourceSpanOf (preds :=> t) =
    firstSourceSpan [sourceSpanOf preds, sourceSpanOf t]

infix 2 :=>

-- type schemes

data Scheme
  = Forall [Tyvar] (Qual Ty)
  deriving (Eq, Ord, Show, Data, Typeable)

instance HasSourceSpan Scheme where
  sourceSpanOf (Forall tyvars qualTy) =
    firstSourceSpan [sourceSpanOf tyvars, sourceSpanOf qualTy]

monotype :: Ty -> Scheme
monotype t = Forall [] ([] :=> t)

{-
A measure for types, predicates and constraints for the Patterson Condition 2:
"The constraint has fewer constructors and variables
(taken together and counting repetitions) than the head"
-}
class HasMeasure a where
  measure :: a -> Int

instance HasMeasure Ty where
  measure (TyVar _) = 1
  measure (Meta _) = 1
  measure (TyCon _ ts) = 1 + sum (map measure ts)

instance HasMeasure Pred where
  measure (InCls _ t as) = sum (map measure as) + measure t
  measure (t :~: u) = measure t + measure u

instance HasMeasure [Pred] where
  measure = sum . map measure
