module Solcore.Frontend.Syntax.Ty where 


import Data.Generics (Data, Typeable)
import Solcore.Frontend.Syntax.Name 

-- basic typing infrastructure 

data Tyvar 
  = TVar { var :: Name }
  deriving (Eq, Ord, Show, Data, Typeable)
   
data Ty 
  = TyVar Tyvar      -- type variable 
  | TyCon Name [Ty]  -- type constructor 
  deriving (Eq, Ord, Show, Data, Typeable)

infixr 4 :-> 

pattern (:->) a b 
  = TyCon (Name "->") [a, b]

argTy :: Ty -> [Ty]
argTy (t1 :-> t2) = t1 : argTy t2 
argTy _ = []

retTy :: Ty -> Maybe Ty
retTy (TyVar _) = Nothing 
retTy (t1 :-> t2) = ret t2
  where 
    ret (ta :-> tb) = ret tb 
    ret t = Just t
retTy t@(TyCon _ _) = Just t

splitTy :: Ty -> ([Ty], Ty)
splitTy (a :-> b) 
  = let (as, r) = splitTy b 
    in (a : as, r)
splitTy t = ([], t)

funtype :: [Ty] -> Ty -> Ty
funtype ts t = foldr (:->) t ts

class AlphaEq a where 
  alphaEq :: a -> a -> Bool

instance AlphaEq a => AlphaEq [a] where 
  alphaEq ts ts' = and $ zipWith alphaEq ts ts'

instance AlphaEq Ty where 
  alphaEq (TyVar _) (TyVar _) 
    = True 
  alphaEq (TyCon n ts) (TyCon n' ts')
    = n == n' && (and (zipWith alphaEq ts ts'))
  alphaEq _ _ 
    = False

instance AlphaEq Pred where 
  alphaEq (InCls n t ts) (InCls n' t' ts')
    = n == n' && alphaEq t t' && alphaEq ts ts'
  alphaEq (t1 :~: t2) (t1' :~: t2') 
    = alphaEq t1 t1' && alphaEq t2 t2'
  alphaEq _ _ = False  

-- definition of constraints 

data Pred = InCls {
              predName :: Name 
            , predMain :: Ty 
            , predParams :: [Ty]
            } 
          | Ty :~: Ty 
          deriving (Eq, Ord, Show, Data, Typeable)


-- qualified types 

data Qual t 
  = [Pred] :=> t 
    deriving (Eq, Ord, Show, Data, Typeable)

infix 2 :=> 

-- type schemes 

data Scheme 
  = Forall [Tyvar] (Qual Ty) 
    deriving (Eq, Ord, Show, Data, Typeable)

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
  measure (TyCon _ ts) = 1 + sum (map measure ts)

instance HasMeasure Pred where
  measure (InCls _ t as) = sum (map measure as) + measure t
  measure (t :~: u) = measure t + measure u

instance HasMeasure [Pred] where
  measure = sum . map measure
