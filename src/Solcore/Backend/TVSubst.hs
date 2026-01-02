module Solcore.Backend.TVSubst where

import Control.Monad.Error.Class
import Data.Data

import Common.Pretty
import Solcore.Backend.SpecMonad+
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id

specmgu :: Ty -> Ty -> Either String TVSubst
specmgu (TyCon n ts) (TyCon n' ts')
  | n == n' && length ts == length ts' =
      specsolve (zip ts ts') mempty
specmgu (TyVar v) t = varBind v t
specmgu t (TyVar v) = varBind v t
specmgu t1 t2 = typesDoNotUnify t1 t2

varBind :: (MonadError String m) => Tyvar -> Ty -> m TVSubst
varBind v t
  | t == TyVar v = return mempty
  | v `elem` freetv t = infiniteTyErr v t
  | otherwise = do
      return (v |-> t)
  where
    infiniteTyErr w u = throwError $
      unwords
        [ "Cannot construct the infinite type:"
        , pretty w
        , "~"
        , pretty u
        ]

specsolve :: [(Ty, Ty)] -> TVSubst -> Either String TVSubst
specsolve [] s = pure s
specsolve ((t1, t2) : ts) s =
  do
    s1 <- specmgu (applytv s t1) (applytv s t2)
    s2 <- specsolve ts s1
    pure (s2 <> s1)

newtype TVSubst
  = TVSubst { unTVSubst :: [(Tyvar, Ty)] } deriving (Eq, Show)

restrict :: TVSubst -> [Tyvar] -> TVSubst
restrict (TVSubst s) vs
  = TVSubst [(v,t) | (v,t) <- s, v `notElem` vs]

emptyTVSubst :: TVSubst
emptyTVSubst = TVSubst []

-- composition operators
-- apply (s1 <> s2) t = apply s1 (apply s2 t)
{-
-- >>> let [a,b,c,x,y] = map (TVar . Name) (Prelude.words "a b c x y")
-- >>> [a,b,c,x,y]
-- [TVar {var = a},TVar {var = b},TVar {var = c},TVar {var = x},TVar {var = y}]
ghci> let [ta,tb,tc,tx,ty] = map TyVar [a,b,c,x,y]
ghci> [ta, tb, tc, tx, ty]
[TyVar (TVar {var = a}),TyVar (TVar {var = b}),TyVar (TVar {var = c}),TyVar (TVar {var = x}),TyVar (TVar {var = y})]

ghci> let s1 = TVSubst [(a,tx), (b,ty)]
ghci> let s2 = TVSubst [(a,tb), (b,tc), (c,ta)]
ghci> s1 <> s1
TVSubst {unTVSubst = [(TVar {var = a},TyVar (TVar {var = x})),(TVar {var = b},TyVar (TVar {var = y}))]}
ghci> s1 <> s2
TVSubst {unTVSubst = [(TVar {var = a},TyVar (TVar {var = y})),(TVar {var = b},TyVar (TVar {var = c})),(TVar {var = c},TyVar (TVar {var = x}))]}
ghci> s2 <> s2
TVSubst {unTVSubst = [(TVar {var = a},TyVar (TVar {var = c})),(TVar {var = b},TyVar (TVar {var = a})),(TVar {var = c},TyVar (TVar {var = b}))]}
ghci> s2 <> s2 <> s2
TVSubst {unTVSubst = [(TVar {var = a},TyVar (TVar {var = a})),(TVar {var = b},TyVar (TVar {var = b})),(TVar {var = c},TyVar (TVar {var = c}))]}
-}
instance Semigroup TVSubst where
  s1 <> s2 = TVSubst (outer ++ inner)
    where
      outer = [(u, applytv s1 t) | (u, t) <- unTVSubst s2]
      inner = [(v,t) | (v,t) <- unTVSubst s1, v `notElem` dom2]
      dom2 = map fst (unTVSubst s2)

instance Monoid TVSubst where
  mempty = emptyTVSubst

(|->) :: Tyvar -> Ty -> TVSubst
u |-> t = TVSubst [(u, t)]

instance Pretty TVSubst where
  ppr = braces . commaSep . map go . unTVSubst
    where
      go (v,t) = ppr v <+> text "|->" <+> ppr t

class Data a => HasTV a where
  applytv :: TVSubst -> a -> a
  applytv s = everywhere (mkT (applytv @Ty s))

  freetv  :: a -> [Tyvar]    -- free variables
  freetv = everything (<>) (mkQ mempty (freetv @Ty))

  renametv :: a -> SM (a, TVRenaming)
  renametv a = pure (a, mempty)

  applyRenaming :: TVRenaming -> a -> a
  applyRenaming r = everywhere (mkT (renameTV r))

instance HasTV Ty where
  applytv (TVSubst s) t@(TyVar v)
    = maybe t id (lookup v s)
  applytv s (TyCon n ts)
    = TyCon n (applytv s ts)
  applytv _ t = t

  freetv (TyVar v@(TVar _)) = [v]
  freetv (TyCon _ ts) = freetv ts
  freetv _ = []

instance HasTV a => HasTV [a] where
    applytv s = map (applytv s)
    freetv = foldr (union . freetv) mempty

instance HasTV a => HasTV (Maybe a) where
  applytv s = fmap (applytv s)
  freetv = maybe [] freetv

instance (HasTV a, HasTV b) => HasTV (a,b) where  -- defaults

{-
instance (HasTV a, HasTV b, HasTV c) => HasTV (a,b,c) where
  applytv s (z,x,y) = (applytv s z, applytv s x, applytv s y)
  freetv (z,x,y) = freetv z `union` freetv x `union` freetv y

instance (HasTV a, HasTV b) => HasTV (a,b) where
  applytv s (x,y) = (applytv s x, applytv s y)
  freetv (x,y) = freetv x `union` freetv y
-}

instance HasTV Id where
  applytv s (Id n t) = Id n (applytv s t)
  freetv (Id _ t) = freetv t

instance HasTV a => HasTV (Param a) where -- defaults
instance HasTV a => HasTV (Exp a) where  -- defaults
instance HasTV a => HasTV (Stmt a) where  -- defaults

instance HasTV (Pat Id) where


instance HasTV (Signature Id) where
    applytv s = everywhere (mkT (applytv @Ty s))
    freetv sig = (everything (<>) (mkQ mempty (freetv @Ty))) sig \\ sigVars sig
    renametv sig = do
      renaming <- foldM addRenaming mempty (sigVars sig)
      pure (applyRenaming renaming sig, renaming)


{-
data FunDef a
  = FunDef {
      funSignature :: Signature a
    , funDefBody :: [Stmt a]
    } deriving (Eq, Ord, Show, Data, Typeable)
-}

instance HasTV (FunDef Id) where
    freetv fd = (everything (<>) (mkQ mempty (freetv @Ty))) fd \\ sigVars (funSignature fd)
    renametv fd = do
      let sig = funSignature fd
      renaming <- foldM addRenaming mempty (sigVars sig)
      let subst = toTVS renaming
      let sig' = applytv subst sig
      let body' = applytv subst (funDefBody fd)
      pure(FunDef sig' body', renaming)

addRenaming :: TVRenaming -> Tyvar -> SM TVRenaming
addRenaming b a = do
           fresh <- spNewName
           pure (TVR [(a, TVar fresh)] <> b)

-- TODO: refactor - make renametv return TVRenaming; turn rename* into class methods

newtype TVRenaming
  = TVR { unTVR :: [(Tyvar, Tyvar)] } deriving (Eq, Show)

instance Pretty TVRenaming where
  ppr = braces . commaSep . map go . unTVR
    where
      go (v,t) = ppr v <+> text "|->" <+> ppr t

-- composition operators
-- apply (s1 <> s2) t = apply s1 (apply s2 t)
--  renameTy ([(a,x) (b,y)] <> [(a,b),  (b,c), (c,a)]) (a :-> b :-> c)
--  = renameTy ([(a,x) (b,y)] (b :-> c :-> a)
--  = y :-> c :-> x
-- Hence ([(a,x) (b,y)] <> [(a,b), (b,c), (c,a)]) = [(a,y), (b,c), (c,x)]
--
-- >>> let [a,b,c,x,y] = map (TVar . Name) (Prelude.words "a b c x y")
-- >>> [a,b,c,x,y]
-- [TVar {var = a},TVar {var = b},TVar {var = c},TVar {var = x},TVar {var = y}]
-- >>> let r1 = TVR [(a,x), (b,y)]
-- >>> let r2 = TVR [(a,b), (b,c), (c,a)]
-- >>> r1 <> r1
-- TVR {unTVR = [(TVar {var = a},TVar {var = x}),(TVar {var = b},TVar {var = y})]}
-- >>> r1 <> r2
-- TVR {unTVR = [(TVar {var = a},TVar {var = y}),(TVar {var = b},TVar {var = c}),(TVar {var = c},TVar {var = x})]}
-- >>> r2 <> r2
-- TVR {unTVR = [(TVar {var = a},TVar {var = c}),(TVar {var = b},TVar {var = a}),(TVar {var = c},TVar {var = b})]}
-- >>> r2 <> r2 <> r2
-- TVR {unTVR = []}

instance Semigroup TVRenaming where
  r1 <> r2 = TVR (filter (uncurry (/=)) [ (u, renameTV r1 v) | (u, v) <- unTVR r2])

instance Monoid TVRenaming where
  mempty = TVR mempty

toTVS :: TVRenaming -> TVSubst
toTVS = TVSubst . map (fmap TyVar) . unTVR

fromTVS :: TVSubst -> TVRenaming
fromTVS = TVR . map (fmap unTyVar) . unTVSubst where
    unTyVar (TyVar x) = x
    unTyVar t = error("fromTVS: " ++ pretty t ++ "is not a type variable")

renameTV :: TVRenaming -> Tyvar -> Tyvar
renameTV (TVR r) v = fromMaybe v (lookup v r)

renameTy :: TVRenaming -> Ty -> Ty
renameTy  = applyRenaming

renameSubst :: TVRenaming -> TVSubst -> TVSubst
renameSubst r = TVSubst . map rename . unTVSubst where
    rename (v, t) = (renameTV r v, renameTy r t)
