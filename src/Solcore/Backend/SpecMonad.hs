module Solcore.Backend.SpecMonad where
import Control.Applicative
import Control.Monad
import Control.Monad.Error.Class
import Control.Monad.State
import Data.Generics
import Data.List(union, (\\))
import Data.Maybe(fromMaybe)
import qualified Data.Map as Map

import Common.Monad
import Common.Pretty
import Solcore.Backend.Retype
import Solcore.Frontend.Pretty.ShortName
import Solcore.Frontend.Pretty.SolcorePretty
import Solcore.Frontend.Syntax
import Solcore.Frontend.TypeInference.Id
import Solcore.Frontend.TypeInference.NameSupply
import Solcore.Frontend.TypeInference.TcEnv(TcEnv(typeTable),TypeInfo(..))
import Solcore.Frontend.TypeInference.TcUnify(typesDoNotUnify)


-- ** Specialisation state and monad
-- SpecState and SM are meant to be local to this module.
type Table a = Map.Map Name a
emptyTable :: Table a
emptyTable = Map.empty

type Resolution = (Ty, TcFunDef)
data SpecState = SpecState
  { spResTable :: Table [Resolution]
  , specTable :: Table TcFunDef
  , spTypeTable :: Table TypeInfo
  , spDataTable :: Table DataTy
  , spGlobalEnv :: TcEnv
  , splocalEnv :: Table Ty
  , spSubst :: TVSubst
  , spDebug :: Bool
  , spNS :: NameSupply
  }


type SM = StateT SpecState IO

getDebug :: SM Bool
getDebug = gets spDebug

withDebug m = do
    savedDebug <- getDebug
    modify $ \s -> s { spDebug = True }
    a <- m
    modify $ \s -> s { spDebug = savedDebug }
    return a

whenDebug m = do
    debug <- getDebug
    when debug m

debug :: [String] -> SM ()
debug msg = do
    enabled <- getDebug
    when enabled $ writes msg

runSM :: Bool -> TcEnv -> SM a -> IO a
runSM debugp env m = evalStateT m (initSpecState debugp env)


-- | `withLocalState` runs a computation with a local state
-- local changes are discarded, with the exception of the `specTable` and name supply
withLocalState :: SM a -> SM a
withLocalState m = do
    s <- get
    a <- m
    spTable <- gets specTable
    ns <- gets spNS
    put s
    modify $ \s -> s { specTable = spTable, spNS = ns }
    return a

initSpecState :: Bool ->TcEnv -> SpecState
initSpecState debugp env = SpecState
    { spResTable = emptyTable
    , specTable = emptyTable
    , spTypeTable = typeTable env
    , spDataTable = Map.empty
    , spGlobalEnv = env
    , splocalEnv = emptyTable
    , spSubst = emptyTVSubst
    , spDebug = debugp
    , spNS = namePool
    }


{-
-- make type variables flexible by replacing them with metas
flex :: Ty -> Ty
flex (TyVar (TVar n)) = Meta (MetaTv n)
flex (TyCon cn tys) = TyCon cn (map flex tys)
flex t = t

-- make all type variables flexible in a syntactic construct
flexAll :: Data a => a -> a
flexAll = everywhere (mkT flex)
-}

-- | A signature forall tvs . t is considered ambiguous if `tvs \\ FTV(t) /= mempty`
-- this is should be the same as `FTV(body) \\ FTV(t) /= {}`
-- returns list of ambiguous variables
ambiguousVarsInSig :: HasTV a => Signature a -> [Tyvar]
ambiguousVarsInSig sig = sigVars sig \\ freetv (sigParams sig, sigReturn sig)

addSpecialisation :: Name -> TcFunDef -> SM ()
addSpecialisation name fd = modify $ \s -> s { specTable = Map.insert name fd (specTable s) }

lookupSpecialisation :: Name -> SM (Maybe TcFunDef)
lookupSpecialisation name = gets (Map.lookup name . specTable)

addResolution :: Name -> Ty -> TcFunDef -> SM ()
addResolution name ty fun = do
    -- debug ["+ addResolution ", pretty name, "@", pretty ty, " |-> ", shortName fun]
    let sig = funSignature fun
    reportAmbiguousVars sig
    modify $ \s -> s { spResTable = Map.insertWith (++) name [(ty, fun)] (spResTable s) }
    where
      reportAmbiguousVars sig = do
        let vars = ambiguousVarsInSig sig
        let scheme = schemeOfTcSignature sig
        unless (null vars) $ nopanics [ "Error: function ", pretty name
                        ," cannot be specialised because it has an ambiguous type:\n   "
                        , pretty scheme
                        ,"\n variables: ", prettys vars
                        ,"\n do not occur in the argument/result types."
                        ]

lookupResolution :: Name -> Ty ->  SM (Maybe (TcFunDef, Ty, TVSubst))
lookupResolution name ty = gets (Map.lookup name . spResTable) >>= findMatch ty where
  str :: Pretty a => a -> String
  str = pretty
  findMatch :: Ty -> Maybe [Resolution] -> SM (Maybe (TcFunDef, Ty, TVSubst))
  findMatch etyp (Just res) = do
    debug ["|> findMatch ", pretty etyp, " in ", prettysWith pprRes res]
    firstMatch etyp res
  findMatch _ Nothing = return Nothing
  firstMatch :: Ty -> [Resolution] -> SM (Maybe (TcFunDef, Ty, TVSubst))
  firstMatch etyp [] = return Nothing
  firstMatch etyp ((t,e):rest)
    | Right subst <- specmgu t etyp = do  -- TESTME: match is to weak for MPTC, but isn't mgu too strong?
        debug ["< lookupRes - match found for ", str name, ": ", str t, " ~ ", str etyp, " => ", str subst]
        return (Just (e, t, subst))
    | otherwise = firstMatch etyp rest

pprRes :: Resolution -> Doc
pprRes(ty, fd) = ppr ty <+> text ":" <+> text(shortName fd)

getSpSubst :: SM TVSubst
getSpSubst = gets spSubst

putSpSubst :: TVSubst -> SM ()
putSpSubst subst = modify $ \s -> s { spSubst = subst }
extSpSubst :: TVSubst -> SM ()

extSpSubst subst = modify $ \s -> s { spSubst = spSubst s <> subst }

atCurrentSubst :: HasTV a => a -> SM a
atCurrentSubst a = flip applytv a <$> getSpSubst

addData :: DataTy -> SM ()
addData dt = modify (\s -> s { spDataTable = Map.insert (dataName dt) dt (spDataTable s) })

spNewName :: SM Name
spNewName = do
    s <- get
    let (n, ns) = newName (spNS s)
    put s { spNS = ns }
    pure (addPrefix "_" n)

-- data Name = Name String | QualName Name String
addPrefix :: String -> Name -> Name
addPrefix p (Name s) = Name (p ++ s)
addPrefix p (QualName q s) = QualName q (p ++ s)

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
-- >>> r1 <> mempty
-- TVR {unTVR = [(TVar {var = a},TVar {var = x}),(TVar {var = b},TVar {var = y})]}


instance Semigroup TVRenaming where
  r1 <> r2 = TVR (filter (uncurry (/=)) (outer ++ inner))
    where
      outer = [(u, renameTV r1 v) | (u, v) <- unTVR r2]
      inner = [(v, t) | (v, t) <- unTVR r1, v `notElem` domR2]
      domR2 = map fst (unTVR r2)

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
