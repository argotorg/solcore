module Solcore.Frontend.TypeInference.SccAnalysis where

import Algebra.Graph.AdjacencyMap
import qualified Algebra.Graph.NonEmpty.AdjacencyMap as N
import Algebra.Graph.AdjacencyMap.Algorithm

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity
import Control.Monad.Trans
import Control.Monad.Writer

import Data.List
import Data.List.NonEmpty (toList)
import Data.Maybe
import Data.Map (Map)
import qualified Data.Map as Map

import Language.Yul

import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty
import Solcore.Frontend.Pretty.SolcorePretty

-- strong connect component analysis for building mutual blocks

sccAnalysis :: CompUnit Name -> IO (Either String (CompUnit Name))
sccAnalysis cunit
  = do
      r <- runSCC (sccAnalysis' cunit)
      case r of
        Left err -> pure $ Left err
        Right (cunit', logs) -> pure $ Right cunit'

sccAnalysis' :: CompUnit Name -> SCC (CompUnit Name)
sccAnalysis' (CompUnit imps ds)
  = do
      cs' <- mapM sccContract cs
      CompUnit imps <$> analysis (cs' ++ ds')
    where
      isContract (TContr _) = True
      isContract _ = False

      (cs, ds') = partition isContract ds

-- sort inner contract definitions

sccContract :: TopDecl Name -> SCC (TopDecl Name)
sccContract (TContr (Contract n vs ds))
  = (TContr . Contract n vs) <$> analysis ds
sccContract d = pure d

analysis :: (Ord a, Names a, Decl a, Show a, Groupable a) => [a] -> SCC [a]
analysis ds
  = do
      let grph = mkGraph ds
          cmps = scc grph
      case topSort cmps of
        Left _ -> pure []
        Right ds' -> pure $ reverse $ concatMap (groupMutualDefs . toList . N.vertexList1) ds'

-- building the dependency graph

mkGraph :: (Ord a, Names a, Decl a) => [a] -> AdjacencyMap a
mkGraph ds = stars $ mkEdges (mkNameEnv ds) ds

-- definition of enviroment of definition names

type NameEnv a = Map Name a

lookupDef :: Ord a => Name -> NameEnv a -> Maybe a
lookupDef n env = Map.lookup n env

lookupDefs :: Ord a => [Name] -> NameEnv a -> [a]
lookupDefs ns env = mapMaybe (flip lookupDef env) ns

mkNameEnv :: (Ord a, Names a, Decl a) => [a] -> NameEnv a
mkNameEnv = foldr go Map.empty
  where
    go d ac = let ns = decl d
              in foldr (\ n m -> Map.insert n d m) ac ns

-- creating graph edges

mkEdges :: (Ord a, Names a, Decl a) => NameEnv a -> [a] -> [(a,[a])]
mkEdges env = foldr step []
  where
    step d ac = (d, lookupDefs (names d) env) : ac


-- definition of dependency analysis using type classes

class Decl a where
  decl :: a -> [Name]

instance Decl a => Decl [a] where
  decl = concatMap decl

instance Decl a => Decl (Maybe a) where
  decl Nothing = []
  decl (Just x) = decl x

instance Decl Constr where
  decl (Constr n _) = [n]

instance Decl DataTy where
  decl (DataTy n _ cs)
    = n : decl cs

instance Decl TySym where
  decl (TySym n _ _) = [n]

instance Decl (Signature Name) where
  decl s = [sigName s]

instance Decl (FunDef Name) where
  decl (FunDef sig _) = decl sig

instance Decl (Contract Name) where
  decl (Contract n _ ds) = n : concatMap decl ds

instance Decl (Field Name) where
  decl d = [fieldName d]

instance Decl (TopDecl Name) where
  decl (TContr c) = decl c
  decl (TFunDef fd) = decl fd
  decl (TMutualDef ds) = decl ds
  decl (TDataDef d) = decl d
  decl (TSym t) = decl t
  decl _ = []


instance Decl (ContractDecl Name) where
  decl (CDataDecl dt) = decl dt
  decl (CFieldDecl fd) = decl fd
  decl (CFunDecl fd) = decl fd
  decl (CMutualDecl ds)
    = concatMap decl ds
  decl (CConstrDecl cd) = []

-- getting the mentioned names in a declaration

class Names a where
  names :: a -> [Name]

instance Names a => Names [a] where
  names = foldr (union . names) []

instance Names a => Names (Maybe a) where
  names Nothing = []
  names (Just x) = names x

instance Names (Exp Name) where
  names (Con n es) = n : names es
  names (FieldAccess me n) = n : names me
  names (Call me n es)
    = n : names me `union` names es
  names (Lam ps bdy mt) =
    names ps `union` names bdy `union` names mt
  names (TyExp e t) = names e `union` names t
  names (Var n) = [n]
  names (Lit _) = []

instance Names (Param Name) where
  names (Typed _ t)
    = names t
  names _ = []

instance Names (Stmt Name) where
  names (e1 := e2)
    = names [e1, e2]
  names (Let _ mt me)
    = names mt `union` names me
  names (StmtExp e)
    = names e
  names (Return e)
    = names e
  names (Match es eqns)
    = names es `union` names eqns
  names (Asm _) = []
  names (If e blk1 blk2)
    = names e `union` names blk1 `union` names blk2

instance Names (Equation Name) where
  names (_, bdy) = names bdy

instance Names (Signature Name) where
  names (Signature _ ctx _ ps mret)
    = names ctx `union` names ps `union` names mret

instance Names (FunDef Name) where
  names (FunDef sig bdy)
    = names sig `union` names bdy

instance Names (Constructor Name) where
  names (Constructor ps bdy)
    = names ps `union` names bdy

instance Names (Class Name) where
  names (Class _ ctx _ _ _ sigs)
    = names ctx `union` names sigs

instance Names (Instance Name) where
  names (Instance _ _ ctx n ts t funs)
    = [n] `union` names ctx `union` names (t : ts)  `union` names funs

instance Names Ty where
  names (TyCon n ts)
    = n : names ts
  names _ = []

instance Names Pred where
  names (InCls n t ts)
    = n : names (t : ts)
  names (t1 :~: t2)
    = names [t1, t2]

instance Names (Field Name) where
  names (Field _ t me)
    = names t `union` names me

instance Names TySym where
  names (TySym _ _ t)
    = names t

instance Names Constr where
  names (Constr _ ts)
    = names ts

instance Names DataTy where
  names (DataTy _ _ cs)
    = names cs

instance Names (ContractDecl Name) where
  names (CDataDecl dt) = names dt
  names (CFieldDecl fd) = names fd
  names (CFunDecl fd) = names fd
  names (CMutualDecl cs) = names cs
  names (CConstrDecl cd) = names cd

instance Names (Contract Name) where
  names (Contract _ _ decls)
    = names decls

instance Names (TopDecl Name) where
  names (TContr c) = names c
  names (TFunDef fd) = names fd
  names (TClassDef c) = names c
  names (TInstDef instd) = names instd
  names (TMutualDef ts) = names ts
  names (TDataDef dt) = names dt
  names (TSym ts) = names ts
  names _ = []

instance Names (CompUnit Name) where
  names (CompUnit _ decls) = names decls

-- Groupable class for handling mutual definitions

class Groupable a where
  isFunctionDef :: a -> Bool
  wrapMutualDefs :: [a] -> [a]

instance Groupable (TopDecl Name) where
  isFunctionDef (TFunDef _) = True
  isFunctionDef _ = False
  wrapMutualDefs xs = [TMutualDef xs]

instance Groupable (ContractDecl Name) where
  isFunctionDef (CFunDecl _) = True
  isFunctionDef _ = False
  wrapMutualDefs xs = [CMutualDecl xs]

groupMutualDefs :: Groupable a => [a] -> [a]
groupMutualDefs xs =
  if (all isFunctionDef xs && length xs > 1)
  then wrapMutualDefs xs
  else xs

-- monad definition

type SCC a = WriterT [String] (ExceptT String IO) a

runSCC :: SCC a -> IO (Either String (a, [String]))
runSCC m = runExceptT (runWriterT m)


