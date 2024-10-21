module Solcore.Frontend.TypeInference.SccAnalysis where 

import Control.Monad
import Control.Monad.Except
import Control.Monad.Identity 
import Control.Monad.Trans

import Data.Graph.Inductive hiding (mkEdges)
import Data.Graph.Inductive.Query.DFS
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map 

import Language.Yul

import Solcore.Frontend.Syntax.Contract
import Solcore.Frontend.Syntax.Name 
import Solcore.Frontend.Syntax.Stmt
import Solcore.Frontend.Syntax.Ty 
import Solcore.Frontend.Pretty.SolcorePretty

-- strong connect component analysis for building mutual blocks 

type SCC a = (ExceptT String IO) a 

sccAnalysis :: CompUnit Name -> IO (Either String (CompUnit Name))
sccAnalysis m = runExceptT (mkScc m)

mkScc :: CompUnit Name -> SCC (CompUnit Name)
mkScc (CompUnit imps cs) = CompUnit imps <$> depDecls cs

depDecls :: [TopDecl Name] -> SCC [TopDecl Name]
depDecls ds 
  = do 
      cs' <- mapM depContract cs
      (uncurry (++)) <$> depAnalysis (cs' ++ ds')
    where 
      (cs, ds') = partition isContract ds 
      isContract (TContr _) = True 
      isContract _ = False 

depContract :: TopDecl Name -> SCC (TopDecl Name)
depContract (TContr (Contract n vs ds))
  = do 
      let
        isFun (CFunDecl _) = True 
        isFun _ = False 
        (ds1, ds2) = partition isFun ds 
      (os, ds') <- depAnalysis ds1  
      pure (TContr $ Contract n vs (ds2 ++ os ++ ds'))
depContract d = pure d


-- generic dependency analysis algorithm

depAnalysis :: HasDeps a => [a] -> SCC ([a], [a])
depAnalysis ds 
  = do
      (cgraph, posMap, declMap) <- mkCallGraph ds
      newDecls <- rebuildDecls posMap declMap (scc cgraph)
      (cgraph', posMap', declMap') <- mkCallGraph newDecls
      newDecls' <- sortDecls posMap' declMap' (topsort cgraph')
      return (others, newDecls')
    where 
      (decls, others) = partition isDecl ds 

-- creating the call graph for the dependency analysis

mkCallGraph :: HasDeps a => [a] -> SCC ( Gr Name ()
                                       , Map Int Name 
                                       , Map Name a
                                       )
mkCallGraph ds 
  = do 
      let 
        nodes' = zip [0..] (concatMap nameOf ds)
        swap (x,y) = (y,x)
        valMap = Map.fromList (map swap nodes')
        declMap = mkDeclMap ds 
      table <- mkCallTable ds 
      edges' <- mkEdges table valMap 
      pure (mkGraph nodes' edges', Map.fromList nodes', declMap)

mkDeclMap :: HasDeps a => [a] -> Map Name a 
mkDeclMap 
  = foldr step Map.empty 
    where 
      step d ac = Map.union ac (Map.fromList (zip (nameOf d) 
                                                  (repeat d)))

mkEdges :: Map Name [Name] -> Map Name Int -> SCC [LEdge ()]
mkEdges tables pos 
  = foldM step [] (Map.toList tables)
  where 
    step ac (k, vs) = do 
      v' <- findPos k 
      vs' <- mapM findPos vs 
      let ac' = map (\ x -> (x, v', ())) vs' 
      pure (ac' ++ ac)
    findPos k 
      = case Map.lookup k pos of 
          Just n -> pure n 
          _ -> pure minBound

mkCallTable :: HasDeps a => [a] -> SCC (Map Name [Name])
mkCallTable ds 
  = do 
      let emptyTable = mkEmptyTable ds 
          funs = Map.keys emptyTable
          go d ac = Map.unionWith (++) 
                      ac 
                      (Map.fromList [(n, fv d) | n <- nameOf d])
          m = foldr go emptyTable ds 
      pure m 

mkEmptyTable :: HasDeps a => [a] -> Map Name [Name]
mkEmptyTable = foldr step Map.empty 
  where 
    step d ac = Map.union ac (Map.fromList (zip (nameOf d) (repeat [])))

-- rebuilding the whole module declaration list 

rebuildDecls :: HasDeps a => Map Int Name -> 
                             Map Name a -> 
                             [[Node]] -> 
                             SCC [a]
rebuildDecls posMap declMap 
  = mapM (rebuildDecl posMap declMap)

-- rebuilding a set of mutual declarations out of 
-- the node maps.

rebuildDecl :: HasDeps a => Map Int Name -> 
                            Map Name a -> 
                            [Node] -> 
                            SCC a 
rebuildDecl _ _ [] 
  = throwError "Impossible! Empty node list!"
rebuildDecl pmap dmap [n] 
  = rebuild pmap dmap n 
rebuildDecl pmap dmap ns 
  = mkMutual <$> mapM (rebuild pmap dmap) ns

-- getting a declaration out of the node map 

rebuild :: HasDeps a => Map Int Name -> 
                        Map Name a -> 
                        Node -> 
                        SCC a
rebuild pmap dmap n 
  = case Map.lookup n pmap of
      Just k -> 
        case Map.lookup k dmap of
          Just d -> pure d
          Nothing -> throwError ("Impossible! Undefined decl:" ++ (show k)) 
      Nothing -> throwError ("Impossible! Undefined decl:" ++ show n)

sortDecls :: HasDeps a => Map Int Name -> 
                          Map Name a -> 
                          [Node] -> 
                          SCC [a]
sortDecls posMap declMap nodes 
  = mapM (rebuild posMap declMap) nodes

-- type class for SCC analysis

class (Pretty a, FreeVars a) => HasDeps a where 
  nameOf :: a -> [Name]
  mkMutual :: [a] -> a
  isDecl :: a -> Bool 

instance HasDeps (TopDecl Name) where 
  nameOf (TFunDef fd) = [sigName $ funSignature fd]
  nameOf (TMutualDef ds) = concatMap nameOf ds
  nameOf (TContr c) = [name c]
  nameOf _ = []
  mkMutual = TMutualDef 
  isDecl (TFunDef _) = True
  isDecl (TContr _) = True
  isDecl _ = False 

instance HasDeps (ContractDecl Name) where 
  nameOf (CFunDecl fd) = [sigName $ funSignature fd]
  nameOf (CMutualDecl ds) = concatMap nameOf ds
  nameOf _ = []
  mkMutual = CMutualDecl 
  isDecl (CFunDecl _) = True 
  isDecl _ = False 

class FreeVars a where 
  fv :: a -> [Name]

instance FreeVars a => FreeVars [a] where 
  fv = foldr (union . fv) []

instance FreeVars (Class Name) where 
  fv _ = []

instance FreeVars (Exp Name) where
  fv c@(Con _ es) = fv es
  fv (FieldAccess Nothing _) = []
  fv (FieldAccess (Just e) _) = fv e 
  fv (Call _ n es) 
    | isPrimitive n = []
    | otherwise = n : fv es
  fv (Var v) 
    | isPrimitive v = [] 
    | otherwise = [v]
  fv (Lam args bd _) = fv bd \\ ns 
    where 
      ns = map f args 
      f (Typed n _) = n 
      f (Untyped n) = n 
  fv _ = []

isPrimitive :: Name -> Bool 
isPrimitive n = n `elem` [Name "primAddWord", Name "primEqWord"]

instance FreeVars (Stmt Name) where 
  fv (_ := e) = fv e 
  fv (Let n _ (Just e)) = fv e \\ [n]
  fv (StmtExp e) = fv e 
  fv (Return e) = fv e 
  fv (Match es eqns) = fv es `union` fv eqns 
  fv (Asm blk) = fv blk
  fv _ = []

instance FreeVars YulStmt where
  fv (YBlock blk) = fv blk 
  fv (YFun n args rets ss)
    = fv ss \\ (n : args `union` (maybe [] id rets))
  fv (YLet ns (Just e)) = fv e \\ ns 
  fv (YIf e blk) = fv e `union` fv blk 
  fv (YSwitch e cs d) 
    = fv e `union` fv cs' `union` (maybe [] fv d)
      where cs' = map snd cs
  fv (YFor is e bd up) 
    = fv e `union` fv bd `union` fv up \\ fv is 
  fv (YExp e) = fv e 
  fv _ = []

instance FreeVars YulExp where 
  fv (YCall n es) = n : fv es 
  fv (YIdent n) = [n]
  fv _ = []

instance FreeVars (FunDef Name) where 
  fv (FunDef sig ss) = fv ss \\ ps 
    where 
      ps = (map f (sigParams sig)) ++ letDefs 
      f (Typed n _) = n 
      f (Untyped n) = n 
      
      letDefs = foldr step [] ss 
      step (Let n _ _) ac = n : ac 
      step _ ac = ac 


instance FreeVars (ContractDecl Name) where 
  fv (CFunDecl fd) = fv fd 
  fv (CMutualDecl ds) = fv ds
  fv _ = []

instance FreeVars (TopDecl Name) where 
  fv (TFunDef fd) = fv fd 
  fv (TMutualDef ds) = fv ds
  fv (TInstDef idecl) = fv idecl
  fv (TContr c) = fv c 
  fv _ = []

instance FreeVars (Contract Name) where 
  fv (Contract _ _ ds) = fv ds 

instance FreeVars (Instance Name) where 
  fv (Instance _ _ _ _ fds) = fv fds 

instance FreeVars (Equation Name) where 
  fv = fv . snd 


