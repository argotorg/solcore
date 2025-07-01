module Solcore.Desugarer.UniqueTypeGen where

import Control.Monad.State
import qualified Data.Map as Map
import Solcore.Frontend.Syntax
import Solcore.Frontend.Pretty.SolcorePretty

uniqueTypeGen :: CompUnit Name -> IO (CompUnit Name, UniqueTyMap)
uniqueTypeGen c@(CompUnit imps ds)
  = do
      env <- runUniqueM (uniqueTyGen c)
      let ds' = (TDataDef <$> Map.elems (uniqueMap env)) ++ ds
      pure (CompUnit imps ds', uniqueMap env)

class UniqueTypeGen a where
  uniqueTyGen :: a -> UniqueM ()

instance UniqueTypeGen a => UniqueTypeGen [a] where
  uniqueTyGen = mapM_ uniqueTyGen

instance UniqueTypeGen a => UniqueTypeGen (Maybe a) where
  uniqueTyGen Nothing = pure ()
  uniqueTyGen (Just x) = uniqueTyGen x

instance UniqueTypeGen (CompUnit Name) where
  uniqueTyGen (CompUnit _ ds)
    = uniqueTyGen ds

instance UniqueTypeGen (TopDecl Name) where
  uniqueTyGen (TContr c) = uniqueTyGen c
  uniqueTyGen (TFunDef f) = uniqueTyGen f
  uniqueTyGen (TClassDef c) = uniqueTyGen c
  uniqueTyGen _ = pure ()

instance UniqueTypeGen (FunDef Name) where
  uniqueTyGen (FunDef sig _) = uniqueTyGen sig

instance UniqueTypeGen (Signature Name) where
  uniqueTyGen sig
    = createUniqueType (sigName sig)

instance UniqueTypeGen (Class Name) where
  uniqueTyGen = uniqueTyGen . signatures

instance UniqueTypeGen (Contract Name) where
  uniqueTyGen (Contract _ _ ds)
    = uniqueTyGen ds

instance UniqueTypeGen (ContractDecl Name) where
  uniqueTyGen (CFunDecl fd) = uniqueTyGen fd
  uniqueTyGen _ = pure ()

-- creating a new unique type

createUniqueType :: Name -> UniqueM ()
createUniqueType n
  = do
      dn <- freshName ("t_" ++ pretty n)
      addUniqueType n (mkUniqueType dn)

mkUniqueType :: Name -> DataTy
mkUniqueType dn
  = let
      argVar = TVar (Name "args")
      retVar = TVar (Name "ret")
      c = Constr dn []
    in DataTy dn [] [c]

-- monad definition

type UniqueM a = StateT Env IO a

type UniqueTyMap = Map.Map Name DataTy

data Env = Env {
             uniqueMap :: UniqueTyMap
           , count :: Int
           }

runUniqueM :: UniqueM a -> IO Env
runUniqueM m
  = execStateT m (Env Map.empty 0)

addUniqueType :: Name -> DataTy -> UniqueM ()
addUniqueType n t
  = modify $ \ env -> env { uniqueMap = Map.insert n t (uniqueMap env) }

inc :: UniqueM Int
inc = do
  s <- get
  let c = count s
  put $ s { count = c + 1 }
  return c

freshName :: String -> UniqueM Name
freshName s
  = do
       n <- inc
       pure (Name $ s ++ show n)
