module Language.Hull.TcMonad
  ( HullTcM,
    runHullTcM,
    lookupVar,
    extendVar,
    withLocalEnv,
    lookupFun,
    extendFun,
    withRetType,
    getRetType,
    expectType,
    typeEq,
    hullError,
  )
where

import Control.Monad (unless)
import Control.Monad.Except
import Control.Monad.State
import Data.Map qualified as Map
import Language.Hull.TcEnv
import Language.Hull.Types

type HullTcM a = StateT HullTcEnv (ExceptT String IO) a

runHullTcM :: HullTcM a -> HullTcEnv -> IO (Either String a)
runHullTcM m env = runExceptT (evalStateT m env)

lookupVar :: String -> HullTcM Type
lookupVar x = do
  vars <- gets hull_vars
  case Map.lookup x vars of
    Just t -> pure t
    Nothing -> hullError ("Undefined variable: " ++ x)

extendVar :: String -> Type -> HullTcM ()
extendVar x t =
  modify (\e -> e {hull_vars = Map.insert x t (hull_vars e)})

-- Save and restore the variable context across a sub-computation.
-- Function registrations are intentionally not restored: Hull functions
-- are globally visible once defined (analogous to Yul function hoisting).
withLocalEnv :: HullTcM a -> HullTcM a
withLocalEnv m = do
  saved <- gets hull_vars
  x <- m
  modify (\e -> e {hull_vars = saved})
  pure x

lookupFun :: String -> HullTcM HullFunSig
lookupFun f = do
  funs <- gets hull_funs
  case Map.lookup f funs of
    Just sig -> pure sig
    Nothing -> hullError ("Undefined function: " ++ f)

extendFun :: String -> HullFunSig -> HullTcM ()
extendFun f sig =
  modify (\e -> e {hull_funs = Map.insert f sig (hull_funs e)})

withRetType :: Type -> HullTcM a -> HullTcM a
withRetType t m = do
  saved <- gets hull_ret
  modify (\e -> e {hull_ret = Just t})
  x <- m
  modify (\e -> e {hull_ret = saved})
  pure x

getRetType :: HullTcM (Maybe Type)
getRetType = gets hull_ret

-- Check that two types are equal modulo TNamed wrappers.
expectType :: Type -> Type -> HullTcM ()
expectType expected actual =
  unless (typeEq expected actual) $
    hullError $
      unlines
        [ "Type mismatch",
          "  expected: " ++ show expected,
          "  actual:   " ++ show actual
        ]

-- Structural type equality, transparent to TNamed.
typeEq :: Type -> Type -> Bool
typeEq t1 t2 = norm t1 == norm t2
  where
    norm (TNamed _ t) = norm t
    norm (TPair a b) = TPair (norm a) (norm b)
    norm (TSum a b) = TSum (norm a) (norm b)
    norm (TSumN ts) = TSumN (map norm ts)
    norm (TFun ts t) = TFun (map norm ts) (norm t)
    norm t = t

hullError :: String -> HullTcM a
hullError = throwError
