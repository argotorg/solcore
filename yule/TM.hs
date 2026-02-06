module TM
  ( TM,
    runTM,
    CEnv (..),
    -- , module RIO
    module Locus,
    FunInfo (..),
    getCounter,
    setCounter,
    freshId,
    lookupVar,
    insertVar,
    lookupFun,
    insertFun,
    getVarEnv,
    putVarEnv,
    withLocalEnv,
    debug,
  )
where

import Common.Monad
import Common.RIO
import Control.Monad (when)
import Data.Map (Map)
import Data.Map qualified as Map
import Language.Hull qualified as Hull
import Locus
import Options (Options)
import Options qualified

type VarEnv = Map String Location

type FunEnv = Map String FunInfo

data FunInfo = FunInfo {fun_args :: [Hull.Type], fun_result :: Hull.Type}

data CEnv = CEnv
  { env_counter :: IORef Int,
    env_vars :: IORef VarEnv,
    env_funs :: IORef FunEnv,
    env_options :: Options
  }

type TM a = RIO CEnv a

runTM :: Options -> TM a -> IO a
runTM options m = do
  counter <- newIORef 0
  vars <- newIORef Map.empty
  funs <- newIORef (Map.fromList builtinFuns)
  runRIO m (CEnv counter vars funs options)

getCounter :: TM Int
getCounter = reader env_counter >>= load

setCounter :: Int -> TM ()
setCounter n = reader env_counter >>= flip store n

getDebug :: TM Bool
getDebug = reader (Options.debug . env_options)

whenDebug m = do
  debugp <- getDebug
  when debugp m

debug :: [String] -> TM ()
debug msg = whenDebug $ writes msg

freshId :: TM Int
freshId = do
  counter <- reader env_counter
  n <- load counter
  store counter (n + 1)
  return n

lookupVar :: String -> TM Location
lookupVar x = do
  vars <- getVarEnv
  case Map.lookup x vars of
    Just n -> return n
    Nothing -> error ("Variable not found: " ++ x)

insertVar :: String -> Location -> TM ()
insertVar x n = do
  vars <- reader env_vars
  update vars (Map.insert x n)

lookupFun :: String -> TM FunInfo
lookupFun f = do
  funs <- getFunEnv
  case Map.lookup f funs of
    Just n -> return n
    Nothing -> error ("Function not found: " ++ f)

insertFun :: String -> FunInfo -> TM ()
insertFun f n = do
  funs <- reader env_funs
  update funs (Map.insert f n)

getVarEnv :: TM VarEnv
getVarEnv = load =<< reader env_vars

putVarEnv :: VarEnv -> TM ()
putVarEnv m = do
  vars <- reader env_vars
  store vars m

getFunEnv :: TM FunEnv
getFunEnv = load =<< reader env_funs

putFunEnv :: FunEnv -> TM ()
putFunEnv m = do
  funs <- reader env_funs
  store funs m

withLocalEnv :: TM a -> TM a
withLocalEnv m = do
  vars <- getVarEnv
  funs <- getFunEnv
  x <- m
  putVarEnv vars
  putFunEnv funs
  return x

builtinFuns :: [(String, FunInfo)]
builtinFuns =
  [ ("stop", FunInfo [] Hull.TUnit),
    ("add", FunInfo [Hull.TWord, Hull.TWord] Hull.TWord),
    ("mul", FunInfo [Hull.TWord, Hull.TWord] Hull.TWord),
    ("sub", FunInfo [Hull.TWord, Hull.TWord] Hull.TWord),
    ("div", FunInfo [Hull.TWord, Hull.TWord] Hull.TWord),
    ("sdiv", FunInfo [Hull.TWord, Hull.TWord] Hull.TWord),
    ("mod", FunInfo [Hull.TWord, Hull.TWord] Hull.TWord),
    ("smod", FunInfo [Hull.TWord, Hull.TWord] Hull.TWord),
    ("addmod", FunInfo [Hull.TWord, Hull.TWord, Hull.TWord] Hull.TWord),
    ("mulmod", FunInfo [Hull.TWord, Hull.TWord, Hull.TWord] Hull.TWord),
    ("exp", FunInfo [Hull.TWord, Hull.TWord] Hull.TWord)
  ]
