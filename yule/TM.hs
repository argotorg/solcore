module TM
( TM
, runTM
, CEnv(..)
--, module RIO
, module Locus
, FunInfo(..)
, getCounter
, setCounter
, freshId
, maybeLookupVar
, lookupVar
, insertVar
, lookupFun
, insertFun
, getVarEnv
, putVarEnv
, withLocalEnv
, debug
) where
import Common.Monad
import Common.RIO
import Control.Monad(when)
import qualified Data.Map as Map
import Data.Map(Map)

import Locus
import Language.Core qualified as Core
import qualified Options
import Options(Options)

type VarEnv = Map String Location
type FunEnv = Map String FunInfo
data FunInfo = FunInfo { fun_args :: [Core.Type], fun_result :: Core.Type}
data CEnv = CEnv
    { env_counter :: IORef Int
    , env_vars :: IORef VarEnv
    , env_funs :: IORef FunEnv
    , env_options :: Options
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
    store counter (n+1)
    return n

maybeLookupVar :: String -> TM (Maybe Location)
maybeLookupVar x = Map.lookup x <$> getVarEnv

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
    [ ("stop", FunInfo [] Core.TUnit)
    , ("add", FunInfo [Core.TWord, Core.TWord] Core.TWord)
    , ("mul", FunInfo [Core.TWord, Core.TWord] Core.TWord)
    , ("sub", FunInfo [Core.TWord, Core.TWord] Core.TWord)
    , ("div", FunInfo [Core.TWord, Core.TWord] Core.TWord)
    , ("sdiv", FunInfo [Core.TWord, Core.TWord] Core.TWord)
    , ("mod", FunInfo [Core.TWord, Core.TWord] Core.TWord)
    , ("smod", FunInfo [Core.TWord, Core.TWord] Core.TWord)
    , ("addmod", FunInfo [Core.TWord, Core.TWord, Core.TWord] Core.TWord)
    , ("mulmod", FunInfo [Core.TWord, Core.TWord, Core.TWord] Core.TWord)
    , ("exp", FunInfo [Core.TWord, Core.TWord] Core.TWord)
    ]
