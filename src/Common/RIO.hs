module Common.RIO
  ( RIO,
    runRIO,
    writeln,
    load,
    store,
    update,
    module Control.Monad.Reader,
    module Data.IORef,
  )
where

import Common.Monad
import Control.Monad.Reader
import Data.IORef

type RIO env a = ReaderT env IO a

-- instance MonadIO RIO

-- load :: MonadIO m => IORef a -> m a
load :: IORef a -> RIO env a
load = liftIO . readIORef

-- store :: MonadIO m =>IORef a -> a -> m ()
store :: IORef a -> a -> RIO env ()
store r v = liftIO $ writeIORef r v

-- update :: MonadIO m => IORef a -> (a->a) -> m ()
update :: IORef a -> (a -> a) -> RIO env ()
update f = liftIO . modifyIORef f

runRIO :: RIO env a -> env -> IO a
runRIO m env = runReaderT m env
