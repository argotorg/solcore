module Common.Monad where

import Control.Monad
import Control.Monad.IO.Class (MonadIO (..))
import GHC.Stack (HasCallStack)
import System.Exit (exitFailure)

writeln :: (MonadIO m) => String -> m ()
writeln = liftIO . putStrLn

writes :: (MonadIO m) => [String] -> m ()
writes = writeln . concat

errors :: (HasCallStack) => [String] -> a
errors = error . concat

panics :: (MonadIO m) => [String] -> m a
panics msgs = do
  liftIO $ putStrLn $ concat ("PANIC: " : msgs)
  liftIO exitFailure

nopanics :: (MonadIO m) => [String] -> m a
nopanics msgs = do
  liftIO $ putStrLn $ concat msgs
  liftIO exitFailure

warns :: (MonadIO m) => [String] -> m ()
warns = writes
