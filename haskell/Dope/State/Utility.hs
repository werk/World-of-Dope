module Dope.State.Utility where

import Control.Concurrent.STM
import Control.Monad

modifyTVar :: TVar a -> (a -> a) -> STM a
modifyTVar var function = do
    value <- readTVar var
    writeTVar var (function value)
    return value

modifyTVar_ :: TVar a -> (a -> a) -> STM ()
modifyTVar_ var function = do
    value <- readTVar var
    writeTVar var (function value)

filterVars :: [TVar a] -> (a -> Bool) -> STM [TVar a]
filterVars vars predicate = 
    filterM (\var -> readTVar var >>= (return . predicate)) vars
