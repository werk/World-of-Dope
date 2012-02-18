module Dope.Server.Session where

import Dope.State.Utility
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)
import Data.Word
import System.Random

type SessionId = String
type SessionsVar a = TVar (Map SessionId a)

newSessionsVar :: IO (SessionsVar a)
newSessionsVar = newTVarIO $ Map.empty

newSession :: (SessionsVar a) -> a -> IO SessionId
newSession sessionsVar a = do
    sessionId <- generateSessionId
    atomically $ modifyTVar_ sessionsVar (Map.insert sessionId a)
    return sessionId    

useSession :: (SessionsVar a) -> SessionId -> IO (Maybe (a, SessionId))
useSession sessionsVar sessionId = atomically $ do
    sessions <- readTVar sessionsVar
    return $ do 
        a <- Map.lookup sessionId sessions
        return (a, sessionId)

generateSessionId :: IO SessionId
generateSessionId = do
    number1 <- randomIO :: IO Word
    number2 <- randomIO :: IO Word
    return $ show number1 ++ show number2
