module Dope.Server.Http where

import Dope.Logic.Act
import qualified Dope.View.PlayerIntrospection as PlayerIntrospection
import qualified Dope.Server.Protocol as Protocol
import qualified Dope.State.GameState as GameState
import Dope.Model.Player (Player)
import Dope.State.GameState (GameState (GameState))

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Happstack.Server as H
import Control.Monad
import Control.Monad.Trans
import Control.Applicative.Error (maybeRead)
import Control.Concurrent.STM
import System.FilePath (joinPath, takeExtension) 
import qualified System.Directory as D
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Label
import Text.JSON

import Dope.State.Operation

main :: IO ()
main = do
    stateVar <- newGameState
    simpleHTTP configuration (handler stateVar)

configuration = Conf { 
    port = 8080,
    validator = Nothing,
    logAccess = Nothing,
    timeout = 10
    }

handler :: TVar GameState -> ServerPartT IO H.Response
handler stateVar = msum [
    dir "play" $ nullDir >> method [GET, POST] >> process stateVar,
    nullDir >> method [GET] >> serveFile (asContentType "text/html") "Client/index2.html"
    ]

parameter :: JSON a => String -> H.Request -> ServerPartT IO a
parameter name request = do
    let Just p = lookup name (rqInputsQuery request)
    let Input { inputValue = Right v } = p
    let Ok json = decode $ L.unpack v
    let Ok value = readJSON $ json
    return value

respond :: JSON a => a -> ServerPartT IO H.Response
respond value = return $ toResponse $ encode $ showJSON $ value

process :: TVar GameState -> ServerPartT IO H.Response
process stateVar = do
    request <- askRq
    option <- parameter "option" request
    playerName <- parameter "player" request
    playerVar <- liftIO $ getPlayerVar stateVar playerName
    case playerVar of 
        Just playerVar -> do
            (error, player, possibilities) <- liftIO $ actAndReportOptions stateVar playerVar option
            let p = PlayerIntrospection.fromPlayer player
            case error of
                Nothing -> respond (Protocol.OK p possibilities)
                Just reason -> respond (Protocol.Failure (Protocol.IllegalAct reason p possibilities))
        Nothing -> 
            respond (Protocol.Failure Protocol.PlayerDoesNotExist)

-- TODO The player TVar should be associated with the login session.
getPlayerVar :: TVar GameState -> String -> IO (Maybe (TVar Player))
getPlayerVar stateVar name = atomically $ do
    state <- readTVar stateVar
    return $ Map.lookup name (get GameState.playerVars state)
    