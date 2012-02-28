module Dope.Server.Http where

import Dope.Logic.Act
import qualified Dope.View.PlayerIntrospection as PlayerIntrospection
import Dope.Server.Protocol
import Dope.Server.Session
import qualified Dope.State.GameState as GameState
import qualified Dope.State.Operation as Operation
import Dope.Model.Player (Player)
import Dope.State.GameState (GameState (GameState))
import Dope.Logic.City
import System.Random (mkStdGen)

import qualified Data.ByteString.Char8 as S
import qualified Data.ByteString.Lazy.Char8 as L
import Happstack.Server as H
import Control.Monad
import Control.Monad.Trans
import Control.Applicative.Error (maybeRead)
import Control.Concurrent.STM (TVar)
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
    sessionsVar <- newSessionsVar
    stateVar <- newGameState
    simpleHTTP configuration (handler sessionsVar stateVar)

configuration = Conf { 
    port = 8080,
    validator = Nothing,
    logAccess = Nothing,
    timeout = 10
    }

handler :: SessionsVar (TVar Player) -> TVar GameState -> ServerPartT IO H.Response
handler sessionsVar stateVar = msum [
    dir "login" $ nullDir >> method [GET, POST] >> login sessionsVar stateVar ,
    dir "play" $ nullDir >> method [GET, POST] >> play sessionsVar stateVar,
    --nullDir >> method [GET] >> serveFile (asContentType "text/html") "../html/index.html",
    serveDirectory EnableBrowsing ["index.html"] "../html/"
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

tiles = generateMap (mkStdGen 42) 30 20

login :: SessionsVar (TVar Player) -> TVar GameState -> ServerPartT IO H.Response
login sessionsVar stateVar = do
    request <- askRq
    playerName <- parameter "playerName" request
    -- TODO password
    playerVar <- liftIO $ Operation.getPlayerVar stateVar playerName
    case playerVar of 
        Just playerVar -> do
            sessionId <- liftIO $ newSession sessionsVar playerVar
            (possibilities, player) <- liftIO $ optionsIO playerVar stateVar
            let p = PlayerIntrospection.fromPlayer player
            respond $ OK sessionId p possibilities tiles
        Nothing -> 
            respond $ Failure "" PlayerDoesNotExist

play :: SessionsVar (TVar Player) -> TVar GameState -> ServerPartT IO H.Response
play sessionsVar stateVar = do
    request <- askRq
    sessionId <- parameter "sessionId" request
    option <- parameter "option" request
    pair <- liftIO $ useSession sessionsVar sessionId
    case pair of 
        Just (playerVar, nextSessionId) -> do
            (error, player, possibilities) <- liftIO $ actAndReportOptions stateVar playerVar option
            let p = PlayerIntrospection.fromPlayer player
            case error of
                Nothing -> respond $ OK nextSessionId p possibilities tiles
                Just reason -> respond $ Failure sessionId (IllegalAct reason)
        Nothing -> 
            respond (Failure sessionId NotLoggedIn)

