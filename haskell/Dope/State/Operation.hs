{-# LANGUAGE TemplateHaskell #-}
module Dope.State.Operation where

import Dope.State.Utility
import qualified Dope.State.GameState as GameState
import Dope.State.GameState (GameState)

import Dope.Model.Common
import qualified Dope.Model.Player as Player
import Dope.Model.Player (Player)
import qualified Dope.Model.Site as Site
import Dope.Model.Site (Site)
import qualified Dope.Model.DrugBag as DrugBag
import Dope.Model.DrugBag (DrugBag)

import Data.Label
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)


-----------------------------
-- GameState
-----------------------------

newGameState :: IO (TVar GameState)
newGameState = atomically $ do
    let jail = Site "The Prison" Jail (Position 0 0) []
    let smokey = Site "Smokey Joe's" Club (Position 4 4) []
    let sams = Site "Sams Bar" Club (Position 2 6) []
    siteVars <- makeSiteVars [jail, smokey, sams]
    newTVar $ GameState Map.empty siteVars
    
-----------------------------
-- Player
-----------------------------

-- | Create a new player and put her into the nearest jail. 
-- | Returns Nothing if the requested player name is unavailable.
newPlayerVar :: TVar GameState -> String -> STM (Maybe (TVar Player))
newPlayerVar stateVar name = do
    state <- readTVar stateVar
    let names = Map.keys (get State.playerVars state)
    if elem name names
        then return Nothing
        else do
            let siteVars = (Map.elems (get State.siteVars state))
            jail <- getJail siteVars
            let jailPosition = get sitePosition jail
            let player = Player name Idle (Street jailPosition) 100 [] True
            var <- newTVar player
            let playerVars = Map.insert name var (get State.playerVars state)
            let state' = set State.playerVars playerVars state
            writeTVar stateVar state'
            return (Just var)


-- | Maintains the invariant that a player is only in one place at a time,
-- | even though this fact is represented in multiple places.
movePlayer :: GameState -> PlayerName -> Place -> STM ()
movePlayer state playerName place = do
    let Just playerVar = Map.lookup playerName (get statePlayerVars state)
    player <- readTVar playerVar
    case get playerPlace player of
        Inside siteName -> do
            let Just siteVar = Map.lookup siteName (get stateSiteVars state)
            modifyTVar siteVar (modify siteGuestVars (filter (/= playerVar)))
        Street _ -> return ()
    writeTVar playerVar (set playerPlace place player)
    case place of
        Inside siteName -> do
            let Just siteVar = Map.lookup siteName (get stateSiteVars state)
            modifyTVar siteVar (modify siteGuestVars (playerVar:))
        Street _ -> return ()


-----------------------------
-- Site
-----------------------------
            
makeSiteVars :: [Site] -> STM (Map SiteName (TVar Site))
makeSiteVars sites = do
    pairs <- forM sites (\s -> do 
        var <- newTVar s
        return (get siteName s, var))
    return $ Map.fromList pairs


getJail :: [TVar Site] -> STM Site
getJail siteVars = do
    hits <- filterVars siteVars (\site -> get siteType site == Jail)
    case hits of
        [] -> error "No jail found"
        siteVar : _ -> readTVar siteVar
