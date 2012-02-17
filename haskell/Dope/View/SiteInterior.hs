{-# LANGUAGE TemplateHaskell #-}
module Dope.View.SiteInterior where

import Dope.Model.Common

import qualified Dope.View.SiteExterior as SiteExterior
import Dope.View.SiteExterior (SiteExterior (SiteExterior))

import qualified Dope.Model.Site as Site
import Dope.Model.Site (Site (Site))

import qualified Dope.Model.Player as Player
import Dope.Model.Player (Player (Player))

import qualified Dope.State.GameState as GameState
import Dope.State.GameState (GameState (GameState))

import Dope.Model.DeriveJson
import Data.Label

import Control.Monad
import Control.Concurrent.STM
import qualified Data.Map as Map
import Data.Map (Map)

data SiteInterior = SiteInterior {
    _exterior :: SiteExterior,
    _visitors :: [PlayerName]
    }
$(mkLabels [''SiteInterior])
$(derive makeJSON ''SiteInterior)

fromSite :: TVar GameState -> Site -> STM SiteInterior
fromSite stateVar site = do
    state <- readTVar stateVar
    let playerVars = get GameState.playerVars state
    let guestNames = get Site.guests site
    let guestVars = map (\name -> playerVars Map.! name) guestNames
    guests <- mapM readTVar guestVars
    return (SiteInterior (SiteExterior.fromSite site) (map (get Player.name) guests))

