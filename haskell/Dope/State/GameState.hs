{-# LANGUAGE TemplateHaskell #-}
module Dope.State.GameState where

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

data GameState = GameState {
    _statePlayerVars :: Map PlayerName (TVar Player),
    _stateSiteVars :: Map SiteName (TVar Site)
    }
$(mkLabels [''GameState])
