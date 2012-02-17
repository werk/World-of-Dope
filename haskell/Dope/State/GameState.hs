{-# LANGUAGE TemplateHaskell #-}
module Dope.State.GameState where

import Dope.Model.Common
import Dope.Model.Player (Player)
import Dope.Model.Site (Site)

import Data.Label
import Control.Concurrent.STM
import Data.Map (Map)

data GameState = GameState {
    _playerVars :: Map PlayerName (TVar Player),
    _siteVars :: Map SiteName (TVar Site)
    }
$(mkLabels [''GameState])
