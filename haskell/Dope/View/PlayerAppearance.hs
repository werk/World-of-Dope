{-# LANGUAGE TemplateHaskell #-}
module Dope.View.PlayerAppearance where

import qualified Dope.Model.Player as Player
import Dope.Model.Player (Player (Player))

import Dope.Model.DeriveJson
import Data.Label

data PlayerAppearance = PlayerAppearance {
    _name :: String
    }
$(mkLabels [''PlayerAppearance])
$(derive makeJSON ''PlayerAppearance)

fromPlayer :: Player -> PlayerAppearance
fromPlayer player = PlayerAppearance (get Player.name player)

