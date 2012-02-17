{-# LANGUAGE TemplateHaskell #-}
module Dope.View.PlayerIntrospection where

import qualified Dope.Model.Player as Player
import Dope.Model.Player (Player (Player))

import Dope.Model.DeriveJson
import Data.Label

-- TODO: This should have its own fields, but I'm too lazy
data PlayerIntrospection = PlayerIntrospection {
    _player :: Player
    }
    deriving (Eq, Show)
$(mkLabels [''PlayerIntrospection])
$(derive makeJSON ''PlayerIntrospection)

fromPlayer :: Player -> PlayerIntrospection
fromPlayer player = PlayerIntrospection player

