{-# LANGUAGE TemplateHaskell #-}
module Dope.Model.Player where

import Dope.Model.Common
import Dope.Model.DrugBag (DrugBag)

import Dope.Model.DeriveJson
import Data.Label

data Player = Player {
    _name :: String,
    _situation :: Situation,
    _place :: Place,
    _money :: Integer,
    _drugBags :: [DrugBag],
    _online :: Bool
    } deriving (Show, Eq)
$(mkLabels [''Player])
$(derive makeJSON ''Player)
