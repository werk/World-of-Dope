{-# LANGUAGE TemplateHaskell #-}
module Dope.Model.Site where

import Dope.Model.Common

import Dope.Model.DeriveJson
import Data.Label

data Site = Site {
    _name :: SiteName,
    _type :: SiteType,
    _position :: Position,
    _guests :: [PlayerName]
    } deriving (Show, Eq)
$(mkLabels [''Site])
$(derive makeJSON ''Site)
