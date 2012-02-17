{-# LANGUAGE TemplateHaskell #-}
module Dope.Model.DrugBag where

import Dope.Model.Common

import Dope.Model.DeriveJson
import Data.Label

data DrugBag = DrugBag {
    _drug :: Drug,
    _seller :: PlayerName,
    _purity :: Double,
    _units :: Int
    } deriving (Show, Eq)
$(mkLabels [''DrugBag])
$(derive makeJSON ''DrugBag)
