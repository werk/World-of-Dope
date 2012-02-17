{-# LANGUAGE TemplateHaskell #-}
module Dope.Logic.Option where

import Dope.Model.Common as Common
import Dope.State.GameState as GameState
import Data.Label
import Control.Concurrent.STM
import qualified Data.Map as Map

data Optional a = Some a | None deriving (Show, Read)

instance Eq a => Eq (Optional a) where
    _ == _ = True

data Option 
    = TakeACap (Optional Position)
    | Enter SiteName
    | Exit
    | DealDrugs (Optional Int)
    | Trade PlayerName
    | AbortTrade
    | BribePolice (Optional Integer)
    | SnitchFriend PlayerName
    deriving Eq

