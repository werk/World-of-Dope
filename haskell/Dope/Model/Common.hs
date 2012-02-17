{-# LANGUAGE TemplateHaskell #-}
module Dope.Model.Common where

import Data.DeriveTH
import Data.Derive.JSON
import Text.JSON
import Data.Label

type Name = String
type SiteName = String
type PlayerName = String

data Position 
    = Position Int Int 
    deriving (Show, Eq)
$(derive makeJSON ''Position)

data Place
    = Street Position
    | Inside SiteName
    deriving (Show, Eq)
$(derive makeJSON ''Place)

data Situation
    = Idle
    | Busted
    | Trading PlayerName
    deriving (Show, Eq)
$(derive makeJSON ''Situation)

data Drug
    = Heroin
    | Cocaine
    | Meth
    | Crack
    | LSD
    | Ecstasy
    | Opium
    | Marijuana
    | Mushrooms
    deriving (Show, Eq)
$(derive makeJSON ''Drug)

data SiteType 
    = Club 
    | Jail
    deriving (Show, Eq)
$(derive makeJSON ''SiteType)
