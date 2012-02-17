{-# LANGUAGE TemplateHaskell #-}
module Dope.View.SiteExterior where

import Dope.Model.Common

import qualified Dope.Model.Site as Site
import Dope.Model.Site (Site (Site))

import Dope.Model.DeriveJson
import Data.Label

data SiteExterior = SiteExterior {
    _name :: String,
    _type :: SiteType,
    _position :: Position
    }
$(mkLabels [''SiteExterior])
$(derive makeJSON ''SiteExterior)

fromSite :: Site -> SiteExterior
fromSite site = SiteExterior (get Site.name site) (get Site.type site) (get Site.position site)

