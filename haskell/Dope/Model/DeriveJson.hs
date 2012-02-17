-- |A convenience module exporting the modules needed to derive JSON using template haskell.
module Dope.Model.DeriveJson (
    module Data.DeriveTH,
    module Data.Derive.JSON,
    module Text.JSON
) where

import Data.DeriveTH
import Data.Derive.JSON
import Text.JSON
