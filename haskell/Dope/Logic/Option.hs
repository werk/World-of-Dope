{-# LANGUAGE TemplateHaskell, FlexibleInstances, TypeSynonymInstances #-}
module Dope.Logic.Option where

import Dope.Model.Common as Common
import Dope.State.GameState as GameState
import Dope.Model.DeriveJson
import Text.JSON.Types
import Data.Label
import Data.Word
import Control.Concurrent.STM
import qualified Data.Map as Map

-- | This type is used to remedy that we use Option to model two slightly
-- | different concepts: When we responds to clients we use Option to model
-- | the possible actions the client may take. When the client responds, Option
-- | is used to model the action chosen. Some actions are parametric as modelled
-- | using Parameter. The parameters carries no values when possibilities is
-- | send to the client but the client must provide values for then when
-- | responding with the chosen Option.
data Parameter a
    = Parameter a
    | Requested
    deriving (Show, Read)

class InputType a where
    showType :: Parameter a -> String
instance InputType Int where
    showType _ = "Int"
instance InputType Position where
    showType _ = "Position"

instance (JSON a, InputType a) => JSON (Parameter a) where
    readJSON (JSObject (JSONObject {fromJSObject = [("Parameter", JSArray [v])]})) = do
        v <- readJSON v
        return (Parameter v)
    readJSON (JSObject (JSONObject {fromJSObject = [("Requested", _)]})) = return Requested

    showJSON (Parameter v) = JSObject (JSONObject {fromJSObject = [("Parameter", JSArray [showJSON v])]})
    showJSON v@Requested = JSObject (JSONObject {fromJSObject = [("Requested", JSArray [showJSON (showType v)])]})


instance Eq a => Eq (Parameter a) where
    _ == _ = True

data Option 
    = TakeACap (Parameter Position)
    | Enter SiteName
    | Exit
    | DealDrugs (Parameter Int)
    | Trade PlayerName
    | AbortTrade
    | BribePolice (Parameter Int)
    | SnitchFriend PlayerName
    deriving (Show, Eq)
$(derive makeJSON ''Option)


