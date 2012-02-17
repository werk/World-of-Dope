{-# LANGUAGE TemplateHaskell #-}
module Dope.Server.Protocol where

import Dope.View.PlayerIntrospection (PlayerIntrospection)
import Dope.Logic.Option
import Dope.Model.DeriveJson


data Request
    = UsePlayer String
    | NewPlayer String
    | Act Option
    | Quit
    deriving (Show, Eq)
$(derive makeJSON ''Request)
    
data Error
    = InvalidRequest
    | IllegalAct String PlayerIntrospection [Option]
    | PlayerDoesNotExist
    | NotLoggedIn
    | PlayerAlreadyExists
    deriving (Show, Eq)
$(derive makeJSON ''Error)

data Response
    = OK PlayerIntrospection [Option]
    | Failure Error
    | Bye
    deriving (Show, Eq)
$(derive makeJSON ''Response)


