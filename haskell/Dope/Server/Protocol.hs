{-# LANGUAGE TemplateHaskell #-}
module Dope.Server.Protocol where

import Dope.View.PlayerIntrospection (PlayerIntrospection)
import Dope.Server.Session (SessionId)
import Dope.Logic.Option
import Dope.Model.DeriveJson


data Request
    = UsePlayer String
    | NewPlayer String
    | Act Option
    | NoAct
    | Quit
    deriving (Show, Eq)
$(derive makeJSON ''Request)
    
data Error
    = InvalidRequest
    | IllegalAct String
    | PlayerDoesNotExist
    | NotLoggedIn
    | PlayerAlreadyExists
    deriving (Show, Eq)
$(derive makeJSON ''Error)

data Response
    = OK SessionId PlayerIntrospection [Option]
    | Failure SessionId Error
    | Bye
    deriving (Show, Eq)
$(derive makeJSON ''Response)


