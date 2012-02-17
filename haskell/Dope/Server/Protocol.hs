module Protocol where

import Dope
import State
import ClientData
import Data.DeriveTH
import Data.Derive.JSON
import Text.JSON


data Request
    = UsePlayer String
    | NewPlayer String
    | Act Option
    | Quit
    deriving (Show, Read, Eq)
$(derive makeJSON ''Request)
    
data Response
    = OK PlayerIntrospection [Option]
    | Error Error
    | Bye
    deriving (Show, Read, Eq)
$(derive makeJSON ''Response)

data Error
    = InvalidRequest
    | IllegalAct String PlayerIntrospection [Option]
    | PlayerDoesNotExist
    | NotLoggedIn
    | PlayerAlreadyExists
    deriving (Show, Read, Eq)
$(derive makeJSON ''Error)

