module Types
    exposing
        ( AppMsg(..)
        , Context
        )

import Jangle exposing (Connection)


type alias Context =
    { connection : Connection
    }


type AppMsg
    = SetConnection Connection
