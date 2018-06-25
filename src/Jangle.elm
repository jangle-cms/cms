module Jangle
    exposing
        ( Connection
        , authenticate
        , connect
        )

import Jangle.Connection as Connection


type alias Connection =
    Connection.Connection


authenticate =
    Connection.authenticate


connect =
    Connection.connect
