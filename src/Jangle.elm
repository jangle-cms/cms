module Jangle
    exposing
        ( Connection
        , User
        , authenticate
        , connect
        , user
        )

import Jangle.Connection as Connection
import Jangle.User as User


type alias Connection =
    Connection.Connection


type alias User =
    User.User


authenticate =
    Connection.authenticate


connect =
    Connection.connect


user =
    Connection.user
