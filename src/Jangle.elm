module Jangle
    exposing
        ( Connection
        , Error
        , authenticate
        , connect
        , errorToString
        )

import Jangle.Connection as Connection
import Jangle.Request as Request


type alias Connection =
    Connection.Connection


authenticate =
    Connection.authenticate


connect =
    Connection.connect


type alias Error =
    Request.Error


errorToString =
    Request.errorToString
