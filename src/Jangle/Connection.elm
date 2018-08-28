module Jangle.Connection exposing
    ( Connection
    , connect
    , url
    )

import Http exposing (Body, Error(..))


type Connection
    = Connection String


connect : String -> Connection
connect baseUrl =
    Connection baseUrl


url : Connection -> String -> String
url (Connection baseUrl) path =
    baseUrl ++ path
