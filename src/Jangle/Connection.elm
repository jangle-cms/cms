module Jangle.Connection exposing (Connection, connect, url)

import Http exposing (Body, Error(..))
import Jangle.User as User exposing (User)


type Connection
    = Connection String


connect : String -> Connection
connect baseUrl =
    Connection baseUrl


url : Connection -> String -> String
url (Connection baseUrl) path =
    baseUrl ++ path
