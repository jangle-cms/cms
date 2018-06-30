module Jangle.Connection exposing (Connection, authenticate, connect, url, user)

import Http exposing (Body, Error(..))
import Jangle.User as User exposing (User)


type Connection
    = Connection String (Maybe User)


connect : String -> Connection
connect baseUrl =
    Connection baseUrl Nothing


authenticate : User -> Connection -> Connection
authenticate user_ (Connection baseUrl _) =
    Connection baseUrl (Just user_)


url : Connection -> String -> String
url (Connection baseUrl _) path =
    baseUrl ++ path


user : Connection -> Maybe User
user (Connection _ maybeUser) =
    case maybeUser of
        Just user_ ->
            Just user_

        Nothing ->
            Nothing
