module Jangle.Connection exposing (Connection, authenticate, connect, token, url)

import Http exposing (Body, Error(..))
import Jangle.User as User exposing (User)


type Connection
    = Connection String (Maybe User)


connect : String -> Connection
connect baseUrl =
    Connection baseUrl Nothing


authenticate : User -> Connection -> Connection
authenticate user (Connection baseUrl _) =
    Connection baseUrl (Just user)


url : Connection -> String -> String
url (Connection baseUrl _) path =
    baseUrl ++ path


token : Connection -> Maybe String
token (Connection _ user) =
    case user of
        Just user_ ->
            Just (User.token user_)

        Nothing ->
            Nothing
