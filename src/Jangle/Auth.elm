module Jangle.Auth exposing (canSignUp, signIn, signUp)

import Http
import Jangle exposing (Connection, Error)
import Jangle.User as User exposing (User)
import Json.Decode as Decode
import Json.Encode as Encode
import Task


canSignUp : Connection -> Task Error Bool
canSignUp =
    Jangle.get
        "/auth/can-sign-up"
        Decode.bool


encodeUserInfo : { email : String, name : String, password : String } -> Encode.Value
encodeUserInfo =
    Encode.object
        [ ( "email", email )
        , ( "name", name )
        , ( "password", password )
        ]


signUp : { email : String, name : String, password : String } -> Connection -> Task Error Connection
signUp userInfo =
    Jangle.post
        (encodeUserInfo userInfo)
        "/auth/sign-up"
        User.decoder
        connection
        |> Task.map (\user -> Jangle.authenticate user connection)


signIn : { email : String, password : String } -> Connection -> Task Error Connection
signIn { email, password } connection =
    Jangle.get
        ("/auth/sign-in?email=" ++ email ++ "&password=" ++ password)
        User.decoder
        connection
        |> Task.map (\user -> Jangle.authenticate user connection)
