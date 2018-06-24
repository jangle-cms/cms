module Jangle.Auth exposing (canSignUp, signIn, signUp)

import Http
import Jangle exposing (Connection)
import Jangle.Request exposing (Error)
import Jangle.User as User exposing (User)
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)


canSignUp : Connection -> Task Error Bool
canSignUp =
    Jangle.Request.get
        "/auth/can-sign-up"
        Decode.bool


encodeUserInfo : { email : String, name : String, password : String } -> Encode.Value
encodeUserInfo { email, name, password } =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "name", Encode.string name )
        , ( "password", Encode.string password )
        ]


signUp : { email : String, name : String, password : String } -> Connection -> Task Error Connection
signUp userInfo connection =
    Jangle.Request.post
        (encodeUserInfo userInfo)
        "/auth/sign-up"
        User.decoder
        connection
        |> Task.map (\user -> Jangle.authenticate user connection)


signIn : { email : String, password : String } -> Connection -> Task Error Connection
signIn { email, password } connection =
    Jangle.Request.get
        ("/auth/sign-in?email=" ++ email ++ "&password=" ++ password)
        User.decoder
        connection
        |> Task.map (\user -> Jangle.authenticate user connection)
