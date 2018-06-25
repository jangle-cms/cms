module Jangle.Auth exposing (User, canSignUp, signIn, signUp)

import Http
import Jangle exposing (Connection)
import Jangle.Request
import Jangle.User as User
import Json.Decode as Decode
import Json.Encode as Encode
import Task exposing (Task)


type alias User =
    User.User


canSignUp : Connection -> Task String Bool
canSignUp =
    Jangle.Request.get
        "/auth/can-sign-up"
        Decode.bool


encodeUserSignUpInfo : { email : String, name : String, password : String } -> Encode.Value
encodeUserSignUpInfo { email, name, password } =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "name", Encode.string name )
        , ( "password", Encode.string password )
        ]


encodeUserSignInInfo : { email : String, password : String } -> Encode.Value
encodeUserSignInInfo { email, password } =
    Encode.object
        [ ( "email", Encode.string email )
        , ( "password", Encode.string password )
        ]


signUp : { email : String, name : String, password : String } -> Connection -> Task String User
signUp userInfo connection =
    Jangle.Request.post
        (encodeUserSignUpInfo userInfo)
        "/auth/sign-up"
        User.decoder
        connection


signIn : { email : String, password : String } -> Connection -> Task String User
signIn info connection =
    Jangle.Request.post
        (encodeUserSignInInfo info)
        "/auth/sign-in"
        User.decoder
        connection
