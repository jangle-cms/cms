module Jangle.User
    exposing
        ( User
        , decoder
        , email
        , name
        , token
        )

import Json.Decode as Decode exposing (Decoder, float, int, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias Email =
    String


type alias DisplayName =
    String


type alias Token =
    String


type User
    = User Email DisplayName Token


email : User -> Email
email (User email_ _ _) =
    email_


name : User -> DisplayName
name (User _ name_ _) =
    name_


token : User -> Token
token (User _ _ token_) =
    token_


decoder : Decoder User
decoder =
    Decode.succeed User
        |> required "email" string
        |> required "name" string
        |> required "token" string
