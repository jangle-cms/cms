module Jangle.User exposing
    ( User
    , decoder
    )

import Json.Decode as Decode exposing (Decoder, float, int, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode


type alias User =
    { email : String
    , name : String
    , token : String
    }


decoder : Decoder User
decoder =
    Decode.succeed User
        |> required "email" string
        |> required "name" string
        |> required "token" string
