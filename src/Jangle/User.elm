module Jangle.User exposing
    ( User
    , decoder
    , encode
    )

import Json.Decode as Decode exposing (Decoder, float, string)
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


encode : User -> Encode.Value
encode user =
    Encode.object
        [ ( "email", Encode.string user.email )
        , ( "name", Encode.string user.name )
        , ( "token", Encode.string user.token )
        ]
