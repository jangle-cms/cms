module Jangle.List.Field exposing (Field, decoder)

import Json.Decode as Decode exposing (Decoder, bool, string)
import Json.Decode.Pipeline exposing (required)


type alias Field =
    { name : String
    , label : String
    , type_ : String
    , default : String
    , required : Bool
    }


decoder : Decoder Field
decoder =
    Decode.succeed Field
        |> required "name" string
        |> required "label" string
        |> required "type" string
        |> required "default" string
        |> required "required" bool
