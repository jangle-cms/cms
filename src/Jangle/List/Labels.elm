module Jangle.List.Labels exposing (Labels, decoder)

import Json.Decode as Decode exposing (Decoder, string)
import Json.Decode.Pipeline exposing (required)


type alias Labels =
    { singular : String
    , plural : String
    }


decoder : Decoder Labels
decoder =
    Decode.succeed Labels
        |> required "singular" string
        |> required "plural" string
