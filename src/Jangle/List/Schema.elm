module Jangle.List.Schema exposing (Schema, decoder)

import Jangle.List.Field as Field exposing (Field)
import Jangle.List.Labels as Labels exposing (Labels)
import Json.Decode as Decode exposing (Decoder, bool, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Schema =
    { name : String
    , slug : String
    , labels : Labels
    , fields : List Field
    }


decoder : Decoder Schema
decoder =
    Decode.succeed Schema
        |> required "name" string
        |> required "slug" string
        |> required "labels" Labels.decoder
        |> required "fields" (list Field.decoder)
