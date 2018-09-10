module Jangle.List.Item exposing (Item, decoder)

import Dict exposing (Dict)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Item =
    Dict String String


decoder : Decoder Item
decoder =
    Decode.dict string
