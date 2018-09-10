module Jangle.List.Item exposing (Item, decoder)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Item =
    { total : Int
    , items : List (List ( String, String ))
    }


decoder : Decoder Item
decoder =
    Decode.succeed Item
        |> required "total" int
        |> required "items" (list (Decode.keyValuePairs string))
