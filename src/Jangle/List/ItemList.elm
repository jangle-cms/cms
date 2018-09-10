module Jangle.List.ItemList exposing (ItemList, decoder)

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias ItemList =
    { total : Int
    , items : List (List ( String, String ))
    }


decoder : Decoder ItemList
decoder =
    Decode.succeed ItemList
        |> required "total" int
        |> required "items" (list (Decode.keyValuePairs string))
