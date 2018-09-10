module Jangle.List.ItemList exposing (ItemList, decoder)

import Jangle.List.Item as Item exposing (Item)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias ItemList =
    { total : Int
    , items : List Item
    }


decoder : Decoder ItemList
decoder =
    Decode.succeed ItemList
        |> required "total" int
        |> required "items" (list Item.decoder)
