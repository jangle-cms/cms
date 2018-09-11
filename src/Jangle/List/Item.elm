module Jangle.List.Item exposing (Item, decoder, empty, toDict)

import Dict exposing (Dict)
import Jangle.List.JangleMeta as JangleMeta exposing (JangleMeta, Timestamp)
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Item =
    { id : String
    , name : String
    , jangle : JangleMeta
    }


empty : Item
empty =
    Item "" "" (JangleMeta 1 (Timestamp "" "") (Timestamp "" ""))


toDict : Item -> Dict String String
toDict item =
    Dict.fromList
        [ ( "_id", item.id )
        , ( "name", item.name )
        , ( "jangle.version", String.fromInt item.jangle.version )
        , ( "jangle.created.by", item.jangle.created.by )
        , ( "jangle.created.at", item.jangle.created.at )
        , ( "jangle.updated.by", item.jangle.updated.by )
        , ( "jangle.updated.by", item.jangle.updated.by )
        ]


decoder : Decoder Item
decoder =
    Decode.succeed Item
        |> required "_id" string
        |> required "name" string
        |> required "jangle" JangleMeta.decoder
