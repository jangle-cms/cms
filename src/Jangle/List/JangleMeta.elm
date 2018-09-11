module Jangle.List.JangleMeta exposing
    ( JangleMeta
    , Timestamp
    , decoder
    )

import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias JangleMeta =
    { version : Int
    , created : Timestamp
    , updated : Timestamp
    }


type alias Timestamp =
    { by : String
    , at : String
    }


decoder : Decoder JangleMeta
decoder =
    Decode.succeed JangleMeta
        |> required "version" int
        |> required "created" timestampDecoder
        |> required "updated" timestampDecoder


timestampDecoder : Decoder Timestamp
timestampDecoder =
    Decode.succeed Timestamp
        |> required "by" string
        |> required "at" string
