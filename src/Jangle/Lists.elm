module Jangle.Lists exposing
    ( ListInfo
    , index
    )

import Jangle.Connection exposing (Connection)
import Jangle.Request
import Jangle.User exposing (User)
import Json.Decode as Decode exposing (Decoder, float, string)
import Json.Decode.Pipeline exposing (required)
import Task exposing (Task)


type alias ListInfo =
    { name : String
    , slug : String
    , labels : Labels
    }


type alias Labels =
    { singular : String
    , plural : String
    }


listInfoDecoder : Decoder ListInfo
listInfoDecoder =
    Decode.succeed ListInfo
        |> required "name" string
        |> required "slug" string
        |> required "labels" labelsDecoder


labelsDecoder : Decoder Labels
labelsDecoder =
    Decode.succeed Labels
        |> required "singular" string
        |> required "plural" string


index : User -> Connection -> Task String (List ListInfo)
index user =
    Jangle.Request.getAs
        user
        "/lists"
        (Decode.list listInfoDecoder)
