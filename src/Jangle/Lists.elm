module Jangle.Lists exposing
    ( ListInfo
    , index
    )

import Jangle.Connection exposing (Connection)
import Jangle.List.Labels as Labels exposing (Labels)
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


listInfoDecoder : Decoder ListInfo
listInfoDecoder =
    Decode.succeed ListInfo
        |> required "name" string
        |> required "slug" string
        |> required "labels" Labels.decoder


index : User -> Connection -> Task String (List ListInfo)
index user =
    Jangle.Request.getAs
        user
        "/lists"
        (Decode.list listInfoDecoder)
