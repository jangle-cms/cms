module Jangle.List exposing (FindConfig, JangleList, find, init, schema)

import Jangle.Connection exposing (Connection)
import Jangle.List.Item as Item exposing (Item)
import Jangle.List.Schema as Schema exposing (Schema)
import Jangle.Request
import Jangle.User exposing (User)
import Json.Decode as Decode exposing (Decoder, bool, list, string)
import Json.Decode.Pipeline exposing (required)
import Task exposing (Task)


type JangleList
    = JangleList String User Connection


type alias FindConfig =
    { where_ : Maybe (List ( String, String ))
    , skip : Maybe Int
    , limit : Maybe Int
    , populate : Maybe (List String)
    , select : Maybe (List String)
    , sort : Maybe String
    }


init : String -> User -> Connection -> JangleList
init =
    JangleList


schema : JangleList -> Task String Schema
schema (JangleList slug user connection) =
    Jangle.Request.get
        ("/lists/" ++ slug ++ "/schema")
        Schema.decoder
        connection


find : FindConfig -> JangleList -> Task String Item
find { where_, skip, limit, populate, select, sort } (JangleList slug user connection) =
    let
        query : String
        query =
            [ where_
                |> Maybe.map
                    (List.map (\( key, value ) -> key ++ ": " ++ value)
                        >> String.join ", "
                        >> (\str -> "{ " ++ str ++ " }")
                    )
                |> Maybe.map (\val -> "where=" ++ val)
            , skip
                |> Maybe.map String.fromInt
                |> Maybe.map (\val -> "skip=" ++ val)
            , limit
                |> Maybe.map String.fromInt
                |> Maybe.map (\val -> "limit=" ++ val)
            , populate
                |> Maybe.map (String.join ",")
                |> Maybe.map (\val -> "populate=" ++ val)
            , select
                |> Maybe.map (String.join ",")
                |> Maybe.map (\val -> "select=" ++ val)
            , sort
                |> Maybe.map (\val -> "sort=" ++ val)
            ]
                |> List.filterMap identity
                |> String.join "&"
    in
    Jangle.Request.get
        ("/lists/" ++ slug ++ "?" ++ query)
        Item.decoder
        connection