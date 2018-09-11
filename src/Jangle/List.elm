module Jangle.List exposing
    ( CreateConfig
    , FindConfig
    , GetConfig
    , JangleList
    , create
    , find
    , get
    , init
    , schema
    )

import Dict exposing (Dict)
import Jangle.Connection exposing (Connection)
import Jangle.List.Item as Item exposing (Item)
import Jangle.List.ItemList as ItemList exposing (ItemList)
import Jangle.List.Schema as Schema exposing (Schema)
import Jangle.Request
import Jangle.User exposing (User)
import Json.Decode as Decode exposing (Decoder, bool, list, string)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as Encode
import Task exposing (Task)


type JangleList
    = JangleList String User Connection


init : String -> User -> Connection -> JangleList
init =
    JangleList



-- SCHEMA


schema : JangleList -> Task String Schema
schema (JangleList slug user connection) =
    Jangle.Request.get
        ("/lists/" ++ slug ++ "/schema")
        Schema.decoder
        connection



-- FIND


type alias FindConfig =
    { where_ : Maybe (List ( String, String ))
    , skip : Maybe Int
    , limit : Maybe Int
    , populate : Maybe (List String)
    , select : Maybe (List String)
    , sort : Maybe String
    }


find : FindConfig -> JangleList -> Task String ItemList
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
                |> Maybe.map (String.join " ")
                |> Maybe.map (\val -> "populate=" ++ val)
            , select
                |> Maybe.map (String.join " ")
                |> Maybe.map (\val -> "select=" ++ val)
            , sort
                |> Maybe.map (\val -> "sort=" ++ val)
            ]
                |> List.filterMap identity
                |> String.join "&"
    in
    Jangle.Request.getAs user
        ("/lists/" ++ slug ++ "?" ++ query)
        ItemList.decoder
        connection



-- GET


type alias GetConfig =
    { populate : Maybe (List String)
    , select : Maybe (List String)
    }


get : String -> GetConfig -> JangleList -> Task String Item
get id { populate, select } (JangleList slug user connection) =
    let
        query : String
        query =
            [ populate
                |> Maybe.map (String.join ",")
                |> Maybe.map (\val -> "populate=" ++ val)
            , select
                |> Maybe.map (String.join ",")
                |> Maybe.map (\val -> "select=" ++ val)
            ]
                |> List.filterMap identity
                |> String.join "&"
    in
    Jangle.Request.getAs user
        ("/lists/" ++ slug ++ "/" ++ id ++ "?" ++ query)
        Item.decoder
        connection



-- CREATE


type alias CreateConfig =
    { item : Dict String String
    }


create : Item -> JangleList -> Task String Item
create item (JangleList slug user connection) =
    Jangle.Request.postAs user
        (Item.encode item)
        ("/lists/" ++ slug)
        Item.decoder
        connection
