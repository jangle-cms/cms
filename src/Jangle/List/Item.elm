module Jangle.List.Item exposing
    ( Item
    , ItemField
    , decoder
    , empty
    , encode
    , intField
    , intFrom
    , objectFrom
    , stringField
    , stringFrom
    )

import Dict exposing (Dict)
import Jangle.List.JangleMeta as JangleMeta exposing (JangleMeta, Timestamp)
import Json.Decode as Decode
    exposing
        ( Decoder
        , at
        , bool
        , dict
        , float
        , int
        , list
        , map
        , string
        )
import Json.Decode.Pipeline exposing (custom, required)
import Json.Encode as Encode


type alias Item =
    { id : String
    , name : String
    , fields : Dict String ItemField
    }


empty : Item
empty =
    Item
        ""
        ""
        Dict.empty


decoder : Decoder Item
decoder =
    Decode.succeed Item
        |> required "_id" string
        |> required "name" string
        |> custom (at [] (dict itemFieldDecoder))


encode : Item -> Encode.Value
encode { fields } =
    encodeFields fields


encodeFields : Dict String ItemField -> Encode.Value
encodeFields fields =
    fields
        |> Dict.toList
        |> List.map (\( key, field ) -> ( key, encodeField field ))
        |> Encode.object


encodeField : ItemField -> Encode.Value
encodeField field =
    case field of
        SomeString val ->
            Encode.string val

        SomeInt val ->
            Encode.int val

        SomeFloat val ->
            Encode.float val

        SomeBool val ->
            Encode.bool val

        SomeObject dict ->
            encodeFields dict

        SomeList fields ->
            Encode.list encodeField fields



-- ItemField


type ItemField
    = SomeString String
    | SomeInt Int
    | SomeFloat Float
    | SomeBool Bool
    | SomeObject (Dict String ItemField)
    | SomeList (List ItemField)


stringFrom : ItemField -> Maybe String
stringFrom field =
    case field of
        SomeString str ->
            Just str

        _ ->
            Nothing


stringField : String -> ItemField
stringField =
    SomeString


intFrom : ItemField -> Maybe Int
intFrom field =
    case field of
        SomeInt val ->
            Just val

        _ ->
            Nothing


intField : Int -> ItemField
intField =
    SomeInt


floatField : Float -> ItemField
floatField =
    SomeFloat


objectFrom : ItemField -> Maybe (Dict String ItemField)
objectFrom field =
    case field of
        SomeObject val ->
            Just val

        _ ->
            Nothing


itemFieldDecoder : Decoder ItemField
itemFieldDecoder =
    Decode.oneOf
        [ map SomeString string
        , map SomeInt int
        , map SomeBool bool
        , map SomeObject (Decode.lazy (\_ -> dict itemFieldDecoder))
        , map SomeList (Decode.lazy (\_ -> list itemFieldDecoder))
        ]
