module Jangle.List.Item exposing
    ( Item
    , ItemField
    , decoder
    , empty
    , encode
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
    , jangle : JangleMeta
    , fields : Dict String ItemField
    }


empty : Item
empty =
    Item
        ""
        ""
        (JangleMeta 1 (Timestamp "" "") (Timestamp "" ""))
        Dict.empty


decoder : Decoder Item
decoder =
    Decode.succeed Item
        |> required "_id" string
        |> required "name" string
        |> required "jangle" JangleMeta.decoder
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


intField : Int -> ItemField
intField =
    SomeInt


floatField : Float -> ItemField
floatField =
    SomeFloat


itemFieldDecoder : Decoder ItemField
itemFieldDecoder =
    Decode.oneOf
        [ map SomeString string
        , map SomeInt int
        , map SomeBool bool
        , map SomeObject (Decode.lazy (\_ -> dict itemFieldDecoder))
        , map SomeList (Decode.lazy (\_ -> list itemFieldDecoder))
        ]
