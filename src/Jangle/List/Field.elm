module Jangle.List.Field exposing
    ( Field
    , NestedFields
    , decoder
    , fieldsFrom
    )

import Json.Decode as Decode exposing (Decoder, bool, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Field =
    { name : String
    , label : String
    , type_ : String
    , ref : String
    , isList : Bool
    , required : Bool
    , fields : NestedFields
    }


type NestedFields
    = NestedFields (List Field)


fieldsFrom : NestedFields -> List Field
fieldsFrom (NestedFields fields) =
    fields


decoder : Decoder Field
decoder =
    Decode.succeed Field
        |> required "name" string
        |> required "label" string
        |> required "type" string
        |> required "ref" string
        |> required "isList" bool
        |> required "required" bool
        |> required "fields"
            (Decode.map NestedFields (list (Decode.lazy (\_ -> decoder))))
