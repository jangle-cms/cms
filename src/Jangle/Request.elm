module Jangle.Request
    exposing
        ( Error
        , errorToString
        , get
        , post
        )

import Http exposing (Body)
import Jangle.Connection exposing (Connection, token, url)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Task exposing (Task)


type Error
    = Error String


errorToString : Error -> String
errorToString (Error reason) =
    reason


type alias Response a =
    { error : Bool
    , message : String
    , data : a
    }


type alias EmptyResponse =
    { error : Bool
    , message : String
    }


decoder : Decoder a -> Decoder (Response a)
decoder dataDecoder =
    Decode.succeed Response
        |> required "error" Decode.bool
        |> required "message" Decode.string
        |> required "data" dataDecoder


emptyDecoder : Decoder EmptyResponse
emptyDecoder =
    Decode.succeed EmptyResponse
        |> required "error" Decode.bool
        |> required "message" Decode.string


convertError : Http.Error -> Error
convertError error =
    (case error of
        Http.BadUrl _ ->
            "Invalid URL."

        Http.Timeout ->
            "Request timed out."

        Http.NetworkError ->
            "Cannot connect to network."

        Http.BadStatus { body } ->
            case Decode.decodeString emptyDecoder body of
                Ok { message } ->
                    message

                Err err ->
                    let
                        _ =
                            Decode.errorToString err
                    in
                    "There was a bad status code."

        Http.BadPayload reason _ ->
            reason
    )
        |> Error


type Method
    = Get
    | Post
    | Patch
    | Delete


methodToString : Method -> String
methodToString method =
    case method of
        Get ->
            "GET"

        Post ->
            "POST"

        Patch ->
            "PATCH"

        Delete ->
            "DELETE"



-- Requests


request : Method -> Body -> String -> Decoder a -> Connection -> Task Error a
request method body path decoder_ connection =
    Http.request
        { url = url connection path
        , method = methodToString method
        , headers =
            case token connection of
                Just token_ ->
                    [ Http.header "Authorization" ("Bearer " ++ token_)
                    ]

                Nothing ->
                    []
        , body = body
        , expect = Http.expectJson (decoder decoder_)
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.toTask
        |> Task.mapError convertError
        |> Task.map (\{ data } -> data)


get : String -> Decoder a -> Connection -> Task Error a
get =
    request Get Http.emptyBody


post : Value -> String -> Decoder a -> Connection -> Task Error a
post bodyValue =
    request Post (Http.jsonBody bodyValue)
