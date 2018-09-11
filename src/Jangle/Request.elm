module Jangle.Request exposing
    ( deleteAs
    , get
    , getAs
    , post
    , postAs
    , putAs
    )

import Http exposing (Body)
import Jangle.Connection exposing (Connection, url)
import Jangle.User exposing (User)
import Json.Decode as Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Task exposing (Task)


type alias Error =
    String


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
    case error of
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


type Method
    = Get
    | Post
    | Put
    | Patch
    | Delete


methodToString : Method -> String
methodToString method =
    case method of
        Get ->
            "GET"

        Post ->
            "POST"

        Put ->
            "PUT"

        Patch ->
            "PATCH"

        Delete ->
            "DELETE"



-- Requests


request : Maybe User -> Method -> Body -> String -> Decoder a -> Connection -> Task Error a
request someUser method body path decoder_ connection =
    Http.request
        { url = url connection path
        , method = methodToString method
        , headers =
            case someUser of
                Just user ->
                    [ Http.header "Authorization" ("Bearer " ++ user.token)
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


anonymousRequest : Method -> Body -> String -> Decoder a -> Connection -> Task Error a
anonymousRequest =
    request Nothing


requestAs : User -> Method -> Body -> String -> Decoder a -> Connection -> Task Error a
requestAs user =
    request (Just user)


get : String -> Decoder a -> Connection -> Task Error a
get =
    anonymousRequest Get Http.emptyBody


getAs : User -> String -> Decoder a -> Connection -> Task Error a
getAs user =
    requestAs user Get Http.emptyBody


post : Value -> String -> Decoder a -> Connection -> Task Error a
post bodyValue =
    anonymousRequest Post (Http.jsonBody bodyValue)


postAs : User -> Value -> String -> Decoder a -> Connection -> Task Error a
postAs user bodyValue =
    requestAs user Post (Http.jsonBody bodyValue)


putAs : User -> Value -> String -> Decoder a -> Connection -> Task Error a
putAs user bodyValue =
    requestAs user Put (Http.jsonBody bodyValue)


deleteAs : User -> String -> Decoder a -> Connection -> Task Error a
deleteAs user =
    requestAs user Delete Http.emptyBody
