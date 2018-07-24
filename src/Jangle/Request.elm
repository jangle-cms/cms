module Jangle.Request
    exposing
        ( get
        , getAs
        , post
        , postAs
        )

import Http exposing (Body)
import Jangle.Connection exposing (Connection, url)
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


type alias Token =
    String


type alias ApiPath =
    String


request : Method -> Body -> Maybe Token -> ApiPath -> Decoder a -> Connection -> Task Error a
request method body maybeToken path decoder_ connection =
    Http.request
        { url = url connection path
        , method = methodToString method
        , headers =
            case maybeToken of
                Just token ->
                    [ Http.header "Authorization" ("Bearer " ++ token)
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


get_ : Maybe Token -> ApiPath -> Decoder a -> Connection -> Task Error a
get_ =
    request Get Http.emptyBody


post_ : Maybe Token -> Value -> ApiPath -> Decoder a -> Connection -> Task Error a
post_ token bodyValue =
    request Post (Http.jsonBody bodyValue) token



-- GET


get : ApiPath -> Decoder a -> Connection -> Task Error a
get =
    get_ Nothing


getAs : Token -> ApiPath -> Decoder a -> Connection -> Task Error a
getAs token =
    get_ (Just token)



-- POST


post : Value -> ApiPath -> Decoder a -> Connection -> Task Error a
post =
    post_ Nothing


postAs : Token -> Value -> ApiPath -> Decoder a -> Connection -> Task Error a
postAs token =
    post_ (Just token)
