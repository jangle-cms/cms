module Jangle exposing (Connection, authenticate, connect, get, post)

import Http
import Jangle.User as User exposing (User)
import Json.Decode as Decode exposing (Decoder)
import Json.Decoder.Pipeline exposing (optional, required)
import Json.Encode as Encode exposing (Value)
import Task



-- Connections


type Connection
    = Connection String (Maybe User)


connect : String -> Connection
connect baseUrl =
    Connection baseUrl Nothing


authenticate : User -> Connection -> Connection
authenticate user (Connection baseUrl _) =
    Connection baseUrl (Just user)


url : Connection -> String -> String
url (Connection baseUrl _) path =
    baseUrl ++ path


token : Connection -> Maybe String
token (Connection _ user) =
    case user of
        Just { token } ->
            Just token

        Nothing ->
            Nothing



-- GET and POST


type alias Error =
    String


type alias Response a =
    { error : Bool
    , message : String
    , data : a
    }


type alias EmptyReponse =
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
        BadUrl _ ->
            "Invalid URL."

        Timeout ->
            "Request timed out."

        NetworkError ->
            "Cannot connect to network."

        BadStatus { body } ->
            case Decode.decodeString emptyDecoder body of
                Ok { message } ->
                    message

                Err err ->
                    let
                        _ =
                            Decode.errorToString err
                    in
                    "There was a bad status code."

        BadPayload reason _ ->
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


request : Method -> Body -> String -> Decoder a -> Connection -> Task Error a
request method body path decoder connection =
    Http.request
        { url = url connection path
        , method = methodToString method
        , headers =
            case token connection of
                Just token_ ->
                    [ Http.header "Authorization" ("Bearer " ++ token)
                    ]

                Nothing ->
                    []
        , body = body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = False
        }
        |> Http.toTask
        |> Task.mapError convertError


get : String -> Decoder a -> Connection -> Task Error a
get =
    request Get Http.emptyBody


post : Value -> String -> Decoder a -> Connection -> Task Error a
post bodyValue =
    request Post (Http.jsonBody bodyValue)
