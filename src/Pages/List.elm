module Pages.List exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Global
import Html exposing (..)
import Jangle.Connection exposing (Connection)
import Jangle.List exposing (JangleList)
import Jangle.List.Item exposing (Item)
import Jangle.List.Schema exposing (Schema)
import Jangle.User exposing (User)
import Task


type alias Model =
    { slug : String
    , list : JangleList
    , schema : RemoteData Schema
    , items : RemoteData Item
    }


type RemoteData a
    = Fetching
    | Success a
    | Failure String


type Msg
    = HandleListSchema (Result String Schema)
    | HandleListItems (Result String Item)



-- INIT


init : String -> User -> Connection -> ( Model, Cmd Msg )
init slug user connection =
    let
        list =
            Jangle.List.init slug user connection
    in
    ( Model
        slug
        list
        Fetching
        Fetching
    , Cmd.batch
        [ getListSchema list
        , getListItems list
        ]
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        HandleListSchema (Ok schema) ->
            ( { model | schema = Success schema }
            , Cmd.none
            , Cmd.none
            )

        HandleListSchema (Err reason) ->
            ( { model | schema = Failure reason }
            , Cmd.none
            , Cmd.none
            )

        HandleListItems (Ok items) ->
            ( { model | items = Success items }
            , Cmd.none
            , Cmd.none
            )

        HandleListItems (Err reason) ->
            ( { model | items = Failure reason }
            , Cmd.none
            , Cmd.none
            )



-- VIEW


view : Model -> { title : String, body : List (Html msg) }
view model =
    { title =
        case listName model of
            Just name ->
                name ++ " | Lists | Jangle"

            Nothing ->
                "Lists | Jangle"
    , body =
        [ h1 [] [ text model.slug ]
        , case model.schema of
            Failure reason ->
                text reason

            _ ->
                text ""
        , case model.items of
            Failure reason ->
                text reason

            _ ->
                text ""
        ]
    }



-- UTILS


getListSchema : JangleList -> Cmd Msg
getListSchema list =
    list
        |> Jangle.List.schema
        |> Task.attempt HandleListSchema


getListItems : JangleList -> Cmd Msg
getListItems list =
    list
        |> Jangle.List.find
            { where_ = Nothing
            , skip = Nothing
            , limit = Nothing
            , populate = Nothing
            , select =
                Just
                    [ "_id"
                    , "name"
                    , "jangle.created"
                    , "jangle.updated"
                    , "jangle.hasWorkingDraft"
                    ]
            , sort = Just "jangle.updated"
            }
        |> Task.attempt HandleListItems


listName : Model -> Maybe String
listName model =
    case model.schema of
        Fetching ->
            Nothing

        Success schema ->
            Just schema.labels.plural

        Failure reason ->
            Nothing
