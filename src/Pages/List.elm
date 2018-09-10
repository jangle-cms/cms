module Pages.List exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Global
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Jangle.Connection exposing (Connection)
import Jangle.List exposing (JangleList)
import Jangle.List.Item exposing (Item)
import Jangle.List.Schema exposing (Schema)
import Jangle.User exposing (User)
import Task
import Utils


type alias Model =
    { slug : String
    , list : JangleList
    , schema : RemoteData Schema
    , item : RemoteData Item
    }


type RemoteData a
    = Fetching
    | Success a
    | Failure String


type Msg
    = HandleListSchema (Result String Schema)
    | HandleListItems (Result String Item)
    | SignOut



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

        HandleListItems (Ok item) ->
            ( { model | item = Success item }
            , Cmd.none
            , Cmd.none
            )

        HandleListItems (Err reason) ->
            ( { model | item = Failure reason }
            , Cmd.none
            , Cmd.none
            )

        SignOut ->
            ( model
            , Cmd.none
            , Utils.perform Global.SignOut
            )



-- VIEW


view : Model -> { title : String, body : List (Html Msg) }
view model =
    { title =
        case listName model of
            Just name ->
                name ++ " | Lists | Jangle"

            Nothing ->
                "Lists | Jangle"
    , body =
        [ page model ]
    }


page : Model -> Html Msg
page model =
    div [ class "page page--padded" ]
        [ navbar model
        , content model
        ]


navbar : Model -> Html Msg
navbar model =
    header [ class "navbar" ]
        [ div
            [ class "navbar__container container" ]
            [ a [ class "navbar__brand", href "/" ] [ text "Jangle" ]
            , button [ class "button button--small", onClick SignOut ] [ text "Sign out" ]
            ]
        ]


content : Model -> Html Msg
content model =
    div [ class "container" ]
        [ div [ class "heading" ]
            [ h1 [ class "heading__title" ] [ text (listName model |> Maybe.withDefault model.slug) ]
            , a
                [ href ("/lists/" ++ model.slug ++ "/new")
                , class "button button--green button--small"
                ]
                [ text "Create new" ]
            ]
        , case model.item of
            Fetching ->
                text ""

            Failure reason ->
                text reason

            Success item ->
                item.items
                    |> List.map (itemInfoListing model.slug)
                    |> div [ class "listing" ]
        ]


itemInfoListing : String -> List ( String, String ) -> Html Msg
itemInfoListing slug item =
    let
        name =
            "Item"

        id =
            "12345"
    in
    a [ class "listing__item", href ("/lists/" ++ slug) ]
        [ h3 [ class "listing__title" ] [ text name ]
        , p [ class "listing__subtitle" ] [ text ("/lists/" ++ slug ++ id) ]
        ]



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
