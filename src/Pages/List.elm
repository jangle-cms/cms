module Pages.List exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Dict
import Global
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Jangle.Connection exposing (Connection)
import Jangle.List exposing (JangleList)
import Jangle.List.Item as Item exposing (Item)
import Jangle.List.ItemList exposing (ItemList)
import Jangle.List.Schema exposing (Schema)
import Jangle.User exposing (User)
import Task
import Time exposing (Zone)
import Utils


type alias Model =
    { slug : String
    , list : JangleList
    , timezone : Maybe Zone
    , schema : RemoteData Schema
    , itemList : RemoteData ItemList
    }


type RemoteData a
    = Fetching
    | Success a
    | Failure String


type Msg
    = HandleListSchema (Result String Schema)
    | HandleListItemList (Result String ItemList)
    | SetTimezone Zone
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
        Nothing
        Fetching
        Fetching
    , Cmd.batch
        [ getListSchema list
        , findListItems list
        , getTimezone
        ]
    )


getTimezone : Cmd Msg
getTimezone =
    Time.here |> Task.perform SetTimezone



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

        HandleListItemList (Ok itemList) ->
            ( { model | itemList = Success itemList }
            , Cmd.none
            , Cmd.none
            )

        HandleListItemList (Err reason) ->
            ( { model | itemList = Failure reason }
            , Cmd.none
            , Cmd.none
            )

        SetTimezone timezone ->
            ( { model | timezone = Just timezone }
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
                , class "heading__cta button button--green button--small"
                ]
                [ text "Create new" ]
            ]
        , case model.itemList of
            Fetching ->
                text ""

            Failure reason ->
                text reason

            Success itemList ->
                itemList.items
                    |> List.map
                        (itemInfoListing
                            (case model.timezone of
                                Just zone ->
                                    zone

                                Nothing ->
                                    Time.utc
                            )
                            model.slug
                        )
                    |> div [ class "listing" ]
        ]


itemInfoListing : Zone -> String -> Item -> Html Msg
itemInfoListing timezone slug item =
    let
        url =
            "/lists/" ++ slug ++ "/" ++ item.id

        updatedTime : Maybe String
        updatedTime =
            Utils.updatedTimeLabel timezone item
    in
    a [ class "listing__item", href url ]
        [ h3 [ class "listing__title" ] [ text item.name ]
        , case updatedTime of
            Just time ->
                p [ class "listing__subtitle" ] [ text time ]

            Nothing ->
                text ""
        ]



-- UTILS


getListSchema : JangleList -> Cmd Msg
getListSchema list =
    list
        |> Jangle.List.schema
        |> Task.attempt HandleListSchema


findListItems : JangleList -> Cmd Msg
findListItems list =
    list
        |> Jangle.List.find
            { where_ = Nothing
            , skip = Nothing
            , limit = Nothing
            , populate = Nothing
            , select = Just [ "name", "jangle" ]
            , sort = Just "-jangle.updated.at"
            }
        |> Task.attempt HandleListItemList


listName : Model -> Maybe String
listName model =
    case model.schema of
        Fetching ->
            Nothing

        Success schema ->
            Just schema.labels.plural

        Failure reason ->
            Nothing
