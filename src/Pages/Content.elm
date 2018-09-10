module Pages.Content exposing
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
import Jangle.Lists exposing (ListInfo)
import Jangle.User exposing (User)
import Task
import Utils


type alias Model =
    { user : User
    , connection : Connection
    , lists : RemoteData (List ListInfo)
    }


type RemoteData a
    = Fetching
    | Success a
    | Failure String


type Msg
    = HandleLists (Result String (List ListInfo))
    | SignOut



-- INIT


init : User -> Connection -> ( Model, Cmd Msg )
init user connection =
    ( Model user connection Fetching
    , getAllLists user connection
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        HandleLists (Ok lists) ->
            ( { model | lists = Success lists }
            , Cmd.none
            , Cmd.none
            )

        HandleLists (Err reason) ->
            ( { model | lists = Failure reason }
            , Cmd.none
            , Cmd.none
            )

        SignOut ->
            ( model
            , Cmd.none
            , Utils.perform Global.SignOut
            )


view : Model -> Html Msg
view model =
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
            [ h1 [ class "heading__title" ] [ text "Content" ]
            ]
        , case model.lists of
            Fetching ->
                text ""

            Failure reason ->
                text reason

            Success lists ->
                lists
                    |> List.map listInfoListing
                    |> div [ class "listing" ]
        ]


listInfoListing : ListInfo -> Html Msg
listInfoListing { labels, slug } =
    a [ class "listing__item", href ("/lists/" ++ slug) ]
        [ h3 [ class "listing__title" ] [ text labels.plural ]
        , p [ class "listing__subtitle" ] [ text ("/lists/" ++ slug) ]
        ]



-- UTILS


getAllLists : User -> Connection -> Cmd Msg
getAllLists user connection =
    Jangle.Lists.index user connection
        |> Task.attempt HandleLists
