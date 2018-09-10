module Pages.ListItem exposing
    ( Model
    , Msg
    , init
    , update
    , view
    )

import Dict exposing (Dict)
import Global
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Jangle.Connection exposing (Connection)
import Jangle.List exposing (JangleList)
import Jangle.List.Field exposing (Field)
import Jangle.List.Item exposing (Item)
import Jangle.List.Schema exposing (Schema)
import Jangle.User exposing (User)
import Task
import Utils


type alias Model =
    { slug : String
    , state : State
    , list : JangleList
    , schema : RemoteData Schema
    , item : RemoteData Item
    }


type RemoteData a
    = Fetching
    | Success a
    | Failure String


type State
    = Creating
    | Updating String


type Msg
    = HandleListSchema (Result String Schema)
    | HandleListItem (Result String Item)
    | UpdateStringField Item Field String
    | SaveItem
    | SignOut



-- INIT


init : String -> String -> User -> Connection -> ( Model, Cmd Msg )
init slug id user connection =
    let
        state : State
        state =
            if id == "new" then
                Creating

            else
                Updating id

        list : JangleList
        list =
            Jangle.List.init slug user connection
    in
    ( Model
        slug
        state
        list
        Fetching
        (if state == Creating then
            Success Dict.empty

         else
            Fetching
        )
    , Cmd.batch <|
        [ getListSchema list
        ]
            ++ (if state == Creating then
                    []

                else
                    [ getListItem id list ]
               )
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

        HandleListItem (Ok item) ->
            ( { model | item = Success item }
            , Cmd.none
            , Cmd.none
            )

        HandleListItem (Err reason) ->
            ( { model | item = Failure reason }
            , Cmd.none
            , Cmd.none
            )

        UpdateStringField item field value ->
            ( { model | item = Success (updateField field value item) }
            , Cmd.none
            , Cmd.none
            )

        SaveItem ->
            ( model
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
            Just singular ->
                "New " ++ singular ++ " | Lists | Jangle"

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
            [ h1 [ class "heading__title" ]
                [ text (listName model |> Maybe.withDefault "Item" |> (\val -> "New " ++ val)) ]
            ]
        , case ( model.schema, model.item ) of
            ( Fetching, _ ) ->
                text ""

            ( _, Fetching ) ->
                text ""

            ( Failure reason, Failure otherReason ) ->
                div []
                    [ p [] [ text reason ]
                    , p [] [ text otherReason ]
                    ]

            ( Failure reason, _ ) ->
                text reason

            ( _, Failure reason ) ->
                text reason

            ( Success schema, Success item ) ->
                viewForm model.slug item schema
        ]


viewForm : String -> Item -> Schema -> Html Msg
viewForm slug item schema =
    Html.form
        [ class "field__group"
        , novalidate True
        , onSubmit SaveItem
        ]
        [ viewFields item schema.fields
        , viewButtons slug
        ]


viewFields : Item -> List Field -> Html Msg
viewFields item fields =
    section [ class "field__group" ]
        [ div [ class "field__fields" ]
            (List.map (viewField item) fields)
        ]


viewField : Item -> Field -> Html Msg
viewField item field =
    label [ class "field" ]
        [ span [ class "field__label" ] [ text field.label ]
        , input
            [ class "field__input"
            , type_ "text"
            , onInput (UpdateStringField item field)
            , value (valueOf field item)
            ]
            []
        ]


viewButtons : String -> Html Msg
viewButtons slug =
    div [ class "button__row button__row--right" ]
        [ button [ class "button button--coral" ]
            [ text "Save" ]
        , a [ href ("/lists/" ++ slug), class "button" ] [ text "Cancel" ]
        ]



-- UTILS


valueOf : Field -> Item -> String
valueOf field item =
    Dict.get field.name item
        |> Maybe.withDefault ""


updateField : Field -> String -> Item -> Item
updateField field value =
    Dict.insert field.name value


getListSchema : JangleList -> Cmd Msg
getListSchema list =
    list
        |> Jangle.List.schema
        |> Task.attempt HandleListSchema


getListItem : String -> JangleList -> Cmd Msg
getListItem id list =
    list
        |> Jangle.List.get id
            { populate = Nothing
            , select = Nothing
            }
        |> Task.attempt HandleListItem


listName : Model -> Maybe String
listName model =
    case model.schema of
        Fetching ->
            Nothing

        Success schema ->
            Just schema.labels.singular

        Failure reason ->
            Nothing
