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
import Jangle.List.Item as Item exposing (Item, ItemField)
import Jangle.List.Schema exposing (Schema)
import Jangle.User exposing (User)
import Json.Decode as Decode
import Json.Encode as Encode
import Task
import Utils


type alias Model =
    { slug : String
    , state : State
    , list : JangleList
    , schema : RemoteData Schema
    , item : RemoteData Item
    , counts : Dict String Int
    , savedItem : SaveData Item
    }


type RemoteData a
    = Fetching
    | Success a
    | Failure String


type SaveData a
    = Ready
    | Saving
    | Saved a
    | FailedToSave String


type State
    = Creating
    | Updating String


type Msg
    = HandleListSchema (Result String Schema)
    | HandleListItem (Result String Item)
    | UpdateStringField Item Field String
    | CreateItem Item
    | SaveItem
    | HandleCreateItem (Result String Item)
    | SignOut
    | IncrementCount Field



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
            Success Item.empty

         else
            Fetching
        )
        Dict.empty
        Ready
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
            ( { model
                | item = Success item
                , counts = initCountsFromItem item
              }
            , Cmd.none
            , Cmd.none
            )

        HandleListItem (Err reason) ->
            ( { model | item = Failure reason }
            , Cmd.none
            , Cmd.none
            )

        UpdateStringField item field value ->
            ( { model | item = Success (updateField field (Item.stringField value) item) }
            , Cmd.none
            , Cmd.none
            )

        CreateItem item ->
            ( { model | savedItem = Saving }
            , Jangle.List.create item model.list
                |> Task.attempt HandleCreateItem
            , Cmd.none
            )

        HandleCreateItem (Ok item) ->
            ( { model
                | savedItem = Saved item
              }
            , Cmd.none
            , Cmd.none
            )

        HandleCreateItem (Err reason) ->
            ( { model | savedItem = FailedToSave reason }
            , Cmd.none
            , Cmd.none
            )

        SaveItem ->
            ( model
            , Cmd.none
            , Cmd.none
            )

        IncrementCount { name } ->
            let
                value =
                    Dict.get name model.counts |> Maybe.withDefault 0
            in
            ( { model
                | counts =
                    Dict.insert name (value + 1) model.counts
              }
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
        [ div
            [ class "heading"
            ]
            [ h1
                [ class "heading__title"
                , classList
                    [ ( "heading__title--ready", listName model /= Nothing )
                    ]
                ]
                [ text
                    (listName model
                        |> Maybe.withDefault "Item"
                        |> (\val -> "New " ++ val)
                    )
                ]
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
                viewForm model item schema
        ]


viewForm : Model -> Item -> Schema -> Html Msg
viewForm model item schema =
    Html.form
        [ class "field__group"
        , novalidate True
        , onSubmit (CreateItem item)
        ]
        [ viewFields model item schema.fields
        , viewButtons model
        , case model.savedItem of
            Ready ->
                text ""

            Saving ->
                text "Saving"

            Saved _ ->
                text "Success!"

            FailedToSave reason ->
                text reason
        ]


viewFields : Model -> Item -> List Field -> Html Msg
viewFields model item fields =
    section [ class "field" ]
        [ div [ class "field__fields" ]
            (List.map (viewField model item) fields)
        ]


viewListOfFields : Model -> Item -> Field -> Html Msg
viewListOfFields model item field =
    div [ class "field" ]
        [ viewLabel field
        , div [ class "field__list" ]
            [ div []
                (List.range 1
                    (Dict.get field.name model.counts |> Maybe.withDefault 0)
                    |> List.map
                        (\index ->
                            if field.type_ == "Object" then
                                viewFields model
                                    item
                                    (Jangle.List.Field.fieldsFrom
                                        field.fields
                                        |> List.map
                                            (\innerField ->
                                                { innerField
                                                    | name =
                                                        field.name
                                                            ++ "["
                                                            ++ String.fromInt (index - 1)
                                                            ++ "]."
                                                            ++ innerField.name
                                                }
                                            )
                                    )

                            else
                                viewField model
                                    item
                                    { field
                                        | isList = False
                                        , label = ""
                                        , name = field.name ++ "[" ++ String.fromInt (index - 1) ++ "]"
                                    }
                        )
                )
            , div [ class "button__row button__row--small" ]
                [ button
                    [ class "button button--small"
                    , onClick (IncrementCount field)
                    ]
                    [ text "Add another" ]
                ]
            ]
        ]


viewField : Model -> Item -> Field -> Html Msg
viewField model item field =
    if field.isList then
        viewListOfFields model item field

    else
        case field.type_ of
            "Object" ->
                div [ class "field" ]
                    [ viewLabel field
                    , div [ class "field__list" ]
                        [ viewFields model
                            item
                            (Jangle.List.Field.fieldsFrom field.fields
                                |> List.map
                                    (\innerField ->
                                        { innerField
                                            | name =
                                                field.name
                                                    ++ "."
                                                    ++ innerField.name
                                        }
                                    )
                            )
                        ]
                    ]

            "String" ->
                label [ class "field" ]
                    [ viewLabel field
                    , input
                        [ class "field__input"
                        , type_ "text"
                        , onInput (UpdateStringField item field)
                        , value (valueOf field item)
                        ]
                        []
                    ]

            "Number" ->
                label [ class "field" ]
                    [ viewLabel field
                    , input
                        [ class "field__input"
                        , type_ "number"
                        , onInput (UpdateStringField item field)
                        , value (valueOf field item)
                        ]
                        []
                    ]

            "ObjectID" ->
                label [ class "field" ]
                    [ viewLabel field
                    , input
                        [ class "field__input"
                        , type_ "text"
                        , onInput (UpdateStringField item field)
                        , value (valueOf field item)
                        , placeholder <| field.ref ++ "..."
                        ]
                        []
                    ]

            "Date" ->
                label [ class "field" ]
                    [ viewLabel field
                    , div
                        [ class "field__select"
                        ]
                        [ select
                            [ class "field__select-month" ]
                            (List.indexedMap viewMonthOption
                                [ "January"
                                , "February"
                                , "March"
                                , "April"
                                , "May"
                                , "June"
                                , "July"
                                , "August"
                                , "September"
                                , "October"
                                , "November"
                                , "December"
                                ]
                            )
                        , select
                            [ class "field__select-day" ]
                            (List.map viewDayOption
                                (List.range 1 31 |> List.map String.fromInt)
                            )
                        , input
                            [ class "field__select-year"
                            , value "2017"
                            , type_ "number"
                            ]
                            []
                        ]
                    ]

            "RichText" ->
                label [ class "field" ]
                    [ viewLabel field
                    , Html.node
                        "tinymce-editor"
                        [ class "field__rich-text"
                        , Html.Attributes.property "editorValue" <|
                            Encode.string (valueOf field item)
                        , Html.Events.on "editorChanged" <|
                            Decode.map (UpdateStringField item field) <|
                                Decode.at [ "target", "editorValue" ] <|
                                    Decode.string
                        ]
                        [ p [] []
                        ]
                    ]

            other ->
                p []
                    [ text <|
                        "Todo: "
                            ++ (if field.isList then
                                    "List of "

                                else
                                    ""
                               )
                            ++ other
                            ++ (if field.isList then
                                    "s"

                                else
                                    ""
                               )
                    ]


viewLabel : Field -> Html Msg
viewLabel field =
    if String.isEmpty field.label then
        text ""

    else
        span [ class "field__label" ]
            [ text field.label
            , span [ class "field__label-note" ]
                [ text
                    (if field.required then
                        ""

                     else
                        "Optional"
                    )
                ]
            ]


viewMonthOption : Int -> String -> Html Msg
viewMonthOption index label =
    option [ value (String.fromInt index) ] [ text label ]


viewDayOption : String -> Html Msg
viewDayOption day =
    option [ value day ] [ text day ]


viewButtons : Model -> Html Msg
viewButtons { state, slug } =
    div [ class "button__row button__row--right", style "margin-bottom" "4rem" ]
        [ case state of
            Creating ->
                button
                    [ class "button button--green" ]
                    [ text "Create" ]

            Updating id ->
                button
                    [ class "button button--coral" ]
                    [ text "Save" ]
        , a [ href ("/lists/" ++ slug), class "button" ] [ text "Cancel" ]
        ]



-- UTILS


initCountsFromItem : Item -> Dict String Int
initCountsFromItem item =
    -- TODO: Use item that comes back to determine number of fields to render.
    Dict.empty


valueOf : Field -> Item -> String
valueOf field item =
    Dict.get field.name item.fields
        |> Maybe.andThen Item.stringFrom
        |> Maybe.withDefault ""


updateField : Field -> ItemField -> Item -> Item
updateField field value item =
    { item | fields = Dict.insert field.name value item.fields }


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
