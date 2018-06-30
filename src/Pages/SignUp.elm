module Pages.SignUp exposing (Model, Msg, init, update, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Jangle exposing (Connection)
import Jangle.Auth exposing (User)
import Process
import Task exposing (andThen, attempt)
import Types exposing (AppMsg(..), Context)
import Utils


type alias Model =
    { user : RemoteData User
    , name : String
    , email : String
    , password : String
    }


type RemoteData a
    = Ready
    | Fetching
    | Success a
    | Error String


type Msg
    = Attempt
    | Handle (Result String User)
    | Update Field String


init : Model
init =
    Model
        Ready
        ""
        ""
        ""


dontUpdate : Model -> ( Model, Cmd Msg, Maybe User )
dontUpdate model =
    ( model, Cmd.none, Nothing )


update : { a | connection : Connection } -> Msg -> Model -> ( Model, Cmd Msg, Maybe User )
update { connection } msg model =
    case msg of
        Attempt ->
            ( model
            , case model.user of
                Ready ->
                    attemptSignUp model connection

                Fetching ->
                    Cmd.none

                Error _ ->
                    attemptSignUp model connection

                Success _ ->
                    Cmd.none
            , Nothing
            )

        Handle (Ok user) ->
            ( { model | user = Success user }
            , Cmd.none
            , Just user
            )

        Handle (Err reason) ->
            ( { model | user = Error reason }
            , Cmd.none
            , Nothing
            )

        Update field_ newValue ->
            ( case field_ of
                Name ->
                    { model | name = newValue }

                Email ->
                    { model | email = newValue }

                Password ->
                    { model | password = newValue }
            , Cmd.none
            , Nothing
            )


attemptSignUp : { a | name : String, email : String, password : String } -> Connection -> Cmd Msg
attemptSignUp { name, email, password } connection =
    Jangle.Auth.signUp { name = name, email = email, password = password } connection
        |> Utils.delayTask 300
        |> attempt Handle


view : Model -> Html Msg
view model =
    div []
        [ div
            [ class "container container--small" ]
            [ logo model.user
            , Html.form
                [ class "field__group"
                , novalidate True
                , onSubmit Attempt
                ]
              <|
                case model.user of
                    Success _ ->
                        []

                    _ ->
                        [ fields model
                        , buttons model
                        ]
            ]
        ]


logo : RemoteData User -> Html Msg
logo data =
    div []
        [ span [ class "logo" ]
            [ h1 [ class "logo__title" ]
                [ text "Jangle" ]
            , h2 [ class "logo__subtitle" ]
                [ case data of
                    Success user ->
                        text <| "Hey there, " ++ user.name ++ "."

                    Error reason ->
                        span [ style "color" "#f74" ] [ text reason ]

                    _ ->
                        text "a cms for humans."
                ]
            ]
        ]


fields : Model -> Html Msg
fields model =
    section [ class "field__group" ]
        [ div [ class "field__fields" ]
            [ field
                "Name"
                "text"
                model.name
                Name
                Autofocus
            , field
                "Email Address"
                "email"
                model.email
                Email
                None
            , field
                "Password"
                "password"
                model.password
                Password
                None
            ]
        ]


type Field
    = Name
    | Email
    | Password


type FieldOptions
    = Autofocus
    | None


field : String -> String -> String -> Field -> FieldOptions -> Html Msg
field label_ type__ value_ field_ fieldOptions =
    label [ class "field" ]
        [ span [ class "field__label" ] [ text label_ ]
        , input
            ([ class "field__input"
             , type_ type__
             , onInput (Update field_)
             , value value_
             ]
                ++ (case fieldOptions of
                        Autofocus ->
                            [ autofocus True ]

                        None ->
                            []
                   )
            )
            []
        ]


buttons : Model -> Html Msg
buttons model =
    div [ class "button__row button__row--right" ]
        [ jangleButton
            (buttonState model.user)
            Green
            "Sign up"
        ]


buttonState : RemoteData User -> ButtonState
buttonState data =
    if data == Fetching then
        Loading

    else
        Normal


type ButtonState
    = Normal
    | Loading


type ButtonColor
    = Coral
    | Green


jangleButton : ButtonState -> ButtonColor -> String -> Html Msg
jangleButton state color label_ =
    button
        [ classList
            [ ( "button", True )
            , ( case color of
                    Coral ->
                        "button--coral"

                    Green ->
                        "button--green"
              , True
              )
            , ( case state of
                    Normal ->
                        "button--normal"

                    Loading ->
                        "button--loading"
              , True
              )
            ]
        ]
        [ text label_ ]
