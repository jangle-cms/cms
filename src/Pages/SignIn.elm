module Pages.SignIn exposing (Model, Msg, init, view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias User =
    { name : String
    , email : String
    , token : String
    }


type alias Model =
    { user : RemoteData User
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


view : Model -> Html Msg
view model =
    div [ class "app" ]
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
                "Email Address"
                "email"
                model.email
                Email
                Autofocus
            , field "Password" "password" model.password Password None
            ]
        ]


type Field
    = Email
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
            Coral
            "Sign in"
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
