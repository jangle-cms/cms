module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)


type alias Model =
    { canSignUp : Bool
    }


type Msg
    = NoOp


main : Program () Model Msg
main =
    Browser.fullscreen
        { init = \env -> ( Model False, Cmd.none )
        , update = \msg model -> ( model, Cmd.none )
        , subscriptions = always Sub.none
        , onNavigation = Nothing
        , view =
            \model ->
                { title = "Jangle"
                , body =
                    [ signInPage model
                    ]
                }
        }


signInPage : Model -> Html Msg
signInPage model =
    div [ class "app" ]
        [ div
            [ class "container container--small" ]
            [ logo
            , Html.form
                [ class "field__group"
                , novalidate True
                ]
                [ fields
                , buttons model
                ]
            ]
        ]


logo : Html Msg
logo =
    div []
        [ span [ class "logo" ]
            [ h1 [ class "logo__title" ]
                [ text "Jangle" ]
            , h2 [ class "logo__subtitle" ]
                [ text "a cms for humans." ]
            ]
        ]


fields : Html Msg
fields =
    section [ class "field__group" ]
        [ div [ class "field__fields" ]
            [ label [ class "field" ]
                [ span [ class "field__label" ] [ text "Email Address" ]
                , input [ class "field__input", type_ "email", autofocus True ] []
                ]
            , label [ class "field" ]
                [ span [ class "field__label" ] [ text "Password" ]
                , input [ class "field__input", type_ "password" ] []
                ]
            ]
        ]


buttons : { a | canSignUp : Bool } -> Html Msg
buttons { canSignUp } =
    div [ class "button__row button__row--right" ]
        [ if canSignUp then
            button
                [ class "button button--green" ]
                [ text "Sign up" ]

          else
            button
                [ class "button button--coral" ]
                [ text "Sign in" ]
        ]
