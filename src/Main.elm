module Main exposing (..)

import Browser exposing (Env)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Model =
    { canSignUp : Bool
    , name : String
    , email : String
    , password : String
    }


type Msg
    = Attempt Action
    | Complete Action
    | Update Field String


type Action
    = SignIn
    | SignUp


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.fullscreen
        { init = init
        , update = update
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


init : Env Flags -> ( Model, Cmd Msg )
init env =
    ( Model True "" "" ""
    , Cmd.none
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Attempt SignIn ->
            ( Debug.log "Sign in" model
            , Cmd.none
            )

        Complete SignIn ->
            ( Debug.log "Sign in complete" model
            , Cmd.none
            )

        Attempt SignUp ->
            ( Debug.log "Sign up" model
            , Cmd.none
            )

        Complete SignUp ->
            ( Debug.log "Sign up complete" model
            , Cmd.none
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
            )


signInPage : Model -> Html Msg
signInPage model =
    div [ class "app" ]
        [ div
            [ class "container container--small" ]
            [ logo
            , Html.form
                [ class "field__group"
                , novalidate True
                , onSubmit
                    (Attempt
                        (if model.canSignUp then
                            SignUp

                         else
                            SignIn
                        )
                    )
                ]
                [ fields model
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


fields : Model -> Html Msg
fields model =
    section [ class "field__group" ]
        [ div [ class "field__fields" ]
            [ if model.canSignUp then
                field "Name" "text" model.name Name Autofocus

              else
                text ""
            , field "Email Address"
                "email"
                model.email
                Email
                (if model.canSignUp then
                    None

                 else
                    Autofocus
                )
            , field "Password" "password" model.password Password None
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
