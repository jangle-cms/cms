module Pages.Welcome exposing (Model, Msg, init, update, view)

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
    { canSignUp : Bool
    , state : State
    , name : String
    , email : String
    , password : String
    }


type State
    = Ready
    | SigningUp
    | SigningIn
    | Error String
    | Success User


type Msg
    = Attempt Action
    | Handle Action (Result String User)
    | Update Field String


type Action
    = SignUp
    | SignIn


init : Bool -> Model
init canSignUp =
    Model
        canSignUp
        Ready
        ""
        ""
        ""


update : { a | connection : Connection } -> Msg -> Model -> ( Model, Cmd Msg, Cmd AppMsg )
update { connection } msg model =
    case msg of
        Attempt action ->
            case model.state of
                Ready ->
                    attemptAction connection action model

                Error _ ->
                    attemptAction connection action model

                SigningUp ->
                    ( model, Cmd.none, Cmd.none )

                Success _ ->
                    ( model, Cmd.none, Cmd.none )

                SigningIn ->
                    ( model, Cmd.none, Cmd.none )

        Handle action (Ok user) ->
            ( { model | state = Success user }
            , Cmd.none
            , Cmd.none
              -- Utils.send (SetConnection newConnection)
            )

        Handle _ (Err reason) ->
            ( { model | state = Error reason }
            , Cmd.none
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
            , Cmd.none
            )


attemptAction : Connection -> Action -> Model -> ( Model, Cmd Msg, Cmd AppMsg )
attemptAction connection action model =
    case action of
        SignUp ->
            ( { model | state = SigningUp }
            , attemptSignUp model connection
            , Cmd.none
            )

        SignIn ->
            ( { model | state = SigningIn }
            , attemptSignIn model connection
            , Cmd.none
            )


attemptSignUp : { a | name : String, email : String, password : String } -> Connection -> Cmd Msg
attemptSignUp { name, email, password } connection =
    Jangle.Auth.signUp { name = name, email = email, password = password } connection
        |> Utils.delayTask 300
        |> attempt (Handle SignUp)


attemptSignIn : { a | email : String, password : String } -> Connection -> Cmd Msg
attemptSignIn { email, password } connection =
    Jangle.Auth.signIn { email = email, password = password } connection
        |> Utils.delayTask 300
        |> attempt (Handle SignIn)


view : Model -> Html Msg
view model =
    div [ class "app" ]
        [ div
            [ class "container container--small" ]
            [ logo model
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
                (case model.state of
                    Success user ->
                        []

                    _ ->
                        [ fields model
                        , buttons model
                        ]
                )
            ]
        ]


logo : Model -> Html Msg
logo model =
    div []
        [ span [ class "logo" ]
            [ h1 [ class "logo__title" ]
                [ text "Jangle" ]
            , h2 [ class "logo__subtitle" ]
                [ case model.state of
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


buttons : Model -> Html Msg
buttons model =
    div [ class "button__row button__row--right" ]
        [ if model.canSignUp then
            button
                [ class "button button--green" ]
                [ text "Sign up" ]

          else
            jangleButton
                (buttonState SigningIn model.state)
                Coral
                "Sign in"
        ]


type ButtonState
    = Normal
    | Loading


buttonState : State -> State -> ButtonState
buttonState activeState desiredState =
    if activeState == desiredState then
        Loading

    else
        Normal


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
