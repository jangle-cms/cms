module Pages.SignIn exposing (Model, Msg, init, update, view)

import Global
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Jangle exposing (Connection)
import Jangle.Auth
import Jangle.User as User exposing (User)
import Task
import Utils


type alias Model =
    { connection : Connection
    , user : UserState
    , error : Maybe String
    , name : String
    , email : String
    , password : String
    }


type UserState
    = CheckingCanSignUp
    | AlreadySignedIn User
    | ShouldSignUp
    | ShouldSignIn
    | SigningUp
    | SigningIn


type Msg
    = CheckCanSignUp
    | AttemptSignUp
    | AttemptSignIn
    | HandleCanSignUp (Result String Bool)
    | HandleSignUp (Result String User)
    | HandleSignIn (Result String User)
    | Update Field String



-- INIT


init : Connection -> Maybe User -> ( Model, Cmd Msg )
init connection possibleUser =
    case possibleUser of
        Just user ->
            ( Model
                connection
                (AlreadySignedIn user)
                Nothing
                ""
                ""
                ""
            , Cmd.none
            )

        Nothing ->
            ( Model
                connection
                CheckingCanSignUp
                Nothing
                ""
                ""
                ""
            , Utils.perform CheckCanSignUp
            )


update : Msg -> Model -> ( Model, Cmd Msg, Cmd Global.Msg )
update msg model =
    case msg of
        CheckCanSignUp ->
            ( model
            , Jangle.Auth.canSignUp model.connection
                |> Task.attempt HandleCanSignUp
            , Cmd.none
            )

        AttemptSignUp ->
            ( { model
                | user = SigningUp
                , error = Nothing
              }
            , Jangle.Auth.signUp
                { name = model.name
                , email = model.email
                , password = model.password
                }
                model.connection
                |> Task.attempt HandleSignUp
            , Cmd.none
            )

        AttemptSignIn ->
            ( { model
                | user = SigningIn
                , error = Nothing
              }
            , Jangle.Auth.signIn
                { email = model.email
                , password = model.password
                }
                model.connection
                |> Task.attempt HandleSignIn
            , Cmd.none
            )

        HandleCanSignUp (Ok canSignUp) ->
            if canSignUp then
                ( { model | user = ShouldSignUp }
                , Cmd.none
                , Cmd.none
                )

            else
                ( { model | user = ShouldSignIn }
                , Cmd.none
                , Cmd.none
                )

        HandleCanSignUp (Err reason) ->
            ( { model | error = Just reason }, Cmd.none, Cmd.none )

        HandleSignUp (Ok user) ->
            ( model, Cmd.none, Utils.perform (Global.SignIn user) )

        HandleSignUp (Err reason) ->
            ( { model | error = Just reason, user = ShouldSignUp }, Cmd.none, Cmd.none )

        HandleSignIn (Ok user) ->
            ( model, Cmd.none, Utils.perform (Global.SignIn user) )

        HandleSignIn (Err reason) ->
            ( { model | error = Just reason, user = ShouldSignIn }, Cmd.none, Cmd.none )

        Update Name name ->
            ( { model | name = name }, Cmd.none, Cmd.none )

        Update Email email ->
            ( { model | email = email }, Cmd.none, Cmd.none )

        Update Password password ->
            ( { model | password = password }, Cmd.none, Cmd.none )



-- VIEW


empty : Html Msg
empty =
    Html.text ""


view : Model -> Html Msg
view model =
    div [ class "page page--centered" ]
        [ div
            [ class "container container--small" ]
            [ logo model.error model.user
            , case model.user of
                CheckingCanSignUp ->
                    empty

                AlreadySignedIn user ->
                    empty

                ShouldSignUp ->
                    signUpForm model

                ShouldSignIn ->
                    signInForm model

                SigningUp ->
                    signUpForm model

                SigningIn ->
                    signInForm model
            ]
        ]


signInForm : Model -> Html Msg
signInForm model =
    Html.form
        [ class "field__group"
        , novalidate True
        , onSubmit AttemptSignIn
        ]
        [ signInFields model
        , signInButtons model
        ]


signUpForm : Model -> Html Msg
signUpForm model =
    Html.form
        [ class "field__group"
        , novalidate True
        , onSubmit AttemptSignUp
        ]
        [ signUpFields model
        , signUpButtons model
        ]


logo : Maybe String -> UserState -> Html Msg
logo error data =
    div []
        [ span [ class "logo" ]
            [ h1 [ class "logo__title" ]
                [ text "Jangle" ]
            , h2 [ class "logo__subtitle" ]
                [ case data of
                    AlreadySignedIn user ->
                        text <| "Hey there, " ++ user.name ++ "."

                    _ ->
                        case error of
                            Just reason ->
                                span [ style "color" "#f74" ] [ text reason ]

                            Nothing ->
                                text "a cms for humans."
                ]
            ]
        ]


signInFields : Model -> Html Msg
signInFields model =
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


signUpFields : Model -> Html Msg
signUpFields model =
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


signInButtons : Model -> Html Msg
signInButtons model =
    div [ class "button__row button__row--right" ]
        [ jangleButton
            (buttonState model.user)
            Coral
            "Sign in"
        ]


signUpButtons : Model -> Html Msg
signUpButtons model =
    div [ class "button__row button__row--right" ]
        [ jangleButton
            (buttonState model.user)
            Green
            "Sign up"
        ]


buttonState : UserState -> ButtonState
buttonState data =
    if data == SigningIn || data == SigningUp then
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
