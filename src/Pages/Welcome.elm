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
    { status : Status
    , name : String
    , email : String
    , password : String
    }


type Status
    = CheckingCanSignUp
    | Can FormType (RemoteData User)
    | CanSignUpError String


type RemoteData a
    = Ready
    | Fetching
    | Success a
    | Error String


type FormType
    = SignUp
    | SignIn


type Msg
    = HandleCanSignUp (Result String Bool)
    | Post FormType (RemoteData User) PostMsg


type PostMsg
    = Attempt
    | Handle (Result String User)
    | Update Field String


init : Connection -> ( Model, Cmd Msg )
init connection =
    ( Model
        CheckingCanSignUp
        ""
        ""
        ""
    , checkCanSignUp connection
    )


checkCanSignUp : Connection -> Cmd Msg
checkCanSignUp connection =
    Jangle.Auth.canSignUp connection
        |> attempt HandleCanSignUp


dontUpdate : Model -> ( Model, Cmd Msg, Maybe User )
dontUpdate model =
    ( model, Cmd.none, Nothing )


update : { a | connection : Connection } -> Msg -> Model -> ( Model, Cmd Msg, Maybe User )
update { connection } msg model =
    case msg of
        HandleCanSignUp (Ok canSignUp) ->
            if canSignUp then
                ( { model | formType = Ready SignUp }
                , Cmd.none
                , Nothing
                )

            else
                ( { model | formType = Ready SignIn }
                , Cmd.none
                , Nothing
                )

        HandleCanSignUp (Err reason) ->
            ( { model | state = Error reason }
            , Cmd.none
            , Nothing
            )

        Post formType userData postMsg ->
            case postMsg of
                Attempt ->
                    case userData of
                        Ready ->
                            attemptAction connection formType model

                        Fetching ->
                            dontUpdate model

                        Error _ ->
                            attemptAction connection formType model

                        Success _ ->
                            dontUpdate model

                Handle (Ok user) ->
                    ( { model | state = Success user }
                    , Cmd.none
                    , Just user
                    )

                Handle (Err reason) ->
                    ( { model | state = Error reason }
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


attemptAction : Connection -> FormType -> Model -> ( Model, Cmd Msg, Maybe User )
attemptAction connection formType model =
    case formType of
        SignUp ->
            ( { model | status = Can formType Fetching }
            , attemptSignUp model connection
            , Nothing
            )

        SignIn ->
            ( { model | status = Can formType Fetching }
            , attemptSignIn model connection
            , Nothing
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
    case model.state of
        CheckingCanSignUp ->
            text ""

        Can formType status ->
            viewForm formType status model


viewForm : FormType -> Status -> Model -> Html Msg
viewForm formType status model =
    div []
        [ div
            [ class "container container--small" ]
            [ logo status
            , Html.form
                [ class "field__group"
                , novalidate True
                , onSubmit (Attempt formType)
                ]
              <|
                case status of
                    Success _ ->
                        []

                    _ ->
                        [ fields model
                        , buttons model
                        ]
            ]
        ]


logo : Status -> Html Msg
logo status =
    div []
        [ span [ class "logo" ]
            [ h1 [ class "logo__title" ]
                [ text "Jangle" ]
            , h2 [ class "logo__subtitle" ]
                [ case status of
                    Success user ->
                        text <| "Hey there, " ++ user.name ++ "."

                    Error reason ->
                        span [ style "color" "#f74" ] [ text reason ]

                    _ ->
                        text "a cms for humans."
                ]
            ]
        ]


fields : FormType -> Model -> Html Msg
fields formType model =
    section [ class "field__group" ]
        [ div [ class "field__fields" ]
            [ case formType of
                SignUp ->
                    field "Name" "text" model.name Name Autofocus

                SignIn ->
                    text ""
            , field "Email Address"
                "email"
                model.email
                Email
                (case formType of
                    SignUp ->
                        None

                    SignIn ->
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
        [ if model.canSignUp then
            jangleButton
                (buttonState SigningUp model.state)
                Green
                "Sign up"

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
