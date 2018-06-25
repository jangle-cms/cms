module Main exposing (..)

import Browser exposing (Document, UrlRequest)
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Jangle exposing (Connection)
import Jangle.Auth
import Pages.Welcome as Welcome
import Task exposing (Task, attempt)
import Types exposing (AppMsg(..), Context)
import Url exposing (Url)
import Url.Parser as Url


type alias Model =
    { context : Context
    , key : Nav.Key
    , page : Status
    }


type Status
    = Loading
    | Loaded Page


type Page
    = Welcome Welcome.Model
    | NotFound


type alias Flags =
    ()


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = UrlChange
        , onUrlRequest = UrlRequest
        , view = view
        }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        connection : Jangle.Connection
        connection =
            Jangle.connect "http://localhost:3000/api"
    in
    ( Model
        (Context connection)
        key
        Loading
    , checkForUser connection
    )


checkForUser : Connection -> Cmd Msg
checkForUser connection =
    Jangle.Auth.canSignUp connection
        |> attempt HandleCanSignUp


type Msg
    = HandleCanSignUp (Result String Bool)
    | UrlChange Url
    | UrlRequest UrlRequest
    | AppMessage AppMsg
    | PageMessage PageMsg


type PageMsg
    = WelcomeMsg Welcome.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlChange url ->
            urlChange url model

        UrlRequest request ->
            urlRequest request model

        HandleCanSignUp (Ok canSignUp) ->
            ( { model
                | page =
                    Loaded <|
                        Welcome (Welcome.init canSignUp)
              }
            , Cmd.none
            )

        HandleCanSignUp (Err reason) ->
            let
                _ =
                    Debug.todo "Present error to user" reason
            in
            ( model
            , Cmd.none
            )

        AppMessage appMsg ->
            updateFromAppMessage appMsg model

        PageMessage pageMsg ->
            updateFromPageMessage pageMsg model


updateFromAppMessage : AppMsg -> Model -> ( Model, Cmd Msg )
updateFromAppMessage appMsg model =
    case appMsg of
        SetConnection conn ->
            ( { model | context = setConnection conn model.context }
            , Cmd.none
            )


setConnection : Connection -> Context -> Context
setConnection connection context =
    { context | connection = connection }


updateFromPageMessage : PageMsg -> Model -> ( Model, Cmd Msg )
updateFromPageMessage pageMsg model =
    case ( model.page, pageMsg ) of
        ( Loading, _ ) ->
            ( model, Cmd.none )

        ( Loaded page, _ ) ->
            updateFromPage page pageMsg model


updateFromPage : Page -> PageMsg -> Model -> ( Model, Cmd Msg )
updateFromPage page msg model =
    case ( page, msg ) of
        ( Welcome model_, WelcomeMsg msg_ ) ->
            welcomeUpdate msg_ model_ model

        ( NotFound, _ ) ->
            ( model, Cmd.none )


welcomeUpdate : Welcome.Msg -> Welcome.Model -> Model -> ( Model, Cmd Msg )
welcomeUpdate msg pageModel model =
    let
        ( model_, cmd_, appCmd ) =
            Welcome.update model.context msg pageModel
    in
    ( { model
        | page = Loaded (Welcome model_)
      }
    , Cmd.batch
        [ cmd_
            |> Cmd.map WelcomeMsg
            |> Cmd.map PageMessage
        , appCmd
            |> Cmd.map AppMessage
        ]
    )


urlChange : Url -> Model -> ( Model, Cmd Msg )
urlChange url model =
    ( model, Cmd.none )


urlRequest : UrlRequest -> Model -> ( Model, Cmd Msg )
urlRequest request model =
    ( model, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Jangle"
    , body =
        [ case model.page of
            Loading ->
                text ""

            Loaded (Welcome model_) ->
                Welcome.view model_
                    |> Html.map WelcomeMsg
                    |> Html.map PageMessage

            Loaded NotFound ->
                text "Not found."
        ]
    }
