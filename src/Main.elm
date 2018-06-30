module Main exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Jangle exposing (Connection, User)
import Jangle.Auth
import Pages.SignIn as SignIn
import Pages.SignUp as SignUp
import Task exposing (Task, attempt)
import Types exposing (AppMsg(..), Context)
import Url exposing (Url)
import Url.Parser as Url exposing ((</>), Parser, int, map, oneOf, parse, s, top)
import Utils



-- Routes


type Route
    = SignIn
    | SignUp
    | Dashboard


route : Parser (Route -> a) a
route =
    oneOf
        [ map Dashboard top
        , map SignIn (s "sign-in")
        , map SignUp (s "sign-up")
        ]


toRoute : Url -> Maybe Route
toRoute url =
    parse route url



-- Model


type alias Model =
    { context : Context
    , initialUrl : Url
    , key : Nav.Key
    , page : PageStatus
    }


type PageStatus
    = Loading
    | Loaded Page


type Page
    = SignInPage SignIn.Model
    | SignUpPage SignUp.Model
    | Protected User ProtectedPage
    | NotFoundPage


type ProtectedPage
    = DashboardPage


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
        url
        key
        (Loaded <| toPage (Jangle.user connection) url)
    , Cmd.none
    )


type Msg
    = UrlChange Url
    | UrlRequest UrlRequest
    | LoadPage Page
    | AppMessage AppMsg
    | PageMessage PageMsg


type PageMsg
    = SignInMsg SignIn.Msg
    | SignUpMsg SignUp.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LoadPage page ->
            ( { model | page = Loaded page }
            , Cmd.none
            )

        UrlChange url ->
            urlChange url model

        UrlRequest request ->
            urlRequest request model

        AppMessage appMsg ->
            updateFromAppMessage appMsg model

        PageMessage pageMsg ->
            updateFromPageMessage pageMsg model


view : Model -> Document Msg
view model =
    { title = "Jangle"
    , body =
        [ div [ class "app" ]
            [ div
                [ class "page"
                , classList
                    [ ( "page--loading"
                      , model.page == Loading
                      )
                    ]
                ]
                [ case model.page of
                    Loading ->
                        text ""

                    Loaded somePage ->
                        viewPage somePage
                ]
            ]
        ]
    }


toPage : Maybe User -> Url -> Page
toPage maybeUser =
    toRoute
        >> Maybe.map (getPage maybeUser)
        >> Maybe.withDefault NotFoundPage


loadPage : Page -> Model -> ( Model, Cmd Msg )
loadPage page model =
    ( { model | page = Loading }
    , Utils.delay 300 (LoadPage page)
    )


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
        ( SignInPage model_, SignInMsg msg_ ) ->
            signInUpdate msg_ model_ model

        ( SignInPage model_, _ ) ->
            ( model, Cmd.none )

        ( SignUpPage model_, SignUpMsg msg_ ) ->
            signUpUpdate msg_ model_ model

        ( SignUpPage model_, _ ) ->
            ( model, Cmd.none )

        ( Protected user protectedPage, _ ) ->
            ( model, Cmd.none )

        ( NotFoundPage, _ ) ->
            ( model, Cmd.none )


signInUpdate : SignIn.Msg -> SignIn.Model -> Model -> ( Model, Cmd Msg )
signInUpdate msg pageModel model =
    let
        ( updatedPageModel, pageCmd, maybeUser ) =
            SignIn.update model.context msg pageModel
    in
    ( { model
        | page = Loaded (SignInPage updatedPageModel)
        , context =
            setConnection
                (case maybeUser of
                    Just user ->
                        Jangle.authenticate user model.context.connection

                    Nothing ->
                        model.context.connection
                )
                model.context
      }
    , Cmd.batch
        [ pageCmd
            |> Cmd.map SignInMsg
            |> Cmd.map PageMessage
        ]
    )


signUpUpdate : SignUp.Msg -> SignUp.Model -> Model -> ( Model, Cmd Msg )
signUpUpdate msg pageModel model =
    let
        ( updatedPageModel, pageCmd, maybeUser ) =
            SignUp.update model.context msg pageModel
    in
    ( { model
        | page = Loaded (SignUpPage updatedPageModel)
        , context =
            setConnection
                (case maybeUser of
                    Just user ->
                        Jangle.authenticate user model.context.connection

                    Nothing ->
                        model.context.connection
                )
                model.context
      }
    , Cmd.batch
        [ pageCmd
            |> Cmd.map SignUpMsg
            |> Cmd.map PageMessage
        ]
    )


getPage : Maybe User -> Route -> Page
getPage maybeUser route_ =
    case route_ of
        SignIn ->
            SignInPage SignIn.init

        SignUp ->
            SignUpPage SignUp.init

        Dashboard ->
            case maybeUser of
                Just user ->
                    Protected user DashboardPage

                Nothing ->
                    SignInPage SignIn.init


urlChange : Url -> Model -> ( Model, Cmd Msg )
urlChange url model =
    case toRoute url of
        Just route_ ->
            loadPage (getPage (Jangle.user model.context.connection) route_) model

        Nothing ->
            loadPage NotFoundPage model


urlRequest : UrlRequest -> Model -> ( Model, Cmd Msg )
urlRequest request model =
    case request of
        Internal url ->
            ( model
            , Nav.pushUrl model.key (Url.toString url)
            )

        External url ->
            ( model
            , Nav.load url
            )


viewPage : Page -> Html Msg
viewPage page =
    case page of
        SignInPage model ->
            SignIn.view model
                |> Html.map SignInMsg
                |> Html.map PageMessage

        SignUpPage model ->
            SignUp.view model
                |> Html.map SignUpMsg
                |> Html.map PageMessage

        Protected user protectedPage ->
            case protectedPage of
                DashboardPage ->
                    text <| "Dashboard for: " ++ user.name

        NotFoundPage ->
            text "Not found."
