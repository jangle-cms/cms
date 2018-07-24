module Main exposing (..)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Context
import Html exposing (..)
import Html.Attributes exposing (..)
import Jangle.Auth exposing (User)
import Pages.Dashboard
import Pages.SignIn
import Process
import Task exposing (Task)
import Url exposing (Url)


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequest
        , onUrlChange = OnUrlChange
        }



-- MODEL


type alias Model =
    { url : Url
    , key : Nav.Key
    , context : Context.Model
    , page : Transitionable Page
    }


type Transitionable a
    = TransitioningFrom a
    | Loaded a


type Page
    = Blank
    | SignIn Pages.SignIn.Model
    | Protected User ProtectedPage


type ProtectedPage
    = Dashboard Pages.Dashboard.Model
    | NotFound


type alias Flags =
    { user : Maybe User
    }


init : Flags -> Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    ( Model
        url
        key
        Context.init
        (Loaded (pageFromUrl flags.user url))
    , Cmd.none
    )



-- UPDATE


type Msg
    = OnUrlRequest UrlRequest
    | OnUrlChange Url
    | SetPage Page
    | ContextMsg Context.Msg
    | PageMsg PageMessage


type PageMessage
    = SignInMsg Pages.SignIn.Msg
    | DashboardMsg Pages.Dashboard.Msg


type alias UpdateFunction msg model =
    msg -> model -> ( model, Cmd msg )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnUrlRequest request ->
            case request of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model, Nav.load url )

        OnUrlChange url ->
            ( { model
                | url = url
                , page = TransitioningFrom (currentPage model)
              }
            , setPageLater (pageFromUrl (getUser model) url)
            )

        SetPage page ->
            ( { model | page = Loaded page }
            , Cmd.none
            )

        ContextMsg contextMsg ->
            ( { model | context = Context.update contextMsg model.context }
            , Cmd.none
            )

        PageMsg pageMessage ->
            case model.page of
                TransitioningFrom oldPage ->
                    ( model, Cmd.none )

                Loaded page ->
                    updatePage pageMessage page model


updateMap : (msg -> PageMessage) -> (model -> Page) -> UpdateFunction msg model -> msg -> model -> Model -> ( Model, Cmd Msg )
updateMap toMsg toModel updateFunction msg model fullModel =
    let
        ( newModel, command ) =
            updateFunction msg model
    in
    ( { fullModel | page = Loaded (toModel newModel) }
    , Cmd.map (toMsg >> PageMsg) command
    )


updatePage : PageMessage -> Page -> Model -> ( Model, Cmd Msg )
updatePage pageMsg pageModel fullModel =
    case pageModel of
        Blank ->
            ( fullModel, Cmd.none )

        SignIn model ->
            case pageMsg of
                SignInMsg msg ->
                    updateMap SignInMsg
                        SignIn
                        Pages.SignIn.update
                        msg
                        model
                        fullModel

                _ ->
                    ( fullModel, Cmd.none )

        Protected user page ->
            case ( pageMsg, page ) of
                ( DashboardMsg msg, Dashboard model ) ->
                    updateMap DashboardMsg
                        (Dashboard >> Protected user)
                        Pages.Dashboard.update
                        msg
                        model
                        fullModel

                ( DashboardMsg msg, _ ) ->
                    ( fullModel, Cmd.none )

                ( SignInMsg msg, _ ) ->
                    ( fullModel, Cmd.none )


pageFromUrl : Maybe User -> Url -> Page
pageFromUrl maybeUser url =
    case maybeUser of
        Just user ->
            Protected user <|
                case url.path of
                    "/" ->
                        Dashboard Pages.Dashboard.init

                    _ ->
                        NotFound

        Nothing ->
            SignIn Pages.SignIn.init


currentPage : Model -> Page
currentPage model =
    case model.page of
        TransitioningFrom page ->
            page

        Loaded page ->
            page


getUser : Model -> Maybe User
getUser model =
    case currentPage model of
        Protected user _ ->
            Just user

        _ ->
            Nothing


setPageLater : Page -> Cmd Msg
setPageLater page =
    Process.sleep 600
        |> Task.map (\_ -> page)
        |> Task.perform SetPage



-- VIEW


view : Model -> Document Msg
view model =
    { title = "Jangle"
    , body =
        [ p [] [ text <| Debug.toString model ]
        , p [] [ a [ href "/" ] [ text "Home" ] ]
        , p [] [ a [ href "/broken-link" ] [ text "Broken link" ] ]
        ]
    }



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
