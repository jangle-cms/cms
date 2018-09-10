port module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Global
import Html exposing (..)
import Html.Attributes exposing (..)
import Jangle exposing (Connection)
import Jangle.User exposing (User)
import Json.Encode as Encode
import Pages.Content
import Pages.Item
import Pages.List
import Pages.ListItem
import Pages.Media
import Pages.NotFound
import Pages.SignIn
import Pages.Users
import Route exposing (Route)
import Url exposing (Url)
import Utils



-- PORTS


port outgoing : Encode.Value -> Cmd msg


type PortMsg
    = SetUser User
    | ResetUser


makeAction : String -> Encode.Value
makeAction action =
    Encode.object
        [ ( "action", Encode.string action )
        ]


makeActionWithData : String -> Encode.Value -> Encode.Value
makeActionWithData action data =
    Encode.object
        [ ( "action", Encode.string action )
        , ( "data", data )
        ]


sendOutgoingMessage : PortMsg -> Cmd msg
sendOutgoingMessage msg =
    outgoing <|
        case msg of
            SetUser user ->
                makeActionWithData "SetUser" (Jangle.User.encode user)

            ResetUser ->
                makeAction "ResetUser"



-- MODEL


type alias Model =
    { key : Nav.Key
    , transition : Transition
    , connection : Connection
    , page : Page
    }


type Page
    = SignIn Pages.SignIn.Model
    | NotFound Pages.NotFound.Model
    | Admin User ProtectedPage


type ProtectedPage
    = Content Pages.Content.Model
    | List Pages.List.Model
    | ListItem Pages.ListItem.Model
    | Item Pages.Item.Model
    | Media Pages.Media.Model
    | Users Pages.Users.Model


type Transition
    = NotReady
    | Entering
    | Leaving
    | Ready



-- MSG


type Msg
    = OnUrlChange Url
    | OnUrlRequest Browser.UrlRequest
    | SetPage Page
    | SetPageAndThen Page (Cmd Msg)
    | SetTransition Transition
    | OnPageMsg PageMsg
    | GlobalMsg Global.Msg


type PageMsg
    = SignInMsg Pages.SignIn.Msg
    | ContentMsg Pages.Content.Msg
    | ListMsg Pages.List.Msg
    | ListItemMsg Pages.ListItem.Msg
    | ItemMsg Pages.Item.Msg
    | MediaMsg Pages.Media.Msg
    | UsersMsg Pages.Users.Msg



-- MAIN


type alias Flags =
    { user : Maybe User
    , apiUrl : String
    }


main : Program Flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlChange = OnUrlChange
        , onUrlRequest = OnUrlRequest
        }



-- INIT


init : Flags -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        connection =
            Jangle.connect flags.apiUrl

        ( page, pageCmd ) =
            pageFromUrl url connection flags.user
    in
    ( Model key NotReady connection page
    , Cmd.batch
        [ Utils.after transitionSpeed (SetTransition Ready)
        , Cmd.map OnPageMsg pageCmd
        ]
    )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetPage page ->
            ( { model | transition = Entering, page = page }
            , Utils.after transitionSpeed (SetTransition Ready)
            )

        SetPageAndThen page cmd ->
            ( { model | transition = Entering, page = page }
            , Cmd.batch
                [ cmd
                , Utils.after transitionSpeed (SetTransition Ready)
                ]
            )

        SetTransition transition ->
            ( { model | transition = transition }
            , Cmd.none
            )

        OnUrlChange url ->
            let
                ( page, pageCmd ) =
                    pageFromUrl url model.connection (userFrom model.page)
            in
            ( { model | transition = Leaving }
            , Cmd.batch
                [ Utils.after transitionSpeed (SetPage page)
                , Cmd.map OnPageMsg pageCmd
                ]
            )

        OnUrlRequest urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        GlobalMsg globalMsg ->
            case globalMsg of
                Global.SignIn user ->
                    let
                        ( page, cmd ) =
                            Pages.Content.init user model.connection
                    in
                    ( { model | transition = Leaving }
                    , Cmd.batch
                        [ Utils.after transitionSpeed
                            (SetPageAndThen (Admin user (Content page))
                                (cmd
                                    |> Cmd.map ContentMsg
                                    |> Cmd.map OnPageMsg
                                )
                            )
                        , sendOutgoingMessage (SetUser user)
                        ]
                    )

                Global.SignOut ->
                    let
                        ( page, cmd ) =
                            pageFromRoute Route.SignIn model.connection Nothing
                    in
                    ( { model | transition = Leaving }
                    , Cmd.batch
                        [ Utils.after transitionSpeed (SetPageAndThen page (Cmd.map OnPageMsg cmd))
                        , sendOutgoingMessage ResetUser
                        ]
                    )

        OnPageMsg pageMsg ->
            let
                ( newPage, newPageCmd, globalCmd ) =
                    updatePage pageMsg model.page
            in
            ( { model | page = newPage }
            , Cmd.batch
                [ Cmd.map OnPageMsg newPageCmd
                , Cmd.map GlobalMsg globalCmd
                ]
            )


updatePage : PageMsg -> Page -> ( Page, Cmd PageMsg, Cmd Global.Msg )
updatePage pageMsg pageModel =
    case ( pageMsg, pageModel ) of
        ( SignInMsg msg, SignIn model ) ->
            let
                ( newModel, newCmd, globalCmd ) =
                    Pages.SignIn.update msg model
            in
            ( SignIn newModel
            , Cmd.map SignInMsg newCmd
            , globalCmd
            )

        ( _, SignIn _ ) ->
            ignoreUpdate pageModel

        ( _, NotFound _ ) ->
            ignoreUpdate pageModel

        ( _, Admin user adminPageModel ) ->
            case ( pageMsg, adminPageModel ) of
                ( ContentMsg msg, Content model ) ->
                    let
                        ( newModel, newCmd, globalCmd ) =
                            Pages.Content.update msg model
                    in
                    ( Admin user (Content newModel)
                    , Cmd.map ContentMsg newCmd
                    , globalCmd
                    )

                ( _, _ ) ->
                    let
                        _ =
                            Debug.log "Unhandled updatePage" pageMsg
                    in
                    ( pageModel, Cmd.none, Cmd.none )


ignoreUpdate : Page -> ( Page, Cmd PageMsg, Cmd Global.Msg )
ignoreUpdate pageModel =
    ( pageModel, Cmd.none, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    let
        { title, body } =
            viewPage model.page
    in
    { title = title
    , body =
        [ div
            [ class "app"
            , classList
                [ ( "app--entering", model.transition == Entering )
                , ( "app--ready", model.transition == Ready )
                ]
            ]
            body
        ]
    }


viewPage : Page -> Document Msg
viewPage page =
    documentMap OnPageMsg <|
        case page of
            SignIn model ->
                Document "Jangle" [ Html.map SignInMsg (Pages.SignIn.view model) ]

            NotFound modelNotFoundPages ->
                Document "Jangle" [ Pages.NotFound.view ]

            Admin user protectedPage ->
                case protectedPage of
                    Content model ->
                        Document "Content | Jangle" [ Pages.Content.view model ]
                            |> documentMap ContentMsg

                    Item _ ->
                        Document "Item" [ Pages.Item.view ]

                    List _ ->
                        Document "List" [ Pages.List.view ]

                    ListItem _ ->
                        Document "ListItem" [ Pages.ListItem.view ]

                    Media _ ->
                        Document "Media" [ Pages.Media.view ]

                    Users _ ->
                        Document "Users" [ Pages.Users.view ]



-- OTHER


transitionSpeed : Float
transitionSpeed =
    650


userFrom : Page -> Maybe User
userFrom page =
    case page of
        SignIn _ ->
            Nothing

        NotFound _ ->
            Nothing

        Admin user _ ->
            Just user


pageFromUrl : Url -> Connection -> Maybe User -> ( Page, Cmd PageMsg )
pageFromUrl url =
    pageFromRoute (Route.routeFrom url)


pageFromRoute : Route -> Connection -> Maybe User -> ( Page, Cmd PageMsg )
pageFromRoute route connection possibleUser =
    case possibleUser of
        Just user ->
            case route of
                Route.Content ->
                    let
                        ( page, pageCmd ) =
                            Pages.Content.init user connection
                    in
                    ( Admin user (Content page)
                    , Cmd.map ContentMsg pageCmd
                    )

                Route.Item itemSlug ->
                    ( Admin user (Item Nothing)
                    , Cmd.none
                    )

                Route.List listSlug ->
                    ( Admin user (List Nothing)
                    , Cmd.none
                    )

                Route.ListItem listSlug itemSlug ->
                    ( Admin user (ListItem Nothing)
                    , Cmd.none
                    )

                Route.Media ->
                    ( Admin user (Media Nothing)
                    , Cmd.none
                    )

                Route.Users ->
                    ( Admin user (Users Nothing)
                    , Cmd.none
                    )

                Route.NotFound ->
                    ( NotFound Nothing
                    , Cmd.none
                    )

                Route.SignIn ->
                    let
                        ( page, pageCmd ) =
                            Pages.SignIn.init connection (Just user)
                    in
                    ( SignIn page, Cmd.map SignInMsg pageCmd )

        Nothing ->
            case route of
                Route.NotFound ->
                    ( NotFound Nothing
                    , Cmd.none
                    )

                _ ->
                    let
                        ( page, pageCmd ) =
                            Pages.SignIn.init connection Nothing
                    in
                    ( SignIn page, Cmd.map SignInMsg pageCmd )


documentMap : (a -> b) -> Document a -> Document b
documentMap fn { title, body } =
    { title = title
    , body = List.map (Html.map fn) body
    }
