module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
import Jangle exposing (Connection)
import Jangle.User exposing (User)
import Pages.Content
import Pages.Dashboard
import Pages.Item
import Pages.List
import Pages.ListItem
import Pages.Media
import Pages.NotFound
import Pages.SignIn
import Pages.Users
import Route
import Url exposing (Url)
import Utils



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
    = Dashboard Pages.Dashboard.Model
    | Content Pages.Content.Model
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
    | SetTransition Transition
    | OnPageMsg PageMsg


type PageMsg
    = SignInMsg Pages.SignIn.Msg
    | DashboardMsg Pages.Dashboard.Msg
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
            pageFrom connection flags.user url
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

        SetTransition transition ->
            ( { model | transition = transition }
            , Cmd.none
            )

        OnUrlChange url ->
            let
                ( page, pageCmd ) =
                    pageFrom model.connection (userFrom model.page) url
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

        OnPageMsg pageMsg ->
            let
                ( newPage, newPageCmd ) =
                    updatePage pageMsg model.page
            in
            ( { model | page = newPage }
            , Cmd.map OnPageMsg newPageCmd
            )


updatePage : PageMsg -> Page -> ( Page, Cmd PageMsg )
updatePage pageMsg pageModel =
    case ( pageMsg, pageModel ) of
        ( SignInMsg msg, SignIn model ) ->
            let
                ( newModel, newCmd ) =
                    Pages.SignIn.update msg model
            in
            ( SignIn newModel, Cmd.map SignInMsg newCmd )

        ( _, _ ) ->
            Debug.log "Unhandled updatePage" ( pageModel, Cmd.none )



-- VIEW


view : Model -> Document Msg
view model =
    viewPage model.page


viewPage : Page -> Document Msg
viewPage page =
    documentMap OnPageMsg <|
        case page of
            SignIn model ->
                Document "SignIn" [ Html.map SignInMsg (Pages.SignIn.view model) ]

            NotFound modelNotFoundPages ->
                Document "NotFound" [ Pages.NotFound.view ]

            Admin user protectedPage ->
                case protectedPage of
                    Content _ ->
                        Document "Content" [ Pages.Content.view ]

                    Dashboard _ ->
                        Document "Dashboard" [ Pages.Dashboard.view ]

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
    300


userFrom : Page -> Maybe User
userFrom page =
    case page of
        SignIn _ ->
            Nothing

        NotFound _ ->
            Nothing

        Admin user _ ->
            Just user


pageFrom : Connection -> Maybe User -> Url -> ( Page, Cmd PageMsg )
pageFrom connection possibleUser url =
    case possibleUser of
        Just user ->
            case Route.routeFrom url of
                Route.Content ->
                    ( Admin user (Content Nothing)
                    , Cmd.none
                    )

                Route.Dashboard ->
                    ( Admin user (Dashboard Nothing)
                    , Cmd.none
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
            case Route.routeFrom url of
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
