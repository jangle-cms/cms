module Main exposing (main)

import Browser exposing (Document, UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (..)
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
    , page : Page
    }


type Page
    = SignIn Pages.SignIn.Model
    | NotFound Pages.NotFound.Model
    | Admin User ProtectedPage


type alias User =
    { name : String
    , email : String
    , token : String
    }


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
    ( Model key NotReady (pageFrom flags.user url)
    , Utils.after transitionSpeed (SetTransition Ready)
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
            ( { model | transition = Leaving }
            , Utils.after transitionSpeed (SetPage (pageFrom (userFrom model.page) url))
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
updatePage msg model =
    ( model, Cmd.none )



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


pageFrom : Maybe User -> Url -> Page
pageFrom possibleUser url =
    case possibleUser of
        Just user ->
            case Route.routeFrom url of
                Route.Content ->
                    Admin user (Content Nothing)

                Route.Dashboard ->
                    Admin user (Dashboard Nothing)

                Route.Item itemSlug ->
                    Admin user (Item Nothing)

                Route.List listSlug ->
                    Admin user (List Nothing)

                Route.ListItem listSlug itemSlug ->
                    Admin user (ListItem Nothing)

                Route.Media ->
                    Admin user (Media Nothing)

                Route.Users ->
                    Admin user (Users Nothing)

                Route.NotFound ->
                    NotFound Nothing

                Route.SignIn ->
                    SignIn Pages.SignIn.init

        Nothing ->
            case Route.routeFrom url of
                Route.NotFound ->
                    NotFound Nothing

                _ ->
                    SignIn Pages.SignIn.init


documentMap : (a -> b) -> Document a -> Document b
documentMap fn { title, body } =
    { title = title
    , body = List.map (Html.map fn) body
    }
