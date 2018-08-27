module Route exposing (Route(..), routeFrom)

import Url exposing (Url)
import Url.Parser as Parser exposing (..)


type alias Slug =
    String


type Route
    = Dashboard
    | Content
    | List Slug
    | ListItem Slug Slug
    | Item Slug
    | Media
    | Users
    | SignIn
    | NotFound


routeFrom : Url -> Route
routeFrom url =
    Maybe.withDefault NotFound (parse route url)


route : Parser (Route -> a) a
route =
    oneOf
        [ map Dashboard top
        , map Content (s "content")
        , map List (s "lists" </> string)
        , map ListItem (s "lists" </> string </> string)
        , map Item (s "items" </> string)
        , map Media (s "media")
        , map Users (s "users")
        , map SignIn (s "sign-in")
        ]
