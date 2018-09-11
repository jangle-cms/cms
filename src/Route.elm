module Route exposing (Route(..), routeFrom)

import Url exposing (Url)
import Url.Parser as Parser exposing (..)


type alias Slug =
    String


type Route
    = Content
    | List Slug
    | ListItem Slug Slug
    | Item Slug
    | Media
    | Users
    | SignIn
    | NotFound


routeFrom : String -> Url -> Route
routeFrom prefix url =
    Maybe.withDefault NotFound
        (parse (route prefix) url)


route : String -> Parser (Route -> a) a
route prefix =
    oneOf
        [ map Content top
        , map List (s "lists" </> string)
        , map ListItem (s "lists" </> string </> string)
        , map Item (s "items" </> string)
        , map Media (s "media")
        , map Users (s "users")
        , map SignIn (s "sign-in")
        ]
