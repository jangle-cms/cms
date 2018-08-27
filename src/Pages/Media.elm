module Pages.Media exposing (Model, Msg, view)

import Html exposing (..)


type alias Model =
    Maybe String


type Msg
    = NoOp


view : Html msg
view =
    h1 [] [ text "Media" ]
