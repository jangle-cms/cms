module Pages.Dashboard exposing (Model, Msg, init, update)

import Jangle



-- MODEL


type alias Model =
    { lists : RemoteData (List Int)
    }


type RemoteData a
    = NotLoaded
    | Loading
    | Success a
    | Failure String


init : Model
init =
    Model NotLoaded



-- UPDATE


type Msg
    = SetLists (Result String (List Int))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    normalizeUpdate <|
        case msg of
            SetLists (Ok lists) ->
                { model | lists = Success lists }

            SetLists (Err reason) ->
                { model | lists = Failure reason }


normalizeUpdate : Model -> ( Model, Cmd Msg )
normalizeUpdate model =
    ( model, Cmd.none )
