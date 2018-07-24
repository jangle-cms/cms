module Context
    exposing
        ( Model
        , Msg(..)
        , init
        , update
        )


type alias Model =
    { connection : Maybe String
    }


init : Model
init =
    Model Nothing


type Msg
    = NoOp


update : Msg -> Model -> Model
update msg model =
    case msg of
        NoOp ->
            model
