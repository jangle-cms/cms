module Pages.SignIn exposing (Model, Msg, init, update)

-- MODEL


type alias Model =
    { email : String
    , password : String
    }


init : Model
init =
    Model "" ""



-- UPDATE


type Msg
    = UpdateField Field String


type Field
    = Email
    | Password


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    normalizeUpdate <|
        case msg of
            UpdateField Email email ->
                { model | email = email }

            UpdateField Password password ->
                { model | password = password }


normalizeUpdate : Model -> ( Model, Cmd Msg )
normalizeUpdate model =
    ( model, Cmd.none )
