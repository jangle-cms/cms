module Global exposing (Msg(..))

import Jangle.User exposing (User)
import Route exposing (Route)


type Msg
    = SignIn User
    | SignOut
    | Navigate Route
