module Global exposing (Msg(..))

import Jangle.User exposing (User)


type Msg
    = SignIn User
