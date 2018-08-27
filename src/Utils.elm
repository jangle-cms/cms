module Utils exposing (after)

import Process
import Task


after : Float -> msg -> Cmd msg
after delay msg =
    Process.sleep delay
        |> Task.perform (always msg)
