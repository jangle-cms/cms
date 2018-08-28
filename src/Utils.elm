module Utils exposing (after, perform)

import Process
import Task


after : Float -> msg -> Cmd msg
after delay msg =
    Process.sleep delay
        |> Task.perform (always msg)


perform : msg -> Cmd msg
perform =
    after 0
