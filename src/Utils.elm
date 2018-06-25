module Utils exposing (..)

import Process
import Task exposing (Task)


delay : Float -> msg -> Cmd msg
delay milliseconds msg =
    Task.perform (always msg) (Process.sleep milliseconds)


send : msg -> Cmd msg
send =
    delay 0


delayTask : Float -> Task a b -> Task a b
delayTask milliseconds task =
    Task.andThen (always task) (Process.sleep milliseconds)
