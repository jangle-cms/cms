module Utils exposing
    ( after
    , perform
    , updatedTimeLabel
    )

import DateFormat
import Dict
import Iso8601
import Jangle.List.Item as Item
import Process
import Task
import Time


after : Float -> msg -> Cmd msg
after delay msg =
    Process.sleep delay
        |> Task.perform (always msg)


perform : msg -> Cmd msg
perform =
    after 0


updatedTimeLabel : Time.Zone -> Item.Item -> Maybe String
updatedTimeLabel timezone item =
    Dict.get "jangle" item.fields
        |> Maybe.andThen Item.objectFrom
        |> Maybe.andThen (Dict.get "updated")
        |> Maybe.andThen Item.objectFrom
        |> Maybe.andThen (Dict.get "at")
        |> Maybe.andThen Item.stringFrom
        |> Maybe.map Iso8601.toTime
        |> Maybe.andThen Result.toMaybe
        |> Maybe.map
            (DateFormat.format
                [ DateFormat.monthNameFull
                , DateFormat.text " "
                , DateFormat.dayOfMonthSuffix
                , DateFormat.text ", "
                , DateFormat.yearNumber
                , DateFormat.text " at "
                , DateFormat.hourNumber
                , DateFormat.text ":"
                , DateFormat.minuteFixed
                , DateFormat.amPmLowercase
                ]
                timezone
            )
