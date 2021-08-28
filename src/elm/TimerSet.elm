module TimerSet exposing (Timer, TimerId, TimerSet, addTimer, decodeTimerSet, encodeTimerSet, history, listTimers, renameTimer, reset, toggleTimer)

import Duration
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra
import List.Extra
import Quantity
import Time
import Timeline


type TimerSet
    = TimerSet
        { timers : List Timer
        , history : Timeline.Timeline TimerId
        }


type alias Timer =
    { name : String
    }


type TimerId
    = TimerId Int


history : TimerSet -> Timeline.Timeline TimerId
history (TimerSet timerSet) =
    timerSet.history


addTimer : TimerSet -> TimerSet
addTimer (TimerSet timerSet) =
    TimerSet
        { timerSet
            | timers =
                timerSet.timers
                    ++ [ { name = ""
                         }
                       ]
        }


listTimers : TimerSet -> List ( TimerId, Timer )
listTimers (TimerSet { timers }) =
    List.indexedMap (\index timer -> ( TimerId index, timer )) timers


renameTimer : TimerId -> String -> TimerSet -> TimerSet
renameTimer (TimerId id) name (TimerSet timerSet) =
    TimerSet
        { timerSet
            | timers =
                timerSet.timers
                    |> List.Extra.updateAt id
                        (\item ->
                            let
                                trimmed =
                                    String.trim name
                            in
                            { item
                                | name = trimmed
                            }
                        )
        }


reset : TimerSet -> TimerSet
reset (TimerSet timerSet) =
    TimerSet { timers = [], history = Timeline.empty }


toggleTimer : TimerId -> Time.Posix -> TimerSet -> TimerSet
toggleTimer timerId now (TimerSet timerSet) =
    let
        value =
            case Timeline.at now timerSet.history of
                Nothing ->
                    Just timerId

                Just currentTimerId ->
                    if currentTimerId == timerId then
                        Nothing

                    else
                        Just timerId
    in
    TimerSet
        { timerSet
            | history = Timeline.set value now Nothing timerSet.history
        }


decodeTimeline : Json.Decode.Decoder (Timeline.Timeline TimerId)
decodeTimeline =
    Json.Decode.list
        (Json.Decode.succeed Tuple.pair
            |> Json.Decode.Pipeline.required "start"
                (Json.Decode.int
                    |> Json.Decode.map Time.millisToPosix
                )
            |> Json.Decode.Pipeline.required "timer"
                (Json.Decode.int
                    |> Json.Decode.map TimerId
                    |> Json.Decode.nullable
                )
        )
        |> Json.Decode.map Timeline.fromList


encodeTimeline : Timeline.Timeline TimerId -> Json.Encode.Value
encodeTimeline timeline =
    timeline
        |> Timeline.toList
        |> Json.Encode.list
            (\( posix, value ) ->
                Json.Encode.object
                    [ ( "start", Json.Encode.int (Time.posixToMillis posix) )
                    , ( "timer", Json.Encode.Extra.maybe ((\(TimerId id) -> id) >> Json.Encode.int) value )
                    ]
            )


decodeTimerSet : Json.Decode.Decoder TimerSet
decodeTimerSet =
    Json.Decode.succeed (\timers history_ -> TimerSet { timers = timers, history = history_ })
        |> Json.Decode.Pipeline.optional "timers"
            (Json.Decode.list decodeTimer)
            []
        |> Json.Decode.Pipeline.optional "history"
            decodeTimeline
            Timeline.empty


encodeTimerSet : TimerSet -> Json.Encode.Value
encodeTimerSet (TimerSet timerSet) =
    Json.Encode.object
        [ ( "timers", Json.Encode.list encodeTimer timerSet.timers )
        , ( "history", encodeTimeline timerSet.history )
        ]


decodeTimer : Json.Decode.Decoder Timer
decodeTimer =
    Json.Decode.succeed Timer
        |> Json.Decode.Pipeline.required "name"
            Json.Decode.string


encodeTimer : Timer -> Json.Encode.Value
encodeTimer timer =
    Json.Encode.object
        [ ( "name"
          , Json.Encode.string timer.name
          )
        ]
