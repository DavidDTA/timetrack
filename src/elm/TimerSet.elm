module TimerSet exposing (Timer, TimerId, TimerSet, addTimer, decodeTimerSet, encodeTimerSet, listTimers, renameTimer, reset, toggleTimer)

import Duration
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import List.Extra
import Quantity
import Time


type TimerSet
    = TimerSet
        { timers : List Timer
        }


type alias Timer =
    { accumulated : Duration.Duration
    , name : Maybe String
    , started : Maybe Time.Posix
    }


type TimerId
    = TimerId Int


addTimer : TimerSet -> TimerSet
addTimer (TimerSet timerSet) =
    TimerSet
        { timerSet
            | timers =
                timerSet.timers
                    ++ [ { accumulated = Quantity.zero
                         , name = Nothing
                         , started = Nothing
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
                                | name =
                                    case trimmed of
                                        "" ->
                                            Nothing

                                        _ ->
                                            Just trimmed
                            }
                        )
        }


reset : TimerSet -> TimerSet
reset (TimerSet timerSet) =
    TimerSet { timers = [] }


toggleTimer : TimerId -> Time.Posix -> TimerSet -> TimerSet
toggleTimer (TimerId id) now (TimerSet timerSet) =
    TimerSet
        { timerSet
            | timers =
                timerSet.timers
                    |> List.indexedMap
                        (\mapIndex item ->
                            case item.started of
                                Just started ->
                                    { item
                                        | accumulated =
                                            item.accumulated
                                                |> Quantity.plus (Quantity.max Quantity.zero (Duration.from started now))
                                        , started = Nothing
                                    }

                                Nothing ->
                                    if id == mapIndex then
                                        { item
                                            | started = Just now
                                        }

                                    else
                                        item
                        )
        }


decodeTimerSet : Json.Decode.Decoder TimerSet
decodeTimerSet =
    Json.Decode.succeed (\timers -> TimerSet { timers = timers })
        |> Json.Decode.Pipeline.optional "timers"
            (Json.Decode.list decodeTimer)
            []


encodeTimerSet : TimerSet -> Json.Encode.Value
encodeTimerSet (TimerSet timerSet) =
    Json.Encode.object
        [ ( "timers", Json.Encode.list encodeTimer timerSet.timers )
        ]


decodeTimer : Json.Decode.Decoder Timer
decodeTimer =
    Json.Decode.succeed Timer
        |> Json.Decode.Pipeline.required "accumulated"
            (Json.Decode.float
                |> Json.Decode.map Duration.seconds
            )
        |> Json.Decode.Pipeline.optional "name"
            (Json.Decode.string
                |> Json.Decode.map Just
            )
            Nothing
        |> Json.Decode.Pipeline.required "started"
            (Json.Decode.int
                |> Json.Decode.map Time.millisToPosix
                |> Json.Decode.nullable
            )


encodeTimer : Timer -> Json.Encode.Value
encodeTimer timer =
    Json.Encode.object
        [ ( "accumulated"
          , timer.accumulated
                |> Duration.inSeconds
                |> Json.Encode.float
          )
        , ( "name"
          , timer.name
                |> Maybe.map Json.Encode.string
                |> Maybe.withDefault Json.Encode.null
          )
        , ( "started"
          , timer.started
                |> Maybe.map Time.posixToMillis
                |> Maybe.map Json.Encode.int
                |> Maybe.withDefault Json.Encode.null
          )
        ]
