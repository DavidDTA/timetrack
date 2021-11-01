module Codec.Api exposing (decodeTimerSet, encodeTimerSet)

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra
import Time
import Timeline
import TimerSet


decodeTimeline : Json.Decode.Decoder (Timeline.Timeline TimerSet.TimerId)
decodeTimeline =
    Json.Decode.list
        (Json.Decode.succeed Tuple.pair
            |> Json.Decode.Pipeline.required "start"
                (Json.Decode.int
                    |> Json.Decode.map Time.millisToPosix
                )
            |> Json.Decode.Pipeline.required "timer"
                (Json.Decode.int
                    |> Json.Decode.map TimerSet.timerIdFromRaw
                    |> Json.Decode.nullable
                )
        )
        |> Json.Decode.map Timeline.fromList


encodeTimeline : Timeline.Timeline TimerSet.TimerId -> Json.Encode.Value
encodeTimeline timeline =
    timeline
        |> Timeline.toList
        |> Json.Encode.list
            (\( posix, value ) ->
                Json.Encode.object
                    [ ( "start", Json.Encode.int (Time.posixToMillis posix) )
                    , ( "timer", Json.Encode.Extra.maybe (TimerSet.timerIdToRaw >> Json.Encode.int) value )
                    ]
            )


decodeTimerSet : Json.Decode.Decoder TimerSet.TimerSet
decodeTimerSet =
    Json.Decode.succeed TimerSet.create
        |> Json.Decode.Pipeline.optional "timers"
            (Json.Decode.list decodeTimer)
            []
        |> Json.Decode.Pipeline.optional "history"
            decodeTimeline
            Timeline.empty


encodeTimerSet : TimerSet.TimerSet -> Json.Encode.Value
encodeTimerSet timerSet =
    Json.Encode.object
        [ ( "timers"
          , Json.Encode.list encodeTimer
                (TimerSet.listTimerIds timerSet
                    |> List.filterMap (\id -> TimerSet.get id timerSet)
                )
          )
        , ( "history", encodeTimeline (TimerSet.history timerSet) )
        ]


decodeTimer : Json.Decode.Decoder TimerSet.Timer
decodeTimer =
    Json.Decode.succeed TimerSet.Timer
        |> Json.Decode.Pipeline.required "name" Json.Decode.string
        |> Json.Decode.Pipeline.optional "activity"
            (Json.Decode.string
                |> Json.Decode.map
                    (\value ->
                        case value of
                            "A" ->
                                Just TimerSet.Active

                            "R" ->
                                Just TimerSet.Reactive

                            "P" ->
                                Just TimerSet.Proactive

                            _ ->
                                Nothing
                    )
            )
            Nothing
        |> Json.Decode.Pipeline.optional "category"
            (Json.Decode.string
                |> Json.Decode.map
                    (\value ->
                        case value of
                            "O" ->
                                Just TimerSet.Operational

                            "H" ->
                                Just TimerSet.Helpful

                            "P" ->
                                Just TimerSet.Productive

                            _ ->
                                Nothing
                    )
            )
            Nothing


encodeTimer : TimerSet.Timer -> Json.Encode.Value
encodeTimer timer =
    Json.Encode.object
        [ ( "name", Json.Encode.string timer.name )
        , ( "activity"
          , case timer.activity of
                Nothing ->
                    Json.Encode.null

                Just TimerSet.Active ->
                    Json.Encode.string "A"

                Just TimerSet.Reactive ->
                    Json.Encode.string "R"

                Just TimerSet.Proactive ->
                    Json.Encode.string "P"
          )
        , ( "category"
          , case timer.category of
                Nothing ->
                    Json.Encode.null

                Just TimerSet.Operational ->
                    Json.Encode.string "O"

                Just TimerSet.Helpful ->
                    Json.Encode.string "H"

                Just TimerSet.Productive ->
                    Json.Encode.string "P"
          )
        ]
