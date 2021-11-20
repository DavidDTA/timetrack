module Api exposing (Request(..), Response(..))

import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra
import Serialize
import Time
import Timeline
import TimerSet

type Request =
    Get
    | Update (List TimelineUpdate)

type Response =
    Value TimerSet.TimerSet

type Update =
    TimersAddAndStart Time.Posix
    | TimersClear
    | TimersRename TimerSet.TimerId String
    | TimersSetActivity TimerSet.TimerId TimerSet.Activity
    | TimersSetCategory TimerSet.TimerId TimerSet.Category
    | TimersSetActive (Maybe TimerSet.TimerId) Time.Posix


timerSet =
    Serialize.record TimerSet.create
        |> Serialize.field
            (\timerSet_ ->
                TimerSet.listTimerIds timerSet_
                    |> List.filterMap (\id -> TimerSet.get id timerSet_)
            )
            (Serialize.list timer)
        |> Serialize.field TimerSet.history timeline
        |> Serialize.finishRecord


timeline =
    Serialize.tuple
        (Serialize.map Time.millisToPosix Time.posixToMillis Serialize.int)
        (Serialize.maybe (Serialize.map TimerSet.timerIdFromRaw TimerSet.timerIdToRaw Serialize.int))
        |> Serialize.list
        |> Serialize.map Timeline.fromList Timeline.toList


timer =
    Serialize.record TimerSet.Timer
        |> Serialize.field .name Serialize.string
        |> Serialize.field
            .activity
            (Serialize.maybe
                (Serialize.customType
                    (\active reactive proactive value ->
                        case value of
                            TimerSet.Active ->
                                active

                            TimerSet.Reactive ->
                                reactive

                            TimerSet.Proactive ->
                                proactive
                    )
                    |> Serialize.variant0 TimerSet.Active
                    |> Serialize.variant0 TimerSet.Reactive
                    |> Serialize.variant0 TimerSet.Proactive
                    |> Serialize.finishCustomType
                )
            )
        |> Serialize.field
            .category
            (Serialize.maybe
                (Serialize.customType
                    (\operational helpful productive value ->
                        case value of
                            TimerSet.Operational ->
                                operational

                            TimerSet.Helpful ->
                                helpful

                            TimerSet.Productive ->
                                productive
                    )
                    |> Serialize.variant0 TimerSet.Operational
                    |> Serialize.variant0 TimerSet.Helpful
                    |> Serialize.variant0 TimerSet.Productive
                    |> Serialize.finishCustomType
                )
            )
        |> Serialize.finishRecord


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
encodeTimeline timeline_ =
    timeline_
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
encodeTimerSet timerSet_ =
    Json.Encode.object
        [ ( "timers"
          , Json.Encode.list encodeTimer
                (TimerSet.listTimerIds timerSet_
                    |> List.filterMap (\id -> TimerSet.get id timerSet_)
                )
          )
        , ( "history", encodeTimeline (TimerSet.history timerSet_) )
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
encodeTimer timer_ =
    Json.Encode.object
        [ ( "name", Json.Encode.string timer_.name )
        , ( "activity"
          , case timer_.activity of
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
          , case timer_.category of
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
