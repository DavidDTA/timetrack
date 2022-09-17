module Api exposing (Request(..), Response(..), Update(..), applyUpdate, endpoint)

import Functions
import Http
import Json.Decode
import Json.Decode.Extra
import Json.Decode.Pipeline
import Json.Encode
import Json.Encode.Extra
import Serialize
import Time
import Timeline
import TimerSet
import Version



-- Authentication is a strong word here. For debugging purposes, we simply trust the client is who they say they are.


type alias RequestWithAuthentication =
    { usernameByFiat : String, request : Request }


type Request
    = Get
    | Update Version.Version (List Update)


type Response
    = Value { version : Version.Version, value : TimerSet.TimerSet }


type Update
    = TimersAddAndStart { name : String, start : Time.Posix }
    | TimersRename TimerSet.TimerId String
    | TimersSetActivity TimerSet.TimerId (Maybe TimerSet.Activity)
    | TimersSetCategory TimerSet.TimerId (Maybe TimerSet.Category)
    | TimersSetActive
        { timerId : Maybe TimerSet.TimerId
        , start : Time.Posix
        , end : Maybe Time.Posix
        }


applyUpdate apiUpdate timerSet =
    case apiUpdate of
        TimersAddAndStart { name, start } ->
            let
                ( newTimerSet, newTimerId ) =
                    TimerSet.addTimer name timerSet
            in
            TimerSet.setTimer (Just newTimerId) start Nothing newTimerSet

        TimersRename timerId name ->
            TimerSet.updateTimer timerId (\timer -> { timer | name = String.trim name }) timerSet

        TimersSetActivity timerId activity ->
            TimerSet.updateTimer timerId
                (\timer ->
                    { timer
                        | activity =
                            if timer.activity == activity then
                                Nothing

                            else
                                activity
                    }
                )
                timerSet

        TimersSetCategory timerId category ->
            TimerSet.updateTimer timerId
                (\timer ->
                    { timer
                        | category =
                            if timer.category == category then
                                Nothing

                            else
                                category
                    }
                )
                timerSet

        TimersSetActive { timerId, start, end } ->
            TimerSet.setTimer timerId start end timerSet


endpoint =
    Functions.endpoint "/-/api" serializeRequestWithAuthentication serializeResponse


serializeRequestWithAuthentication =
    Serialize.record RequestWithAuthentication
        |> Serialize.field .usernameByFiat Serialize.string
        |> Serialize.field .request serializeRequest
        |> Serialize.finishRecord


serializeVersion =
    Serialize.map Version.fromRaw Version.toRaw Serialize.int


serializeVersioned serialize =
    Serialize.record (\version value -> { version = version, value = value })
        |> Serialize.field .version serializeVersion
        |> Serialize.field .value serialize
        |> Serialize.finishRecord


serializeRequest =
    Serialize.customType
        (\getEncoder updateEncoder value ->
            case value of
                Get ->
                    getEncoder

                Update version updates ->
                    updateEncoder version updates
        )
        |> Serialize.variant0 Get
        |> Serialize.variant2 Update serializeVersion (Serialize.list serializeUpdate)
        |> Serialize.finishCustomType


serializeResponse =
    Serialize.customType
        (\valueEncoder value ->
            case value of
                Value timerSet ->
                    valueEncoder timerSet
        )
        |> Serialize.variant1 Value (serializeVersioned serializeTimerSet)
        |> Serialize.finishCustomType


serializeUpdate =
    Serialize.customType
        (\timersAddAndStartEncoder timersRenameEncoder timersSetActivityEncoder timersSetCategoryEncoder timersSetActiveEncoder value ->
            case value of
                TimersAddAndStart posix ->
                    timersAddAndStartEncoder posix

                TimersRename timerId name ->
                    timersRenameEncoder timerId name

                TimersSetActivity timerId activity ->
                    timersSetActivityEncoder timerId activity

                TimersSetCategory timerId category ->
                    timersSetCategoryEncoder timerId category

                TimersSetActive params ->
                    timersSetActiveEncoder params
        )
        |> Serialize.variant1 TimersAddAndStart
            (Serialize.record (\name start -> { name = name, start = start })
                |> Serialize.field .name Serialize.string
                |> Serialize.field .start serializePosix
                |> Serialize.finishRecord
            )
        |> Serialize.variant2 TimersRename serializeTimerId Serialize.string
        |> Serialize.variant2 TimersSetActivity serializeTimerId (Serialize.maybe serializeActivity)
        |> Serialize.variant2 TimersSetCategory serializeTimerId (Serialize.maybe serializeCategory)
        |> Serialize.variant1 TimersSetActive
            (Serialize.record (\timerId start end -> { timerId = timerId, start = start, end = end })
                |> Serialize.field .timerId (Serialize.maybe serializeTimerId)
                |> Serialize.field .start serializePosix
                |> Serialize.field .end (Serialize.maybe serializePosix)
                |> Serialize.finishRecord
            )
        |> Serialize.finishCustomType


serializeTimerSet =
    Serialize.record TimerSet.create
        |> Serialize.field
            (\timerSet ->
                TimerSet.listTimerIds timerSet
                    |> List.filterMap (\id -> TimerSet.get id timerSet)
            )
            (Serialize.list serializeTimer)
        |> Serialize.field TimerSet.history serializeTimeline
        |> Serialize.finishRecord


serializeTimeline =
    Serialize.tuple serializePosix (Serialize.maybe serializeTimerId)
        |> Serialize.list
        |> Serialize.map Timeline.fromList Timeline.toList


serializeTimer =
    Serialize.record TimerSet.Timer
        |> Serialize.field .name Serialize.string
        |> Serialize.field .activity (Serialize.maybe serializeActivity)
        |> Serialize.field .category (Serialize.maybe serializeCategory)
        |> Serialize.finishRecord


serializePosix =
    Serialize.map Time.millisToPosix Time.posixToMillis Serialize.int


serializeTimerId =
    Serialize.map TimerSet.timerIdFromRaw TimerSet.timerIdToRaw Serialize.int


serializeActivity =
    Serialize.customType
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


serializeCategory =
    Serialize.customType
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
