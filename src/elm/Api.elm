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
    = TimersAddAndStart Time.Posix
    | TimersClear
    | TimersRename TimerSet.TimerId String
    | TimersSetActive
        { timerId : Maybe TimerSet.TimerId
        , start : Time.Posix
        , end : Maybe Time.Posix
        }


applyUpdate apiUpdate timerSet =
    case apiUpdate of
        TimersAddAndStart timestamp ->
            let
                ( newTimerSet, newTimerId ) =
                    TimerSet.addTimer timerSet
            in
            TimerSet.setTimer (Just newTimerId) timestamp Nothing newTimerSet

        TimersClear ->
            TimerSet.reset timerSet

        TimersRename timerId name ->
            TimerSet.updateTimer timerId (\timer -> { timer | name = String.trim name }) timerSet

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
        (\timersAddAndStartEncoder timersClearEncoder timersRenameEncoder timersSetActiveEncoder value ->
            case value of
                TimersAddAndStart posix ->
                    timersAddAndStartEncoder posix

                TimersClear ->
                    timersClearEncoder

                TimersRename timerId name ->
                    timersRenameEncoder timerId name

                TimersSetActive params ->
                    timersSetActiveEncoder params
        )
        |> Serialize.variant1 TimersAddAndStart serializePosix
        |> Serialize.variant0 TimersClear
        |> Serialize.variant2 TimersRename serializeTimerId Serialize.string
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
        |> Serialize.finishRecord


serializePosix =
    Serialize.map Time.millisToPosix Time.posixToMillis Serialize.int


serializeTimerId =
    Serialize.map TimerSet.timerIdFromRaw TimerSet.timerIdToRaw Serialize.int
