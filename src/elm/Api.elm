module Api exposing (Request(..), Response(..), Update(..), endpoint)

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



-- Authentication is a strong word here. For debugging purposes, we simply trust the client is who they say they are.


type alias RequestWithAuthentication =
    { usernameByFiat : String, request : Request }


type Request
    = Get
    | Update (List Update)


type Response
    = Value TimerSet.TimerSet


type Update
    = TimersAddAndStart Time.Posix
    | TimersClear
    | TimersRename TimerSet.TimerId String
    | TimersSetActivity TimerSet.TimerId (Maybe TimerSet.Activity)
    | TimersSetCategory TimerSet.TimerId (Maybe TimerSet.Category)
    | TimersSetActive (Maybe TimerSet.TimerId) Time.Posix


endpoint =
    Functions.codecEndpoint "/-/api" serializeRequestWithAuthentication serializeResponse


serializeRequestWithAuthentication =
    Serialize.record RequestWithAuthentication
        |> Serialize.field .usernameByFiat Serialize.string
        |> Serialize.field .request serializeRequest
        |> Serialize.finishRecord


serializeRequest =
    Serialize.customType
        (\getEncoder updateEncoder value ->
            case value of
                Get ->
                    getEncoder

                Update updates ->
                    updateEncoder updates
        )
        |> Serialize.variant0 Get
        |> Serialize.variant1 Update (Serialize.list serializeUpdate)
        |> Serialize.finishCustomType


serializeResponse =
    Serialize.customType
        (\valueEncoder value ->
            case value of
                Value timerSet ->
                    valueEncoder timerSet
        )
        |> Serialize.variant1 Value serializeTimerSet
        |> Serialize.finishCustomType


serializeUpdate =
    Serialize.customType
        (\timersAddAndStartEncoder timersClearEncoder timersRenameEncoder timersSetActivityEncoder timersSetCategoryEncoder timersSetActiveEncoder value ->
            case value of
                TimersAddAndStart posix ->
                    timersAddAndStartEncoder posix

                TimersClear ->
                    timersClearEncoder

                TimersRename timerId name ->
                    timersRenameEncoder timerId name

                TimersSetActivity timerId activity ->
                    timersSetActivityEncoder timerId activity

                TimersSetCategory timerId category ->
                    timersSetCategoryEncoder timerId category

                TimersSetActive timerId posix ->
                    timersSetActiveEncoder timerId posix
        )
        |> Serialize.variant1 TimersAddAndStart serializePosix
        |> Serialize.variant0 TimersClear
        |> Serialize.variant2 TimersRename serializeTimerId Serialize.string
        |> Serialize.variant2 TimersSetActivity serializeTimerId (Serialize.maybe serializeActivity)
        |> Serialize.variant2 TimersSetCategory serializeTimerId (Serialize.maybe serializeCategory)
        |> Serialize.variant2 TimersSetActive (Serialize.maybe serializeTimerId) serializePosix
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
