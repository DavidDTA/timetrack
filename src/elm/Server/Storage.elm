module Server.Storage exposing (getTimerSet, updateTimerSet)

import Firestore
import Firestore.Codec
import Task
import Time
import Timeline
import TimerSet
import Version


timerSetCollection =
    "timerSets"


getTimerSet firestore username =
    firestore
        |> Firestore.root
        |> Firestore.collection timerSetCollection
        |> Firestore.document (pathSafe username)
        |> Firestore.get (Firestore.Codec.asDecoder timerSetCodec)
        |> Task.map .fields
        |> Task.onError
            (\error ->
                case error of
                    Firestore.Response { code } ->
                        if code == 404 then
                            Task.succeed { version = Version.zero, value = TimerSet.empty }

                        else
                            Task.fail error

                    _ ->
                        Task.fail error
            )


updateTimerSet firestore username update =
    getTimerSet firestore username
        |> Task.andThen
            (\{ version, value } ->
                firestore
                    |> Firestore.root
                    |> Firestore.collection timerSetCollection
                    |> Firestore.document (pathSafe username)
                    |> Firestore.upsert
                        (Firestore.Codec.asDecoder timerSetCodec)
                        (Firestore.Codec.asEncoder timerSetCodec { version = Version.increment version, value = update value })
                    |> Task.map .fields
            )


pathSafe segment =
    segment
        |> String.replace "/" "-"
        |> String.replace "." "-"
        |> String.replace "_" "-"
        |> (\string ->
                if string == "" then
                    "-"

                else
                    string
           )


type alias TimerSetIntermediate =
    { timerNames : List String
    , timerActivities : List (Maybe TimerSet.Activity)
    , timerCategories : List (Maybe TimerSet.Category)
    , timelineTimestamps : List Time.Posix
    , timelineTimerIds : List (Maybe TimerSet.TimerId)
    }


timelineFromIntermediate timerNames timerActivities timerCategories timelineTimestamps timelineTimerIds version =
    { version = version
    , value =
        TimerSet.create
            (List.map3 TimerSet.Timer timerNames timerActivities timerCategories)
            (List.map2 Tuple.pair timelineTimestamps timelineTimerIds |> Timeline.fromList)
    }


timerSetCodec =
    let
        timers { value } =
            value
                |> TimerSet.listTimerIds
                |> List.filterMap (\id -> TimerSet.get id value)

        timeline { value } =
            TimerSet.history value
                |> Timeline.toList

        listField fieldName listGetter fieldGetter codec =
            Firestore.Codec.optional fieldName (listGetter >> List.map fieldGetter) (Firestore.Codec.list codec) []
    in
    Firestore.Codec.document timelineFromIntermediate
        |> listField "timerNames" timers .name Firestore.Codec.string
        |> listField "timerActivities" timers .activity (Firestore.Codec.maybe activityCodec)
        |> listField "timerCategories" timers .category (Firestore.Codec.maybe categoryCodec)
        |> listField "timelineTimestamps" timeline Tuple.first Firestore.Codec.timestamp
        |> listField "timelineTimerIds" timeline Tuple.second (Firestore.Codec.maybe timerIdCodec)
        |> Firestore.Codec.optional "version" .version versionCodec Version.zero
        |> Firestore.Codec.build


versionCodec =
    Firestore.Codec.map Version.fromRaw Version.toRaw Firestore.Codec.int


timerIdCodec =
    Firestore.Codec.map TimerSet.timerIdFromRaw TimerSet.timerIdToRaw Firestore.Codec.int


activityCodec =
    Firestore.Codec.int
        |> Firestore.Codec.andThen
            (\int ->
                case int of
                    0 ->
                        Firestore.Codec.succeed TimerSet.Active

                    1 ->
                        Firestore.Codec.succeed TimerSet.Reactive

                    2 ->
                        Firestore.Codec.succeed TimerSet.Proactive

                    _ ->
                        Firestore.Codec.fail "invalid activity"
            )
            (\activity ->
                case activity of
                    TimerSet.Active ->
                        0

                    TimerSet.Reactive ->
                        1

                    TimerSet.Proactive ->
                        2
            )


categoryCodec =
    Firestore.Codec.int
        |> Firestore.Codec.andThen
            (\int ->
                case int of
                    0 ->
                        Firestore.Codec.succeed TimerSet.Operational

                    1 ->
                        Firestore.Codec.succeed TimerSet.Helpful

                    2 ->
                        Firestore.Codec.succeed TimerSet.Productive

                    _ ->
                        Firestore.Codec.fail "invalid codec"
            )
            (\category ->
                case category of
                    TimerSet.Operational ->
                        0

                    TimerSet.Helpful ->
                        1

                    TimerSet.Productive ->
                        2
            )
