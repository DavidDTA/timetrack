module Timeline exposing (Timeline, at, empty, fold, fromList, set, toList)

import Duration
import Quantity
import SortedArray
import Time


type Timeline a
    = Timeline (SortedArray.SortedArray Int (Maybe a))


empty : Timeline a
empty =
    Timeline SortedArray.empty


fromList : List ( Time.Posix, Maybe a ) -> Timeline a
fromList list =
    List.foldl (\( k, v ) -> set v k Nothing) empty list


toList : Timeline a -> List ( Time.Posix, Maybe a )
toList (Timeline sortedArray) =
    SortedArray.toList sortedArray
        |> List.map (Tuple.mapFirst Time.millisToPosix)


set : Maybe a -> Time.Posix -> Maybe Time.Posix -> Timeline a -> Timeline a
set value startInclusive maybeEndExclusive (Timeline sortedArray) =
    let
        startInclusiveMillis =
            Time.posixToMillis startInclusive

        prefix =
            sortedArray
                |> SortedArray.slice Nothing (Just startInclusiveMillis)

        addValue =
            case SortedArray.before startInclusiveMillis sortedArray of
                Nothing ->
                    True

                Just ( _, valueBefore ) ->
                    valueBefore /= value

        prefixThroughValue =
            if addValue then
                SortedArray.insert startInclusiveMillis value prefix

            else
                prefix
    in
    Timeline
        (case maybeEndExclusive of
            Nothing ->
                prefixThroughValue

            Just endExclusive ->
                let
                    endExclusiveMillis =
                        Time.posixToMillis endExclusive

                    maybeRestartValue =
                        case SortedArray.before (endExclusiveMillis + 1) sortedArray of
                            Nothing ->
                                Just Nothing

                            Just ( _, valueBeforeSuffix ) ->
                                if valueBeforeSuffix == value then
                                    Nothing

                                else
                                    Just valueBeforeSuffix
                in
                if endExclusiveMillis > startInclusiveMillis then
                    prefixThroughValue
                        |> (case maybeRestartValue of
                                Nothing ->
                                    identity

                                Just restartValue ->
                                    SortedArray.insert endExclusiveMillis restartValue
                           )
                        |> SortedArray.merge (SortedArray.slice (Just endExclusiveMillis) Nothing sortedArray)

                else
                    sortedArray
        )


at : Time.Posix -> Timeline a -> Maybe a
at posix (Timeline sortedArray) =
    let
        posixMillis =
            Time.posixToMillis posix

        exact =
            SortedArray.at posixMillis sortedArray

        before =
            SortedArray.before (Time.posixToMillis posix) sortedArray
    in
    case exact of
        Just value ->
            value

        Nothing ->
            case before of
                Just ( _, value ) ->
                    value

                Nothing ->
                    Nothing


fold : acc -> (Maybe a -> Time.Posix -> Duration.Duration -> acc -> acc) -> Time.Posix -> Time.Posix -> Timeline a -> acc
fold initAcc func startInclusive endExclusive (Timeline sortedArray) =
    let
        startInclusiveMillis =
            Time.posixToMillis startInclusive

        endExclusiveMillis =
            Time.posixToMillis endExclusive

        startingValue =
            case SortedArray.before startInclusiveMillis sortedArray of
                Nothing ->
                    Nothing

                Just ( _, value ) ->
                    value

        sliced =
            SortedArray.slice (Just startInclusiveMillis) (Just endExclusiveMillis) sortedArray

        foldResult =
            SortedArray.foldl
                (\k v { acc, previous } ->
                    let
                        kPosix =
                            Time.millisToPosix k
                    in
                    { acc =
                        func previous.value previous.start (Duration.from previous.start kPosix) acc
                    , previous =
                        { start = kPosix
                        , value = v
                        }
                    }
                )
                { acc = initAcc
                , previous =
                    { start = startInclusive
                    , value = startingValue
                    }
                }
                sliced
    in
    func
        foldResult.previous.value
        foldResult.previous.start
        (Duration.from foldResult.previous.start endExclusive)
        foldResult.acc
