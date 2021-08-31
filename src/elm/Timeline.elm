module Timeline exposing (Timeline, at, duration, empty, fromList, set, toList)

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


duration : (Maybe a -> Bool) -> Time.Posix -> Time.Posix -> Timeline a -> Duration.Duration
duration filter startInclusive endExclusive (Timeline sortedArray) =
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

        addIfKey testKey durationToAdd =
            if filter testKey then
                Quantity.plus durationToAdd

            else
                identity

        sliced =
            SortedArray.slice (Just startInclusiveMillis) (Just endExclusiveMillis) sortedArray

        foldResult =
            SortedArray.foldl
                (\k v { quantity, previous } ->
                    let
                        kPosix =
                            Time.millisToPosix k
                    in
                    { quantity =
                        addIfKey previous.value (Duration.from previous.start kPosix) quantity
                    , previous =
                        { start = kPosix
                        , value = v
                        }
                    }
                )
                { quantity = Quantity.zero
                , previous =
                    { start = startInclusive
                    , value = startingValue
                    }
                }
                sliced
    in
    foldResult.quantity
        |> addIfKey foldResult.previous.value (Duration.from foldResult.previous.start endExclusive)
