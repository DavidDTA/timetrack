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

                    sliced =
                        SortedArray.slice (Just endExclusiveMillis) Nothing sortedArray

                    valueAtStartOfSuffix =
                        case SortedArray.at endExclusiveMillis sortedArray of
                            Just value_ ->
                                value_

                            Nothing ->
                                SortedArray.before endExclusiveMillis sortedArray
                                    |> Maybe.andThen Tuple.second

                    suffix =
                        if valueAtStartOfSuffix == value then
                            SortedArray.remove endExclusiveMillis sliced

                        else
                            SortedArray.insert endExclusiveMillis valueAtStartOfSuffix sliced
                in
                if endExclusiveMillis > startInclusiveMillis then
                    SortedArray.merge prefixThroughValue suffix

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
            before
                |> Maybe.andThen Tuple.second


fold : acc -> ({ value : Maybe a, start : Time.Posix, duration : Duration.Duration } -> acc -> acc) -> Time.Posix -> Time.Posix -> Timeline a -> acc
fold initAcc func startInclusive requestedEndExclusive (Timeline sortedArray) =
    let
        startInclusiveMillis =
            Time.posixToMillis startInclusive

        endExclusiveMillis =
            max startInclusiveMillis (Time.posixToMillis requestedEndExclusive)

        endExclusive =
            Time.millisToPosix endExclusiveMillis

        startingValue =
            SortedArray.before startInclusiveMillis sortedArray
                |> Maybe.andThen Tuple.second

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
                        if previous.start == kPosix then
                            acc

                        else
                            func { value = previous.value, start = previous.start, duration = Duration.from previous.start kPosix } acc
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
        { value = foldResult.previous.value
        , start = foldResult.previous.start
        , duration = Duration.from foldResult.previous.start endExclusive
        }
        foldResult.acc
