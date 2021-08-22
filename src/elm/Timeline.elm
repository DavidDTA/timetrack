module Timeline exposing (Timeline)

import Duration
import Quantity
import SortedArray
import Time


type Timeline a
    = Timeline (SortedArray.SortedArray Int (Maybe a))


consIfNonZero startInclusive endExclusive value list =
    let
        duration =
            Duration.from startInclusive endExclusive
    in
    if duration == Quantity.zero then
        list

    else
        ( value, duration ) :: list


durations : Time.Posix -> Time.Posix -> Timeline a -> List ( Maybe a, Duration.Duration )
durations startInclusive endExclusive (Timeline sortedArray) =
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
            SortedArray.slice startInclusiveMillis endExclusiveMillis sortedArray

        foldResult =
            SortedArray.foldl
                (\k v { list, previous } ->
                    let
                        kPosix =
                            Time.millisToPosix k
                    in
                    { list = consIfNonZero previous.start kPosix previous.value list
                    , previous =
                        { start = kPosix
                        , value = v
                        }
                    }
                )
                { list = []
                , previous =
                    { start = startInclusive
                    , value = startingValue
                    }
                }
                sliced
    in
    consIfNonZero foldResult.previous.start endExclusive foldResult.previous.value foldResult.list
        |> List.reverse
