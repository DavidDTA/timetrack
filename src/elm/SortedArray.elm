module SortedArray exposing (SortedArray, before, empty, foldl, fromList, insert, isEmpty, length, map, remove, slice)

import Array
import Array.Extra
import Dict


type SortedArray k v
    = SortedArray (Array.Array ( k, v ))


type FindStrategy
    = LowerInclusive
    | LowerExclusive
    | Exact
    | HigherInclusive
    | HigherExclusive


type FindResult k a
    = BeforeIndex Int
    | AtIndex Int k a


empty : SortedArray k v
empty =
    SortedArray Array.empty


fromList : List ( comparable, v ) -> SortedArray comparable v
fromList list =
    SortedArray (Array.fromList (Dict.toList (Dict.fromList list)))


isEmpty : SortedArray k v -> Bool
isEmpty (SortedArray array) =
    Array.isEmpty array


length : SortedArray k v -> Int
length (SortedArray array) =
    Array.length array


insert : comparable -> v -> SortedArray comparable v -> SortedArray comparable v
insert key value (SortedArray array) =
    SortedArray
        (case find key array of
            BeforeIndex index ->
                Array.Extra.insertAt index ( key, value ) array

            AtIndex index _ _ ->
                Array.set index ( key, value ) array
        )


remove : comparable -> SortedArray comparable v -> SortedArray comparable v
remove key (SortedArray array) =
    SortedArray
        (case find key array of
            BeforeIndex _ ->
                array

            AtIndex index _ _ ->
                Array.Extra.removeAt index array
        )


slice : comparable -> comparable -> SortedArray comparable v -> SortedArray comparable v
slice startInclusive endExclusive (SortedArray array) =
    let
        startIndex =
            find startInclusive array

        endIndex =
            find endExclusive array

        startIndexInt =
            case startIndex of
                AtIndex index _ _ ->
                    index

                BeforeIndex index ->
                    index

        endIndexInt =
            case endIndex of
                AtIndex index _ _ ->
                    index

                BeforeIndex index ->
                    index
    in
    SortedArray (Array.slice startIndexInt endIndexInt array)


before : comparable -> SortedArray comparable v -> Maybe ( comparable, v )
before k (SortedArray array) =
    case find k array of
        AtIndex index foundKey foundValue ->
            Just ( foundKey, foundValue )

        BeforeIndex 0 ->
            Nothing

        BeforeIndex index ->
            Array.get (index - 1) array


map : (k -> v -> v2) -> SortedArray k v -> SortedArray k v2
map mapper (SortedArray array) =
    SortedArray (Array.map (\( key, value ) -> ( key, mapper key value )) array)


foldl : (k -> v -> b -> b) -> b -> SortedArray k v -> b
foldl f initAcc (SortedArray array) =
    Array.foldl (\( k, v ) b -> f k v b) initAcc array


find : comparable -> Array.Array ( comparable, v ) -> FindResult comparable v
find k array =
    findBetween k 0 (Array.length array) array


findBetween : comparable -> Int -> Int -> Array.Array ( comparable, v ) -> FindResult comparable v
findBetween key lowInclusive highExclusive array =
    if lowInclusive >= highExclusive then
        BeforeIndex 0

    else
        let
            test =
                (lowInclusive + highExclusive) // 2
        in
        case Array.get test array of
            Nothing ->
                -- We should never get here
                BeforeIndex 0

            Just ( foundKey, foundValue ) ->
                case compare key foundKey of
                    LT ->
                        if lowInclusive == test then
                            BeforeIndex test

                        else
                            findBetween key lowInclusive test array

                    EQ ->
                        AtIndex test foundKey foundValue

                    GT ->
                        let
                            newLowInclusive =
                                test + 1
                        in
                        if newLowInclusive == highExclusive then
                            BeforeIndex newLowInclusive

                        else
                            findBetween key newLowInclusive highExclusive array
