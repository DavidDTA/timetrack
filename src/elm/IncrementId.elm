module IncrementId exposing (Id, increment, zero)

import Basics.Extra


type alias Id =
    List Int


zero : Id
zero =
    []


increment : Id -> Id
increment x =
    case x of
        [] ->
            [ Basics.Extra.minSafeInteger ]

        head :: tail ->
            if head < Basics.Extra.maxSafeInteger then
                head + 1 :: tail

            else
                Basics.Extra.minSafeInteger :: increment tail
