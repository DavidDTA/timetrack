module IncrementId exposing (increment, zero)

import Basics.Extra


zero =
    []


increment x =
    case x of
        [] ->
            [ Basics.Extra.minSafeInteger ]

        head :: tail ->
            if head < Basics.Extra.maxSafeInteger then
                head + 1 :: tail

            else
                Basics.Extra.minSafeInteger :: increment tail
