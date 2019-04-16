module TimeChunk exposing (TimeChunk(..), endExclusive, fromPosix, startInclusive)

import Time


type TimeChunk
    = TimeChunk Int


fromPosix posix =
    let
        posixMillis =
            Time.posixToMillis posix
    in
    TimeChunk
        (if posixMillis >= 0 then
            posixMillis // msPerChunk

         else
            -((msPerChunk - posixMillis - 1) // msPerChunk)
        )


startInclusive (TimeChunk n) =
    Time.millisToPosix (n * msPerChunk)


endExclusive (TimeChunk n) =
    startInclusive (TimeChunk (n + 1))


msPerChunk =
    5 * 60 * 1000
