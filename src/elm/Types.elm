module Types exposing (Timer)

import Duration
import Time


type alias Timer =
    { accumulated : Duration.Duration
    , started : Maybe Time.Posix
    }
