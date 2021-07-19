module Transport exposing (..)

import Duration
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import Time


type alias Global =
    { timers : List Timer
    }


type alias Timer =
    { accumulated : Duration.Duration
    , name : Maybe String
    , started : Maybe Time.Posix
    }


decodeGlobal : Json.Decode.Decoder Global
decodeGlobal =
    Json.Decode.succeed Global
        |> Json.Decode.Pipeline.optional "timers"
            (Json.Decode.list decodeTimer)
            []


encodeGlobal : Global -> Json.Encode.Value
encodeGlobal global =
    Json.Encode.object
        [ ( "timers", Json.Encode.list encodeTimer global.timers )
        ]


decodeTimer : Json.Decode.Decoder Timer
decodeTimer =
    Json.Decode.succeed Timer
        |> Json.Decode.Pipeline.required "accumulated"
            (Json.Decode.float
                |> Json.Decode.map Duration.seconds
            )
        |> Json.Decode.Pipeline.optional "name"
            (Json.Decode.string
                |> Json.Decode.map Just
            )
            Nothing
        |> Json.Decode.Pipeline.required "started"
            (Json.Decode.int
                |> Json.Decode.map Time.millisToPosix
                |> Json.Decode.nullable
            )


encodeTimer : Timer -> Json.Encode.Value
encodeTimer timer =
    Json.Encode.object
        [ ( "accumulated"
          , timer.accumulated
                |> Duration.inSeconds
                |> Json.Encode.float
          )
        , ( "name"
          , timer.name
                |> Maybe.map Json.Encode.string
                |> Maybe.withDefault Json.Encode.null
          )
        , ( "started"
          , timer.started
                |> Maybe.map Time.posixToMillis
                |> Maybe.map Json.Encode.int
                |> Maybe.withDefault Json.Encode.null
          )
        ]
