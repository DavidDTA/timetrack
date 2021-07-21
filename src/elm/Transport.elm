module Transport exposing (Global, Timer, TimerId, addTimer, decodeGlobal, encodeGlobal, listTimers, renameTimer, toggleTimer)

import Duration
import Json.Decode
import Json.Decode.Pipeline
import Json.Encode
import List.Extra
import Quantity
import Time


type Global
    = Global
        { timers : List Timer
        }


type alias Timer =
    { accumulated : Duration.Duration
    , name : Maybe String
    , started : Maybe Time.Posix
    }


type TimerId
    = TimerId Int


addTimer : Global -> Global
addTimer (Global global) =
    Global
        { global
            | timers =
                global.timers
                    ++ [ { accumulated = Quantity.zero
                         , name = Nothing
                         , started = Nothing
                         }
                       ]
        }


listTimers : Global -> List ( TimerId, Timer )
listTimers (Global { timers }) =
    List.indexedMap (\index timer -> ( TimerId index, timer )) timers


renameTimer : TimerId -> String -> Global -> Global
renameTimer (TimerId id) name (Global global) =
    Global
        { global
            | timers =
                global.timers
                    |> List.Extra.updateAt id
                        (\item ->
                            let
                                trimmed =
                                    String.trim name
                            in
                            { item
                                | name =
                                    case trimmed of
                                        "" ->
                                            Nothing

                                        _ ->
                                            Just trimmed
                            }
                        )
        }


toggleTimer : TimerId -> Time.Posix -> Global -> Global
toggleTimer (TimerId id) now (Global global) =
    Global
        { global
            | timers =
                global.timers
                    |> List.indexedMap
                        (\mapIndex item ->
                            case item.started of
                                Just started ->
                                    { item
                                        | accumulated =
                                            item.accumulated
                                                |> Quantity.plus (Quantity.max Quantity.zero (Duration.from started now))
                                        , started = Nothing
                                    }

                                Nothing ->
                                    if id == mapIndex then
                                        { item
                                            | started = Just now
                                        }

                                    else
                                        item
                        )
        }


decodeGlobal : Json.Decode.Decoder Global
decodeGlobal =
    Json.Decode.succeed (\timers -> Global { timers = timers })
        |> Json.Decode.Pipeline.optional "timers"
            (Json.Decode.list decodeTimer)
            []


encodeGlobal : Global -> Json.Encode.Value
encodeGlobal (Global global) =
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
