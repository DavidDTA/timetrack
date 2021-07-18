module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation
import Color
import Css
import Css.Global
import Date
import Duration
import Html.Styled
import Html.Styled.Attributes
import Html.Styled.Events
import List.Extra
import Material.Icons.Toggle
import Maybe.Extra
import Quantity
import Result.Extra
import Task
import Time
import TimeZone
import Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = sub
        , onUrlRequest = always Nop
        , onUrlChange = always Nop
        }


type alias Model =
    { time : TimeModel
    , errors : List Error
    , timers : List Timer
    }


type TimeModel
    = TimeUninitialized
        { now : Maybe Time.Posix
        , zone : Maybe Time.Zone
        }
    | TimeInitialized
        { now : Time.Posix
        , zone : Time.Zone
        }


type alias Timer =
    { accumulated : Duration.Duration
    , started : Maybe Time.Posix
    }


type Error
    = TimeZone TimeZone.Error


type Msg
    = Error Error
    | Nop
    | UpdateNow Time.Posix
    | UpdateZone Time.Zone
    | AddTimer
    | ToggleTimer Int


init : String -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { time = TimeUninitialized { now = Nothing, zone = Just (TimeZone.america__new_york ()) }
      , errors = []
      , timers = []
      }
    , TimeZone.getZone
        |> Task.attempt (Result.Extra.unpack (TimeZone >> Error) (Tuple.second >> UpdateZone))
    )


sub _ =
    Browser.Events.onAnimationFrame UpdateNow


update msg model =
    let
        nop =
            ( model, Cmd.none )
    in
    case msg of
        Error error ->
            ( { model | errors = model.errors ++ [ error ] }, Cmd.none )

        Nop ->
            nop

        UpdateNow posix ->
            ( { model
                | time =
                    case model.time of
                        TimeUninitialized uninitialized ->
                            maybeInitialize { uninitialized | now = Just posix }

                        TimeInitialized initialized ->
                            TimeInitialized { initialized | now = posix }
              }
            , Cmd.none
            )

        UpdateZone zone ->
            ( { model
                | time =
                    case model.time of
                        TimeUninitialized uninitialized ->
                            maybeInitialize { uninitialized | zone = Just zone }

                        TimeInitialized initialized ->
                            TimeInitialized { initialized | zone = zone }
              }
            , Cmd.none
            )

        AddTimer ->
            ( { model
                | timers = model.timers ++ [ { accumulated = Quantity.zero, started = Nothing } ]
              }
            , Cmd.none
            )

        ToggleTimer index ->
            ( { model
                | timers =
                    model.timers
                        |> List.Extra.updateAt index
                            (\item ->
                                case model.time of
                                    TimeUninitialized _ ->
                                        item

                                    TimeInitialized { now } ->
                                        case item.started of
                                            Just started ->
                                                { item
                                                    | accumulated =
                                                        item.accumulated
                                                            |> Quantity.plus (Quantity.max Quantity.zero (Duration.from started now))
                                                    , started = Nothing
                                                }

                                            Nothing ->
                                                { item
                                                    | started = Just now
                                                }
                            )
              }
            , Cmd.none
            )


maybeInitialize { now, zone } =
    case ( now, zone ) of
        ( Just justNow, Just justZone ) ->
            TimeInitialized { now = justNow, zone = justZone }

        _ ->
            TimeUninitialized { now = now, zone = zone }


view model =
    { title = "Timetrack"
    , body =
        [ Html.Styled.toUnstyled
            (Html.Styled.div
                []
                ([ Html.Styled.node "link"
                    [ Html.Styled.Attributes.href "https://fonts.googleapis.com/css?family=Nunito"
                    , Html.Styled.Attributes.rel "stylesheet"
                    , Html.Styled.Attributes.type_ "text/css"
                    ]
                    []
                 , globalCss
                 ]
                    ++ viewBody model
                )
            )
        ]
    }


globalCss =
    Css.Global.global
        [ Css.Global.everything
            [ Css.margin Css.zero
            , Css.padding Css.zero
            , Css.property "overscroll-behavior" "none"
            , Css.fontFamilies [ Css.qt "Nunito", Css.sansSerif.value ]
            ]
        ]


viewBody model =
    viewErrors model ++ viewTimers model


viewErrors { errors } =
    errors
        |> List.map (\error -> Html.Styled.div [] [ Html.Styled.text "Error!" ])


viewTimers { time, timers } =
    case time of
        TimeUninitialized _ ->
            []

        TimeInitialized { now, zone } ->
            List.indexedMap (viewTimer now) timers
                ++ [ Html.Styled.button [ Html.Styled.Events.onClick AddTimer ] [ Html.Styled.text "add" ] ]


viewTimer now index { accumulated, started } =
    Html.Styled.div []
        [ Html.Styled.text "Timer"
        , Quantity.plus accumulated
            (Maybe.map
                (\posix ->
                    Duration.from posix now
                        |> Quantity.max Quantity.zero
                )
                started
                |> Maybe.withDefault Quantity.zero
            )
            |> viewDuration
        , Html.Styled.button
            [ Html.Styled.Events.onClick (ToggleTimer index)
            ]
            [ Html.Styled.text
                (if Maybe.Extra.isJust started then
                    "Stop"

                 else
                    "Start"
                )
            ]
        ]


viewDuration duration =
    let
        hours =
            duration |> Duration.inHours |> floor

        hoursQuantity =
            Duration.hours (toFloat hours)

        minutes =
            duration |> Quantity.minus hoursQuantity |> Duration.inMinutes |> floor

        minutesQuantity =
            hoursQuantity |> Quantity.plus (Duration.minutes (toFloat minutes))

        seconds =
            duration |> Quantity.minus minutesQuantity |> Duration.inSeconds |> floor

        secondsQuantity =
            minutesQuantity |> Quantity.plus (Duration.seconds (toFloat seconds))

        hundredths =
            duration |> Quantity.minus secondsQuantity |> Duration.inMilliseconds |> (\x -> x / 10) |> floor
    in
    Html.Styled.span []
        [ Html.Styled.text (String.fromInt hours ++ ":" ++ pad minutes ++ ":" ++ pad seconds)
        , Html.Styled.small [] [ Html.Styled.text ("." ++ pad hundredths) ]
        ]


pad x =
    if x < 0 || x >= 100 then
        "??"

    else if x < 10 then
        "0" ++ String.fromInt x

    else
        String.fromInt x
