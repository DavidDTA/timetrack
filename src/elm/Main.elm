port module Main exposing (main)

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
import Json.Decode
import Json.Encode
import List.Extra
import Material.Icons.Toggle
import Maybe.Extra
import Quantity
import Result.Extra
import Task
import Time
import TimeZone
import Transport
import Url


port save : String -> Cmd msg


port load : (String -> msg) -> Sub msg


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
    , persisted : Maybe Transport.TimerSet
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


type Error
    = TimeZoneError TimeZone.Error
    | LoadError Json.Decode.Error


type Msg
    = Error Error
    | Load String
    | Nop
    | UpdateNow Time.Posix
    | UpdateZone Time.Zone
    | AddTimer
    | RenameTimer { id : Transport.TimerId, name : String }
    | ToggleTimer Transport.TimerId


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { time = TimeUninitialized { now = Nothing, zone = Just (TimeZone.america__new_york ()) }
      , errors = []
      , persisted = Nothing
      }
    , TimeZone.getZone
        |> Task.attempt (Result.Extra.unpack (TimeZoneError >> Error) (Tuple.second >> UpdateZone))
    )


sub _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame UpdateNow
        , load Load
        ]


update msg model =
    let
        nop =
            ( model, Cmd.none )
    in
    case msg of
        Error error ->
            ( { model | errors = model.errors ++ [ error ] }, Cmd.none )

        Load serialized ->
            case Json.Decode.decodeString Transport.decodeTimerSet serialized of
                Ok timerSet ->
                    ( { model | persisted = Just timerSet }, Cmd.none )

                Err error ->
                    update (Error (LoadError error)) model

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
            updatePersisted Transport.addTimer model

        RenameTimer { id, name } ->
            updatePersisted (Transport.renameTimer id name) model

        ToggleTimer id ->
            case model.time of
                TimeUninitialized _ ->
                    nop

                TimeInitialized { now } ->
                    updatePersisted (Transport.toggleTimer id now) model


updatePersisted f ({ persisted } as model) =
    case persisted of
        Nothing ->
            ( model, Cmd.none )

        Just timerSet ->
            let
                updatedTimerSet =
                    f timerSet
            in
            ( { model
                | persisted = Just updatedTimerSet
              }
            , save (Json.Encode.encode 0 (Transport.encodeTimerSet updatedTimerSet))
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
        |> List.map
            (\error ->
                case error of
                    TimeZoneError tzerror ->
                        "Timezone error: "
                            ++ (case tzerror of
                                    TimeZone.NoZoneName ->
                                        "No zone name!"

                                    TimeZone.NoDataForZoneName zonename ->
                                        "No data for zone " ++ zonename ++ "!"
                               )

                    LoadError loadError ->
                        "Load error: "
                            ++ Json.Decode.errorToString loadError
            )
        |> List.map (\error -> Html.Styled.div [] [ Html.Styled.text error ])


viewLoading =
    []


viewTimers { time, persisted } =
    case time of
        TimeUninitialized _ ->
            viewLoading

        TimeInitialized { now, zone } ->
            case persisted of
                Nothing ->
                    viewLoading

                Just timerSet ->
                    List.map (viewTimer now) (Transport.listTimers timerSet)
                        ++ [ Html.Styled.button [ Html.Styled.Events.onClick AddTimer ] [ Html.Styled.text "add" ] ]


viewTimer now ( id, { accumulated, name, started } ) =
    Html.Styled.div []
        [ Html.Styled.input
            [ Html.Styled.Attributes.placeholder "Unnamed Timer"
            , Html.Styled.Attributes.value (Maybe.withDefault "" name)
            , Html.Styled.Events.onInput (\updatedName -> RenameTimer { id = id, name = updatedName })
            ]
            []
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
            [ Html.Styled.Events.onClick (ToggleTimer id)
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
