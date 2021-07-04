module Main exposing (main)

import Browser
import Browser.Events
import Browser.Navigation
import Color
import Css
import Css.Global
import Date
import Html.Styled
import Html.Styled.Attributes
import Material.Icons.Toggle
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
    = TimeZone TimeZone.Error


type Msg
    = Error Error
    | Nop
    | UpdateNow Time.Posix
    | UpdateZone Time.Zone


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { time = TimeUninitialized { now = Nothing, zone = Just (TimeZone.america__new_york ()) }, errors = [] }
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
        [
        Css.Global.everything
            [ Css.margin Css.zero
            , Css.padding Css.zero
            , Css.property "overscroll-behavior" "none"
            , Css.fontFamilies [ Css.qt "Nunito", Css.sansSerif.value ]
            ]
        ]


viewBody { time } =
    case time of
        TimeUninitialized _ ->
            []

        TimeInitialized { now, zone } ->
            let
                today =
                    Date.fromPosix zone now

                days =
                    List.range 0 5
                        |> List.map (\x -> Date.add Date.Days -x today)

                rows =
                    [ "Row 1"
                    ]
            in
            [
            ]


labelForWeekday weekday =
    case weekday of
        Time.Mon ->
            "mon"

        Time.Tue ->
            "tue"

        Time.Wed ->
            "wed"

        Time.Thu ->
            "thu"

        Time.Fri ->
            "fri"

        Time.Sat ->
            "sat"

        Time.Sun ->
            "sun"
