module Main exposing (main)

import Browser
import Browser.Navigation
import Css
import Css.Global
import Date
import Html.Styled
import Html.Styled.Attributes
import Html.Styled.Events
import Json.Decode
import Time
import TimeZone
import Url


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = always Sub.none
        , onUrlRequest = always Nop
        , onUrlChange = always Nop
        }


type alias Model =
    { now : Time.Posix
    , zone : Time.Zone
    }


type Msg
    = Nop


init : Int -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init now _ _ =
    ( { now = Time.millisToPosix now, zone = TimeZone.america__new_york () }, Cmd.none )


update msg model =
    let
        nop =
            ( model, Cmd.none )
    in
    case msg of
        Nop ->
            nop


view model =
    { title = "Timetrack"
    , body =
        [ Html.Styled.toUnstyled
            (Html.Styled.div
                []
                [ fontStylesheet "https://fonts.googleapis.com/css?family=Nunito"
                , fontStylesheet "https://fonts.googleapis.com/css?family=Material+Icons"
                , fontStylesheet "https://fonts.googleapis.com/css?family=Material+Icons+Outlined"
                , fontStylesheet "https://fonts.googleapis.com/css?family=Material+Icons+Round"
                , fontStylesheet "https://fonts.googleapis.com/css?family=Material+Icons+Two+Tone"
                , fontStylesheet "https://fonts.googleapis.com/css?family=Material+Icons+Sharp"
                , globalCss
                , viewBody model
                ]
            )
        ]
    }


fontStylesheet url =
    Html.Styled.node "link"
        [ Html.Styled.Attributes.href url
        , Html.Styled.Attributes.rel "stylesheet"
        , Html.Styled.Attributes.type_ "text/css"
        ]
        []


globalCss =
    Css.Global.global
        [ Css.Global.each
            [ Css.Global.html, Css.Global.body ]
            [ Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            ]
        , Css.Global.everything
            [ Css.margin Css.zero
            , Css.padding Css.zero
            , Css.overflow Css.hidden
            , Css.fontFamilies [ Css.qt "Nunito", Css.sansSerif.value ]
            ]
        ]


viewBody model =
    let
        today =
            Date.fromPosix model.zone model.now

        days =
            List.range 0 5
                |> List.map (\x -> Date.add Date.Days -x today)

        rows =
            [ "Work catchup AM"
            , "Work catchup PM"
            , "Chores catchup"
            , "Russian"
            , "Handstand"
            , "Stretch"
            , "Read"
            , "Project"
            , "Chores progress"
            ]
    in
    Html.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.width (Css.pct 100)
            , Css.height (Css.pct 100)
            , Css.overflowX Css.scroll
            ]
        ]
        [ Html.Styled.table
            [ Html.Styled.Attributes.css
                [ Css.tableLayout Css.fixed
                , Css.width Css.zero
                ]
            ]
            (Html.Styled.tr
                []
                (Html.Styled.td
                    [ Html.Styled.Attributes.css [ Css.width (Css.em 8) ]
                    ]
                    []
                    :: List.map
                        (\day ->
                            Html.Styled.td
                                [ Html.Styled.Attributes.css
                                    [ Css.width (Css.em 2)
                                    , Css.fontVariant Css.smallCaps
                                    , Css.whiteSpace Css.pre
                                    , Css.padding (Css.px 8)
                                    , Css.textAlign Css.center
                                    ]
                                ]
                                [ Html.Styled.text
                                    (labelForWeekday (Date.weekday day)
                                        ++ "\n"
                                        ++ String.fromInt (Date.day day)
                                    )
                                ]
                        )
                        days
                )
                :: List.map
                    (\row ->
                        Html.Styled.tr []
                            [ Html.Styled.td
                                [ Html.Styled.Attributes.css
                                    [ Css.padding (Css.px 8)
                                    , Css.whiteSpace Css.noWrap
                                    , Css.textOverflow Css.ellipsis
                                    ]
                                ]
                                [ Html.Styled.i [ Html.Styled.Attributes.class "material-icons" ] [ Html.Styled.text "star" ]
                                , Html.Styled.text row
                                ]
                            ]
                    )
                    rows
            )
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
