module Main exposing (main)

import Browser
import Browser.Navigation
import Date
import Html
import Html.Attributes
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
        Html.node "style" [] [ Html.text staticCss ] :: viewBody model
    }


staticCss =
    """
@import url('https://fonts.googleapis.com/css?family=Nunito');
@import url('https://fonts.googleapis.com/css?family=Material+Icons');
@import url('https://fonts.googleapis.com/css?family=Material+Icons+Outlined');
@import url('https://fonts.googleapis.com/css?family=Material+Icons+Round');
@import url('https://fonts.googleapis.com/css?family=Material+Icons+Two+Tone');
@import url('https://fonts.googleapis.com/css?family=Material+Icons+Sharp');
html, body {
  width: 100%;
  height: 100%;
}
* {
  margin: 0;
  padding: 0;
  overflow: hidden;
  font-family: 'Nunito', sans-serif;
}
"""


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
    [ Html.div
        [ Html.Attributes.style "width" "100%"
        , Html.Attributes.style "height" "100%"
        , Html.Attributes.style "overflow-x" "scroll"
        ]
        [ Html.table
            [ Html.Attributes.style "table-layout" "fixed"
            , Html.Attributes.style "width" "0"
            ]
            (Html.tr
                []
                (Html.td
                    [ Html.Attributes.style "width" "8em"
                    ]
                    []
                    :: List.map
                        (\day ->
                            Html.td
                                [ Html.Attributes.style "width" "2em"
                                , Html.Attributes.style "font-variant" "small-caps"
                                , Html.Attributes.style "white-space" "pre"
                                , Html.Attributes.style "padding" "8px"
                                , Html.Attributes.style "text-align" "center"
                                ]
                                [ Html.text
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
                        Html.tr []
                            [ Html.td
                                [ Html.Attributes.style "padding" "8px"
                                , Html.Attributes.style "white-space" "nowrap"
                                , Html.Attributes.style "text-overflow" "ellipsis"
                                ]
                                [ Html.i [ Html.Attributes.class "material-icons" ] [ Html.text "star" ]
                                , Html.text row
                                ]
                            ]
                    )
                    rows
            )
        ]
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
