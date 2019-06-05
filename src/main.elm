module Main exposing (main)

import Browser
import Browser.Navigation
import Date
import Time
import TimeZone
import Url
import View


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
    }


type Msg
    = Nop


init : Int -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init now _ _ =
    ( { now = Time.millisToPosix now }, Cmd.none )


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
        View.toHtml (viewBody model)
    }


viewBody model =
    View.verticalScroll
        [ View.centered (View.text (Date.toIsoString (Date.fromPosix timezone model.now)))
        ]


timezone =
    TimeZone.america__new_york ()
