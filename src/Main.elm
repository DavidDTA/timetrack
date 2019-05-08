module Main exposing (main)

import Browser
import Browser.Navigation
import Date
import Task
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


type Model
    = Loading
    | Normal
        { date : Date.Date
        }


type Msg
    = Nop
    | Date Date.Date


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( Loading, Cmd.batch [ Task.perform Date Date.today ] )


update msg model =
    let
        nop =
            ( model, Cmd.none )
    in
    case msg of
        Nop ->
            nop

        Date date ->
            case model of
                Loading ->
                    ( Normal { date = date }, Cmd.none )

                Normal _ ->
                    nop


view model =
    { title = "Timetrack"
    , body =
        case model of
            Loading ->
                []

            Normal model_ ->
                View.toHtml (viewBody model_)
    }


viewBody model =
    View.verticalScroll
        [ View.centered (View.text (Date.toIsoString model.date))
        ]


timezone =
    TimeZone.asia__shanghai ()
