port module Client exposing (main)

import Api
import Browser
import Browser.Events
import Browser.Navigation
import Color
import Css
import Css.Global
import Css.Transitions
import Date
import Duration
import Functions
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
import Serialize
import Task
import Time
import Time.Extra
import TimeZone
import Timeline
import TimerSet
import Url
import Version


port localStorageWrites : ( String, String ) -> Cmd msg


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
    , username : Username
    , errors : List Error
    , historySelectedDate : Maybe Date.Date
    , remote : Remote
    , pending : Pending Remote
    , clearConfirmation : ClearConfirmation
    , edit : Maybe Edit
    }


type alias Remote =
    { timerSet : Maybe { version : Version.Version, value : TimerSet.TimerSet } }


type Pending remote
    = PendingIdle
    | Pending { current : PendingCurrent, queue : List Api.Update, base : remote }


type PendingCurrent
    = PendingOutstanding (List Api.Update)
    | PendingError (List Api.Update)


type ClearConfirmation
    = ClearConfirmationHidden
    | ClearConfirmationShown


type TimeModel
    = TimeUninitialized
        { now : Maybe Time.Posix
        , zone : Maybe Time.Zone
        }
    | TimeInitialized
        { now : Time.Posix
        , zone : Time.Zone
        }


type Username
    = EditingUsername String
    | SelectedUsername String


type Edit
    = EditTimerName { timerId : TimerSet.TimerId, name : String }


type Error
    = TimeZoneError TimeZone.Error
    | ApiError Functions.SendError


type Msg
    = ApiResponse (Result Functions.SendError Api.Response)
    | ApiRetry
    | Nop
    | UpdateNow Time.Posix
    | UpdateZone Time.Zone
    | UpdateZoneError TimeZone.Error
    | AddTimer
    | ClearTimersInitiate
    | ClearTimersCancel
    | ClearTimersConfirm
    | HistoryIncrementDate { days : Int }
    | TimerEditCommit
    | TimerEditRename { timerId : TimerSet.TimerId, name : String }
    | TimerToggleActivity TimerSet.TimerId TimerSet.Activity
    | TimerToggleCategory TimerSet.TimerId TimerSet.Category
    | TimerToggleRunning TimerSet.TimerId
    | UsernameEdit String
    | UsernameSubmit


init : { localStorage : Json.Decode.Value } -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init { localStorage } _ _ =
    let
        username =
            Json.Decode.decodeValue (Json.Decode.field localStorageKeys.username Json.Decode.string) localStorage
                |> Result.map SelectedUsername
                |> Result.withDefault (EditingUsername "")
    in
    ( { time = TimeUninitialized { now = Nothing, zone = Just (TimeZone.america__new_york ()) }
      , username = username
      , errors = []
      , historySelectedDate = Nothing
      , remote = { timerSet = Nothing }
      , pending = PendingIdle
      , clearConfirmation = ClearConfirmationHidden
      , edit = Nothing
      }
    , Cmd.batch
        [ TimeZone.getZone
            |> Task.attempt (Result.Extra.unpack UpdateZoneError (Tuple.second >> UpdateZone))
        , case username of
            SelectedUsername raw ->
                fetchInitialState raw

            EditingUsername _ ->
                Cmd.none
        ]
    )


sub _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame UpdateNow
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        nop =
            ( model, Cmd.none )
    in
    case msg of
        ApiResponse result ->
            case result of
                Result.Err err ->
                    ( { model
                        | errors = ApiError err :: model.errors
                        , pending =
                            case model.pending of
                                PendingIdle ->
                                    PendingIdle

                                Pending { current, queue, base } ->
                                    Pending
                                        { current =
                                            case current of
                                                PendingOutstanding outstanding ->
                                                    PendingError outstanding

                                                PendingError outstanding ->
                                                    PendingError outstanding
                                        , queue = queue
                                        , base = base
                                        }
                      }
                    , Cmd.none
                    )

                Result.Ok response ->
                    case response of
                        Api.Value value ->
                            let
                                newQueue =
                                    case model.pending of
                                        PendingIdle ->
                                            []

                                        Pending { queue } ->
                                            List.reverse queue
                            in
                            { model | remote = { timerSet = Just value }, pending = PendingIdle }
                                |> enqueueAll newQueue

        ApiRetry ->
            case model.pending of
                PendingIdle ->
                    ( model, Cmd.none )

                Pending { current, queue, base } ->
                    case current of
                        PendingOutstanding _ ->
                            ( model, Cmd.none )

                        PendingError failed ->
                            let
                                ( model1, cmd1 ) =
                                    enqueueAll failed { model | remote = base, pending = PendingIdle }

                                ( model2, cmd2 ) =
                                    enqueueAll queue model1
                            in
                            ( model2, Cmd.batch [ cmd1, cmd2 ] )

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

        UpdateZoneError err ->
            ( { model | errors = TimeZoneError err :: model.errors }, Cmd.none )

        AddTimer ->
            case model.time of
                TimeUninitialized _ ->
                    nop

                TimeInitialized { now } ->
                    enqueue (Api.TimersAddAndStart now) model

        ClearTimersInitiate ->
            ( { model | clearConfirmation = ClearConfirmationShown }, Cmd.none )

        ClearTimersCancel ->
            ( { model | clearConfirmation = ClearConfirmationHidden }, Cmd.none )

        ClearTimersConfirm ->
            { model | clearConfirmation = ClearConfirmationHidden }
                |> enqueue Api.TimersClear

        HistoryIncrementDate { days } ->
            ( case model.time of
                TimeUninitialized _ ->
                    model

                TimeInitialized { now, zone } ->
                    let
                        today =
                            Date.fromPosix zone now

                        newDate =
                            model.historySelectedDate
                                |> Maybe.withDefault today
                                |> Date.add Date.Days days
                    in
                    { model
                        | historySelectedDate =
                            if newDate == today then
                                Nothing

                            else
                                Just newDate
                    }
            , Cmd.none
            )

        TimerEditCommit ->
            case model.edit of
                Nothing ->
                    nop

                Just (EditTimerName { timerId, name }) ->
                    { model | edit = Nothing }
                        |> enqueue (Api.TimersRename timerId name)

        TimerEditRename edit ->
            ( { model | edit = Just (EditTimerName edit) }, Cmd.none )

        TimerToggleActivity timerId activity ->
            case model.remote.timerSet of
                Just timerSet ->
                    let
                        currentActivity =
                            TimerSet.get timerId timerSet.value
                                |> Maybe.andThen .activity

                        newActivity =
                            if Just activity == currentActivity then
                                Nothing

                            else
                                Just activity
                    in
                    enqueue (Api.TimersSetActivity timerId newActivity) model

                Nothing ->
                    nop

        TimerToggleCategory timerId category ->
            case model.remote.timerSet of
                Just timerSet ->
                    let
                        currentCategory =
                            TimerSet.get timerId timerSet.value
                                |> Maybe.andThen .category

                        newCategory =
                            if Just category == currentCategory then
                                Nothing

                            else
                                Just category
                    in
                    enqueue (Api.TimersSetCategory timerId newCategory) model

                Nothing ->
                    nop

        TimerToggleRunning id ->
            case ( model.time, model.remote.timerSet ) of
                ( TimeInitialized { now }, Just timerSet ) ->
                    let
                        currentId =
                            Timeline.at now (TimerSet.history timerSet.value)

                        newId =
                            if Just id == currentId then
                                Nothing

                            else
                                Just id
                    in
                    enqueue (Api.TimersSetActive newId now) model

                ( _, _ ) ->
                    nop

        UsernameEdit username ->
            ( { model | username = EditingUsername username }, Cmd.none )

        UsernameSubmit ->
            case model.username of
                EditingUsername username ->
                    ( { model | username = SelectedUsername username }
                    , Cmd.batch
                        [ fetchInitialState username
                        , localStorageWrites ( localStorageKeys.username, username )
                        ]
                    )

                SelectedUsername _ ->
                    ( model, Cmd.none )


fetchInitialState username =
    { usernameByFiat = username, request = Api.Get }
        |> Functions.send Api.endpoint ApiResponse


applyUpdate apiUpdate remote =
    { remote
        | timerSet =
            case remote.timerSet of
                Nothing ->
                    Nothing

                Just timerSet ->
                    Just { version = timerSet.version, value = Api.applyUpdate apiUpdate timerSet.value }
    }


enqueueAll updates model =
    case ( model.username, model.remote.timerSet ) of
        ( SelectedUsername username, Just { version } ) ->
            let
                ( newPending, cmd ) =
                    case ( model.pending, updates ) of
                        ( PendingIdle, [] ) ->
                            ( PendingIdle, Cmd.none )

                        ( PendingIdle, _ ) ->
                            ( Pending
                                { current = PendingOutstanding updates
                                , queue = []
                                , base = model.remote
                                }
                            , { usernameByFiat = username, request = Api.Update version updates }
                                |> Functions.send Api.endpoint ApiResponse
                            )

                        ( Pending pending, _ ) ->
                            ( Pending { pending | queue = List.foldl (::) pending.queue updates }, Cmd.none )
            in
            ( { model | remote = List.foldl applyUpdate model.remote updates, pending = newPending }, cmd )

        _ ->
            ( model, Cmd.none )


enqueue apiUpdate =
    enqueueAll [ apiUpdate ]


maybeInitialize { now, zone } =
    case ( now, zone ) of
        ( Just justNow, Just justZone ) ->
            TimeInitialized { now = justNow, zone = justZone }

        _ ->
            TimeUninitialized { now = now, zone = zone }


view model =
    { title = strings.appTitle
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
                 , globalCss model
                 ]
                    ++ viewBody model
                )
            )
        ]
    }


globalCss { remote, time } =
    Css.Global.global
        [ Css.Global.everything
            [ Css.margin Css.zero
            , Css.padding Css.zero
            , Css.fontFamilies [ Css.qt "Nunito", Css.sansSerif.value ]
            ]
        , Css.Global.html
            [ Css.minHeight (Css.pct 100) -- Without this, background color transitions for the html don't happen properly in the area not covered by the body
            , Css.backgroundColor
                (case remote.timerSet of
                    Nothing ->
                        colors.paused

                    Just timerSet ->
                        case time of
                            TimeUninitialized _ ->
                                colors.paused

                            TimeInitialized { now } ->
                                if Maybe.Extra.isJust (Timeline.at now (TimerSet.history timerSet.value)) then
                                    colors.running

                                else
                                    colors.paused
                )
            , Css.Transitions.transition [ Css.Transitions.backgroundColor (Duration.inMilliseconds durations.transition) ]
            ]
        ]


viewBody model =
    let
        ( forceLoading, body ) =
            case model.username of
                EditingUsername _ ->
                    ( False, viewAuthentication )

                SelectedUsername _ ->
                    case ( model.time, model.remote.timerSet ) of
                        ( TimeInitialized time, Just timerSet ) ->
                            ( False, viewTimers time timerSet.value model ++ viewHistory time timerSet.value model )

                        _ ->
                            ( True, [] )
    in
    viewErrors forceLoading model ++ body


type LoadingState
    = Waiting
    | Retryable
    | Idle


viewErrors forceLoading { errors, pending } =
    let
        cellProperties =
            [ Css.property "grid-row" "1"
            , Css.property "grid-column" "1"
            , Css.Transitions.transition [ Css.Transitions.opacity (Duration.inMilliseconds durations.transition) ]
            ]

        loadingState =
            if forceLoading then
                Waiting

            else
                case pending of
                    PendingIdle ->
                        Idle

                    Pending { current } ->
                        case current of
                            PendingOutstanding _ ->
                                Waiting

                            PendingError _ ->
                                Retryable
    in
    [ Html.Styled.div
        [ Html.Styled.Events.onClick ApiRetry
        , Html.Styled.Attributes.css
            [ Css.property "display" "grid"
            , Css.width (Css.px 24)
            , Css.height (Css.px 24)
            , Css.fontSize (Css.px 24)
            , Css.fontWeight Css.bold
            , Css.lineHeight (Css.px 24)
            , Css.margin Css.auto
            , Css.textAlign Css.center
            , Css.cursor
                (if loadingState == Retryable then
                    Css.pointer

                 else
                    Css.default
                )
            ]
        ]
        [ Html.Styled.div
            [ Html.Styled.Attributes.css
                (cellProperties
                    ++ [ Css.opacity
                            (if loadingState == Waiting then
                                Css.num 100

                             else
                                Css.num 0
                            )
                       ]
                )
            ]
            [ Html.Styled.text "⭮" ]
        , Html.Styled.div
            [ Html.Styled.Attributes.css
                (cellProperties
                    ++ [ Css.opacity
                            (if loadingState == Retryable then
                                Css.num 100

                             else
                                Css.num 0
                            )
                       ]
                )
            ]
            [ Html.Styled.text "⭯" ]
        ]
    ]
        ++ (errors
                |> List.map strings.error
                |> List.map (\error -> Html.Styled.div [] [ Html.Styled.text error ])
           )


viewAuthentication =
    [ Html.Styled.text (strings.enterUsernamePrompt ++ ": ")
    , Html.Styled.input [ Html.Styled.Events.onInput UsernameEdit ] []
    , Html.Styled.button [ Html.Styled.Events.onClick UsernameSubmit ] [ Html.Styled.text strings.submitUsername ]
    ]


viewPaused =
    Html.Styled.text strings.paused


viewTimers { now, zone } timerSet { clearConfirmation, edit } =
    let
        history =
            TimerSet.history timerSet

        currentTimer =
            Timeline.at now history

        timers =
            history
                |> Timeline.toList
                |> List.reverse
                |> List.filterMap (Tuple.second >> Maybe.Extra.filter (Just >> (/=) currentTimer))
                |> List.Extra.uniqueBy TimerSet.keyTimerId
    in
    [ case currentTimer of
        Nothing ->
            viewPaused

        Just currentTimerId ->
            viewTimer now edit timerSet currentTimerId
    ]
        ++ List.map (viewTimer now edit timerSet) timers
        ++ [ Html.Styled.button [ Html.Styled.Events.onClick AddTimer ] [ Html.Styled.text strings.startNewTimer ]
           ]
        ++ (case clearConfirmation of
                ClearConfirmationHidden ->
                    [ Html.Styled.button [ Html.Styled.Events.onClick ClearTimersInitiate ] [ Html.Styled.text strings.clearTimers ] ]

                ClearConfirmationShown ->
                    [ Html.Styled.button [ Html.Styled.Events.onClick ClearTimersCancel ] [ Html.Styled.text strings.clearTimersCancel ]
                    , Html.Styled.button [ Html.Styled.Events.onClick ClearTimersConfirm ] [ Html.Styled.text strings.clearTimersConfirm ]
                    ]
           )
        ++ (case edit of
                Nothing ->
                    []

                Just _ ->
                    [ Html.Styled.text "*" ]
           )


startOfDay zone date =
    Time.Extra.partsToPosix
        zone
        { year = Date.year date
        , month = Date.month date
        , day = Date.day date
        , hour = 0
        , minute = 0
        , second = 0
        , millisecond = 0
        }


viewHistoryItem zone timerSet ( timer, start ) =
    Html.Styled.div []
        [ viewTimestamp zone start
        , Html.Styled.text " "
        , timer
            |> Maybe.map (\id -> TimerSet.get id timerSet)
            |> Maybe.Extra.unwrap strings.paused (Maybe.Extra.unwrap strings.unknownTimer .name)
            |> Html.Styled.text
        ]


viewHistory { now, zone } timerSet { historySelectedDate } =
    let
        date =
            historySelectedDate
                |> Maybe.withDefault (Date.fromPosix zone now)

        dayStart =
            startOfDay zone date

        dayEnd =
            startOfDay zone (Date.add Date.Days 1 date)

        history =
            TimerSet.history timerSet

        dailyHistory =
            history
                |> Timeline.fold [] (\value start duration -> (::) ( value, start )) dayStart dayEnd
                |> List.reverse

        viewTotalLine text predicate =
            Html.Styled.div []
                [ viewDuration (sumDuration predicate dayStart (timeMin now dayEnd) history)
                , Html.Styled.text (" " ++ text)
                ]

        actCatPred prop val =
            Maybe.andThen (\id -> TimerSet.get id timerSet) >> Maybe.andThen prop >> Maybe.Extra.unwrap False ((==) val)

        timers =
            Timeline.fold []
                (\maybeValue start duration acc ->
                    case maybeValue of
                        Nothing ->
                            acc

                        Just value ->
                            if List.member value acc then
                                acc

                            else
                                value :: acc
                )
                dayStart
                dayEnd
                history
                |> List.reverse
    in
    [ Html.Styled.h1 []
        [ historySelectedDate
            |> Maybe.withDefault (Date.fromPosix zone now)
            |> Date.toIsoString
            |> Html.Styled.text
        ]
    , Html.Styled.div []
        [ Html.Styled.button [ Html.Styled.Events.onClick (HistoryIncrementDate { days = -1 }) ] [ Html.Styled.text strings.previous ]
        , Html.Styled.button [ Html.Styled.Events.onClick (HistoryIncrementDate { days = 1 }) ] [ Html.Styled.text strings.next ]
        ]
    ]
        ++ [ Html.Styled.h2 [] [ Html.Styled.text strings.totals ] ]
        ++ List.map
            (\timerId ->
                viewTotalLine
                    (timerSet
                        |> TimerSet.get timerId
                        |> Maybe.map .name
                        |> Maybe.Extra.filter (String.isEmpty >> not)
                        |> Maybe.withDefault strings.unnamedTimer
                    )
                    ((==) (Just timerId))
            )
            timers
        ++ [ viewTotalLine strings.abbreviationActive (actCatPred .activity TimerSet.Active)
           , viewTotalLine strings.abbreviationReactive (actCatPred .activity TimerSet.Reactive)
           , viewTotalLine strings.abbreviationProactive (actCatPred .activity TimerSet.Proactive)
           , viewTotalLine strings.abbreviationOperational (actCatPred .category TimerSet.Operational)
           , viewTotalLine strings.abbreviationHelpful (actCatPred .category TimerSet.Helpful)
           , viewTotalLine strings.abbreviationProductive (actCatPred .category TimerSet.Productive)
           , viewTotalLine strings.total Maybe.Extra.isJust
           ]
        ++ [ Html.Styled.h2 [] [ Html.Styled.text strings.history ] ]
        ++ List.map (viewHistoryItem zone timerSet) dailyHistory


viewActCatToggle factory id currentValue newValue text =
    Html.Styled.button
        [ Html.Styled.Events.onClick (factory id newValue)
        , Html.Styled.Attributes.css
            [ Css.backgroundColor
                (if currentValue == Just newValue then
                    colors.toggleOn

                 else
                    colors.toggleOff
                )
            ]
        ]
        [ Html.Styled.text text
        ]


sumDuration : (Maybe a -> Bool) -> Time.Posix -> Time.Posix -> Timeline.Timeline a -> Duration.Duration
sumDuration filter startInclusive endExclusive =
    let
        addIfKey testKey _ durationToAdd =
            if filter testKey then
                Quantity.plus durationToAdd

            else
                identity
    in
    Timeline.fold Quantity.zero addIfKey startInclusive endExclusive


viewTimer now edit timerSet id =
    let
        history =
            TimerSet.history timerSet

        running =
            case Timeline.at now history of
                Nothing ->
                    False

                Just atNow ->
                    atNow == id
    in
    case TimerSet.get id timerSet of
        Nothing ->
            Html.Styled.div [] [ Html.Styled.text strings.unknownTimer ]

        Just { name, activity, category } ->
            Html.Styled.div []
                [ Html.Styled.input
                    [ Html.Styled.Attributes.placeholder strings.unnamedTimer
                    , Html.Styled.Attributes.value
                        (case edit of
                            Nothing ->
                                name

                            Just (EditTimerName nameEdit) ->
                                if nameEdit.timerId == id then
                                    nameEdit.name

                                else
                                    name
                        )
                    , Html.Styled.Events.onInput (\updatedName -> TimerEditRename { timerId = id, name = updatedName })
                    , Html.Styled.Events.onBlur TimerEditCommit
                    ]
                    []
                , Html.Styled.text " "
                , viewActCatToggle TimerToggleActivity id activity TimerSet.Active strings.abbreviationActive
                , viewActCatToggle TimerToggleActivity id activity TimerSet.Reactive strings.abbreviationReactive
                , viewActCatToggle TimerToggleActivity id activity TimerSet.Proactive strings.abbreviationProactive
                , Html.Styled.text " "
                , viewActCatToggle TimerToggleCategory id category TimerSet.Operational strings.abbreviationOperational
                , viewActCatToggle TimerToggleCategory id category TimerSet.Helpful strings.abbreviationHelpful
                , viewActCatToggle TimerToggleCategory id category TimerSet.Productive strings.abbreviationProductive
                , Html.Styled.text " "
                , Html.Styled.button
                    [ Html.Styled.Events.onClick (TimerToggleRunning id)
                    ]
                    [ Html.Styled.text
                        (if running then
                            strings.stop

                         else
                            strings.start
                        )
                    ]
                ]


viewTimestamp zone time =
    let
        hours =
            Time.toHour zone time

        minutes =
            Time.toMinute zone time

        seconds =
            Time.toSecond zone time

        hundredths =
            Time.toMillis zone time // 10
    in
    Html.Styled.span []
        [ Html.Styled.text (pad hours ++ ":" ++ pad minutes ++ ":" ++ pad seconds)
        , Html.Styled.small [] [ Html.Styled.text ("." ++ pad hundredths) ]
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


timeMin a b =
    if Time.posixToMillis a < Time.posixToMillis b then
        a

    else
        b


rawColors =
    { imperialRed = Css.rgb 239 35 60
    , middleBlue = Css.rgb 138 198 208
    , spanishGreen = Css.rgb 16 150 72
    , blackCoral = Css.rgb 94 105 115
    , silver = Css.rgb 201 201 201
    }


colors =
    { running = rawColors.spanishGreen
    , paused = rawColors.blackCoral
    , toggleOff = rawColors.silver
    , toggleOn = rawColors.middleBlue
    }


strings =
    { appTitle = "Timetrack"
    , unnamedTimer = "Unnamed Timer"
    , enterUsernamePrompt = "Enter username"
    , submitUsername = "Submit"
    , paused = "Paused"
    , unknownTimer = "Unknown Timer"
    , previous = "prev"
    , next = "next"
    , totals = "Totals"
    , abbreviationActive = "A"
    , abbreviationReactive = "R"
    , abbreviationProactive = "P"
    , abbreviationOperational = "O"
    , abbreviationHelpful = "H"
    , abbreviationProductive = "P"
    , total = "Total"
    , history = "History"
    , stop = "Stop"
    , start = "Start"
    , startNewTimer = "start new timer"
    , clearTimers = "clear"
    , clearTimersCancel = "cancel"
    , clearTimersConfirm = "Are you sure?"
    , error =
        \error ->
            case error of
                TimeZoneError tzError ->
                    "Timezone error: "
                        ++ (case tzError of
                                TimeZone.NoZoneName ->
                                    "No zone name!"

                                TimeZone.NoDataForZoneName zonename ->
                                    "No data for zone " ++ zonename ++ "!"
                           )

                ApiError apiError ->
                    case apiError of
                        Functions.BadUrl url ->
                            "Bad url: " ++ url

                        Functions.Timeout ->
                            "Timeout"

                        Functions.NetworkError ->
                            "Network error"

                        Functions.HttpError { statusCode } _ ->
                            if statusCode >= 400 && statusCode < 500 then
                                "HTTP client error: " ++ String.fromInt statusCode

                            else if statusCode >= 500 && statusCode < 600 then
                                "HTTP server error: " ++ String.fromInt statusCode

                            else
                                "HTTP error: " ++ String.fromInt statusCode

                        Functions.MalformedJson _ ->
                            "Malformed json"

                        Functions.SerializationError serializationError ->
                            "Serialization error: "
                                ++ (case serializationError of
                                        Serialize.CustomError _ ->
                                            "Custom error"

                                        Serialize.DataCorrupted ->
                                            "Data corrupted"

                                        Serialize.SerializerOutOfDate ->
                                            "Serializer out of date"
                                   )
    }


durations =
    { transition = Duration.milliseconds 150
    }


localStorageKeys =
    { username = "username"
    }
