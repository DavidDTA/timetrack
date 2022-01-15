module Client exposing (main)

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
    , historySelectedDate : Maybe Date.Date
    , timerSet : Maybe TimerSet.TimerSet
    , pending : Maybe { current : List Api.Update, queue : List Api.Update }
    , clearConfirmation : ClearConfirmation
    , edit : Maybe Edit
    }


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


type Edit
    = EditTimerName { timerId : TimerSet.TimerId, name : String }


type Error
    = TimeZoneError TimeZone.Error
    | ApiError (Api.Error Never)


type Msg
    = ApiResponse (Result (Api.Error Never) Api.Response)
    | Error Error
    | Nop
    | UpdateNow Time.Posix
    | UpdateZone Time.Zone
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


init : () -> Url.Url -> Browser.Navigation.Key -> ( Model, Cmd Msg )
init _ _ _ =
    ( { time = TimeUninitialized { now = Nothing, zone = Just (TimeZone.america__new_york ()) }
      , errors = []
      , historySelectedDate = Nothing
      , timerSet = Nothing
      , pending = Nothing
      , clearConfirmation = ClearConfirmationHidden
      , edit = Nothing
      }
    , Cmd.batch
        [ Api.send Api.Get ApiResponse
        , TimeZone.getZone
            |> Task.attempt (Result.Extra.unpack (TimeZoneError >> Error) (Tuple.second >> UpdateZone))
        ]
    )


sub _ =
    Sub.batch
        [ Browser.Events.onAnimationFrame UpdateNow
        ]


update msg model =
    let
        nop =
            ( model, Cmd.none )
    in
    case msg of
        ApiResponse result ->
            case result of
                Result.Err err ->
                    update (Error (ApiError err)) model

                Result.Ok response ->
                    case response of
                        Api.Value serverTimerSet ->
                            let
                                newQueue =
                                    case model.pending of
                                        Nothing ->
                                            []

                                        Just { queue } ->
                                            List.reverse queue
                            in
                            { model | timerSet = Just serverTimerSet, pending = Nothing }
                                |> enqueueAll newQueue

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
            case model.timerSet of
                Just timerSet ->
                    let
                        currentActivity =
                            TimerSet.get timerId timerSet
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
            case model.timerSet of
                Just timerSet ->
                    let
                        currentCategory =
                            TimerSet.get timerId timerSet
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
            case ( model.time, model.timerSet ) of
                ( TimeInitialized { now }, Just timerSet ) ->
                    let
                        currentId =
                            Timeline.at now (TimerSet.history timerSet)

                        newId =
                            if Just id == currentId then
                                Nothing

                            else
                                Just id
                    in
                    enqueue (Api.TimersSetActive newId now) model

                ( _, _ ) ->
                    nop


enqueueAll updates model =
    case model.timerSet of
        Nothing ->
            ( model, Cmd.none )

        Just timerSet ->
            let
                ( newPending, cmd ) =
                    case ( model.pending, updates ) of
                        ( Nothing, [] ) ->
                            ( Just { current = updates, queue = [] }, Cmd.none )

                        ( Nothing, _ ) ->
                            ( Just { current = updates, queue = [] }, Api.send (Api.Update updates) ApiResponse )

                        ( Just pending, _ ) ->
                            ( Just { pending | queue = List.foldl (::) pending.queue updates }, Cmd.none )
            in
            ( { model | timerSet = Just (List.foldl applyUpdate timerSet updates), pending = newPending }, cmd )


enqueue apiUpdate =
    enqueueAll [ apiUpdate ]


applyUpdate apiUpdate timerSet =
    case apiUpdate of
        Api.TimersAddAndStart timestamp ->
            let
                ( newTimerSet, newTimerId ) =
                    TimerSet.addTimer timerSet
            in
            TimerSet.startTimer (Just newTimerId) timestamp newTimerSet

        Api.TimersClear ->
            TimerSet.reset timerSet

        Api.TimersRename timerId name ->
            TimerSet.updateTimer timerId (\timer -> { timer | name = String.trim name }) timerSet

        Api.TimersSetActivity timerId activity ->
            TimerSet.updateTimer timerId
                (\timer ->
                    { timer
                        | activity =
                            if timer.activity == activity then
                                Nothing

                            else
                                activity
                    }
                )
                timerSet

        Api.TimersSetCategory timerId category ->
            TimerSet.updateTimer timerId
                (\timer ->
                    { timer
                        | category =
                            if timer.category == category then
                                Nothing

                            else
                                category
                    }
                )
                timerSet

        Api.TimersSetActive timerId timestamp ->
            TimerSet.startTimer timerId timestamp timerSet


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
                 , globalCss model
                 ]
                    ++ viewBody model
                )
            )
        ]
    }


globalCss { timerSet, time } =
    Css.Global.global
        [ Css.Global.everything
            [ Css.margin Css.zero
            , Css.padding Css.zero
            , Css.property "overscroll-behavior" "none"
            , Css.fontFamilies [ Css.qt "Nunito", Css.sansSerif.value ]
            ]
        , Css.Global.html
            [ Css.minHeight (Css.pct 100) -- Without this, background color transitions for the html don't happen properly in the area not covered by the body
            , Css.backgroundColor
                (case timerSet of
                    Nothing ->
                        colors.paused

                    Just timerSet_ ->
                        case time of
                            TimeUninitialized _ ->
                                colors.paused

                            TimeInitialized { now } ->
                                if Maybe.Extra.isJust (Timeline.at now (TimerSet.history timerSet_)) then
                                    colors.running

                                else
                                    colors.paused
                )
            , Css.Transitions.transition [ Css.Transitions.backgroundColor (Duration.inMilliseconds durations.transition) ]
            ]
        ]


viewBody model =
    viewErrors model
        ++ (case ( model.time, model.timerSet ) of
                ( TimeInitialized time, Just timerSet ) ->
                    viewTimers time timerSet model ++ viewHistory time timerSet model

                _ ->
                    viewLoading
           )


viewErrors { errors } =
    errors
        |> List.map
            (\error ->
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
                            Api.BadUrl url ->
                                "Bad url: " ++ url

                            Api.Timeout ->
                                "Timeout"

                            Api.NetworkError ->
                                "Network error"

                            Api.HttpError { statusCode } _ ->
                                if statusCode >= 400 && statusCode < 500 then
                                    "HTTP client error"

                                else if statusCode >= 500 && statusCode < 600 then
                                    "HTTP server error"

                                else
                                    "HTTP error"

                            Api.MalformedJson _ ->
                                "Malformed json"

                            Api.SerializationError serializationError ->
                                "Serialization error: "
                                    ++ (case serializationError of
                                            Serialize.CustomError _ ->
                                                "Custom error"

                                            Serialize.DataCorrupted ->
                                                "Data corrupted"

                                            Serialize.SerializerOutOfDate ->
                                                "Serializer out of date"
                                       )
            )
        |> List.map (\error -> Html.Styled.div [] [ Html.Styled.text error ])


viewLoading =
    []


viewPaused =
    Html.Styled.text "paused"


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
        ++ [ Html.Styled.button [ Html.Styled.Events.onClick AddTimer ] [ Html.Styled.text "start new" ]
           ]
        ++ (case clearConfirmation of
                ClearConfirmationHidden ->
                    [ Html.Styled.button [ Html.Styled.Events.onClick ClearTimersInitiate ] [ Html.Styled.text "clear" ] ]

                ClearConfirmationShown ->
                    [ Html.Styled.button [ Html.Styled.Events.onClick ClearTimersCancel ] [ Html.Styled.text "cancel" ]
                    , Html.Styled.button [ Html.Styled.Events.onClick ClearTimersConfirm ] [ Html.Styled.text "Are you sure?" ]
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
            |> Maybe.Extra.unwrap "Timer Paused" (Maybe.Extra.unwrap "Unknown" .name)
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
        [ Html.Styled.button [ Html.Styled.Events.onClick (HistoryIncrementDate { days = -1 }) ] [ Html.Styled.text "prev" ]
        , Html.Styled.button [ Html.Styled.Events.onClick (HistoryIncrementDate { days = 1 }) ] [ Html.Styled.text "next" ]
        ]
    ]
        ++ [ Html.Styled.h2 [] [ Html.Styled.text "Totals" ] ]
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
        ++ [ viewTotalLine "A" (actCatPred .activity TimerSet.Active)
           , viewTotalLine "R" (actCatPred .activity TimerSet.Reactive)
           , viewTotalLine "P" (actCatPred .activity TimerSet.Proactive)
           , viewTotalLine "O" (actCatPred .category TimerSet.Operational)
           , viewTotalLine "H" (actCatPred .category TimerSet.Helpful)
           , viewTotalLine "P" (actCatPred .category TimerSet.Productive)
           , viewTotalLine "Total" Maybe.Extra.isJust
           ]
        ++ [ Html.Styled.h2 [] [ Html.Styled.text "History" ] ]
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
            Html.Styled.div [] [ Html.Styled.text "Unknown Timer" ]

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
                , viewActCatToggle TimerToggleActivity id activity TimerSet.Active "A"
                , viewActCatToggle TimerToggleActivity id activity TimerSet.Reactive "R"
                , viewActCatToggle TimerToggleActivity id activity TimerSet.Proactive "P"
                , Html.Styled.text " "
                , viewActCatToggle TimerToggleCategory id category TimerSet.Operational "O"
                , viewActCatToggle TimerToggleCategory id category TimerSet.Helpful "H"
                , viewActCatToggle TimerToggleCategory id category TimerSet.Productive "P"
                , Html.Styled.text " "
                , Html.Styled.button
                    [ Html.Styled.Events.onClick (TimerToggleRunning id)
                    ]
                    [ Html.Styled.text
                        (if running then
                            "Stop"

                         else
                            "Start"
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
    { unnamedTimer = "Unnamed Timer"
    }


durations =
    { transition = Duration.milliseconds 150
    }
