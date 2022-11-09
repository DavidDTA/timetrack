port module Client exposing (main)

import Api
import Browser
import Browser.Events
import Browser.Navigation
import Color
import Css
import Css.Animations
import Css.Global
import Css.Transitions
import Date
import Duration
import Functions
import GenericDict
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
import Select
import SelectedDate
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
        , onUrlRequest = always UrlRequest
        , onUrlChange = always UrlChange
        }


type alias Model =
    { time : TimeModel
    , username : Username
    , errors : List Error
    , historyEdit : Maybe HistoryEdit
    , historySelectedDate : SelectedDate.SelectedDate
    , remote : Remote
    , pending : Pending Remote
    , timersEdits : GenericDict.Dict TimerSet.TimerId TimerNameEdit
    , timersSelectInput : String
    , timersSelectState : Select.State
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


type alias TimerNameEdit =
    { name : String }


type alias HistoryEdit =
    { originalStart : Time.Posix
    , originalEnd : Time.Posix
    , originalTimerId : Maybe TimerSet.TimerId
    , startHour : Int
    , startMinute : Int
    , endHour : Int
    , endMinute : Int
    , timerId : Maybe TimerSet.TimerId
    , zone : Time.Zone
    }


type TimersSelectOption
    = StartTimer TimerSet.TimerId
    | AddTimer String


type Error
    = TimeZoneError TimeZone.Error
    | ApiError Functions.SendError
    | InvalidHistoryEdit String
    | Uninitialized


type Msg
    = ApiResponse (Result Functions.SendError Api.Response)
    | ApiRetry
    | UpdateNow Time.Posix
    | UpdateZone Time.Zone
    | UpdateZoneError TimeZone.Error
    | HistoryEditStart { timerId : Maybe TimerSet.TimerId, start : Time.Posix, end : Time.Posix }
    | HistoryEditUpdateTimerId String
    | HistoryEditUpdateStartHours String
    | HistoryEditUpdateStartMinutes String
    | HistoryEditUpdateEndHours String
    | HistoryEditUpdateEndMinutes String
    | HistoryEditCommit
    | HistoryIncrementDate { days : Int }
    | TimerEditCommit TimerSet.TimerId
    | TimerEditRename { timerId : TimerSet.TimerId, name : String }
    | TimerSelectMsg (Select.Msg TimersSelectOption)
    | TimerToggleActivity TimerSet.TimerId TimerSet.Activity
    | TimerToggleCategory TimerSet.TimerId TimerSet.Category
    | UrlChange
    | UrlRequest
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
      , historyEdit = Nothing
      , historySelectedDate = SelectedDate.unselected
      , remote = { timerSet = Nothing }
      , pending = PendingIdle
      , timersEdits = timerIdDict.empty
      , timersSelectInput = ""
      , timersSelectState = Select.initState (Select.selectIdentifier "")
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
        updateHistoryEdit parser mapper raw =
            case parser raw of
                Just selection ->
                    ( { model
                        | historyEdit =
                            model.historyEdit
                                |> Maybe.map (mapper selection)
                      }
                    , Cmd.none
                    )

                Nothing ->
                    ( { model | errors = InvalidHistoryEdit raw :: model.errors }, Cmd.none )
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
                                    enqueueAll (List.reverse queue) model1
                            in
                            ( model2, Cmd.batch [ cmd1, cmd2 ] )

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

        UrlChange ->
            ( model, Cmd.none )

        UrlRequest ->
            ( model, Cmd.none )

        HistoryEditStart { timerId, start, end } ->
            case model.time of
                TimeUninitialized _ ->
                    ( { model | errors = Uninitialized :: model.errors }, Cmd.none )

                TimeInitialized { zone } ->
                    let
                        startParts =
                            Time.Extra.posixToParts zone start

                        endParts =
                            Time.Extra.posixToParts zone end
                    in
                    ( { model
                        | historyEdit =
                            Just
                                { originalTimerId = timerId
                                , originalStart = start
                                , originalEnd = end
                                , startHour = startParts.hour
                                , startMinute = startParts.minute
                                , endHour = endParts.hour
                                , endMinute = endParts.minute
                                , timerId = timerId
                                , zone = zone
                                }
                      }
                    , Cmd.none
                    )

        HistoryEditUpdateTimerId raw ->
            updateHistoryEdit
                (\raw_ ->
                    if raw_ == "" then
                        Just Nothing

                    else
                        raw_ |> String.toInt |> Maybe.map TimerSet.timerIdFromRaw |> Just
                )
                (\selection edit -> { edit | timerId = selection })
                raw

        HistoryEditUpdateStartHours raw ->
            updateHistoryEdit String.toInt (\selection edit -> { edit | startHour = selection }) raw

        HistoryEditUpdateStartMinutes raw ->
            updateHistoryEdit String.toInt (\selection edit -> { edit | startMinute = selection }) raw

        HistoryEditUpdateEndHours raw ->
            updateHistoryEdit String.toInt (\selection edit -> { edit | endHour = selection }) raw

        HistoryEditUpdateEndMinutes raw ->
            updateHistoryEdit String.toInt (\selection edit -> { edit | endMinute = selection }) raw

        HistoryEditCommit ->
            case model.historyEdit of
                Nothing ->
                    ( { model | errors = Uninitialized :: model.errors }, Cmd.none )

                Just { originalTimerId, originalStart, originalEnd, startHour, startMinute, endHour, endMinute, timerId, zone } ->
                    let
                        originalStartParts =
                            Time.Extra.posixToParts zone originalStart

                        originalEndParts =
                            Time.Extra.posixToParts zone originalEnd

                        startSame =
                            originalStartParts.hour == startHour && originalStartParts.minute == startMinute

                        endSame =
                            originalEndParts.hour == endHour && originalEndParts.minute == endMinute

                        timerSame =
                            originalTimerId == timerId

                        start =
                            if startSame then
                                originalStart

                            else
                                Time.Extra.partsToPosix zone { originalStartParts | hour = startHour, minute = startMinute, second = 0, millisecond = 0 }

                        end =
                            if endSame then
                                originalEnd

                            else if endHour == 0 && endMinute == 0 then
                                Time.Extra.ceiling Time.Extra.Day zone originalStart

                            else
                                Time.Extra.partsToPosix zone { originalStartParts | hour = endHour, minute = endMinute, second = 0, millisecond = 0 }

                        withEditReset =
                            { model | historyEdit = Nothing }
                    in
                    if startSame && endSame && timerSame then
                        ( withEditReset, Cmd.none )

                    else if timerSame && originalTimerId /= Nothing then
                        let
                            ( withRangeReset, cmd1 ) =
                                withEditReset
                                    |> enqueue (Api.TimersSetActive { timerId = Nothing, start = originalStart, end = Just originalEnd })

                            ( withNewRange, cmd2 ) =
                                withRangeReset
                                    |> enqueue (Api.TimersSetActive { timerId = timerId, start = start, end = Just end })
                        in
                        ( withNewRange, Cmd.batch [ cmd1, cmd2 ] )

                    else
                        withEditReset
                            |> enqueue (Api.TimersSetActive { timerId = timerId, start = start, end = Just end })

        HistoryIncrementDate { days } ->
            ( case model.time of
                TimeUninitialized _ ->
                    model

                TimeInitialized { now, zone } ->
                    { model
                        | historySelectedDate =
                            model.historySelectedDate
                                |> SelectedDate.getDate now zone
                                |> Date.add Date.Days days
                                |> SelectedDate.fromDate now zone
                    }
            , Cmd.none
            )

        TimerEditCommit timerId ->
            case timerIdDict.get timerId model.timersEdits of
                Nothing ->
                    ( model, Cmd.none )

                Just { name } ->
                    { model | timersEdits = timerIdDict.remove timerId model.timersEdits }
                        |> enqueue (Api.TimersRename timerId name)

        TimerEditRename timerEditRename ->
            ( { model | timersEdits = timerIdDict.insert timerEditRename.timerId { name = timerEditRename.name } model.timersEdits }, Cmd.none )

        TimerSelectMsg selectMsg ->
            let
                ( action, updatedState, updateCmd ) =
                    Select.update selectMsg model.timersSelectState

                withUpdatedSelectState =
                    { model
                        | timersSelectState = updatedState
                    }

                withClearedInput =
                    { model
                        | timersSelectState = updatedState
                        , timersSelectInput = ""
                    }

                ( withActionPerformed, actionCmd ) =
                    case action of
                        Nothing ->
                            ( withUpdatedSelectState, Cmd.none )

                        Just (Select.InputChange input) ->
                            ( { withUpdatedSelectState | timersSelectInput = input }, Cmd.none )

                        Just (Select.Select selected) ->
                            case selected of
                                StartTimer selectedTimerId ->
                                    case model.time of
                                        TimeInitialized { now } ->
                                            enqueue (Api.TimersSetActive { timerId = Just selectedTimerId, start = now, end = Nothing }) withClearedInput

                                        TimeUninitialized _ ->
                                            ( { withClearedInput | errors = Uninitialized :: model.errors }, Cmd.none )

                                AddTimer name ->
                                    case model.time of
                                        TimeInitialized { now } ->
                                            enqueue (Api.TimersAddAndStart { name = name, start = now }) withClearedInput

                                        TimeUninitialized _ ->
                                            ( { withClearedInput | errors = Uninitialized :: model.errors }, Cmd.none )

                        Just (Select.DeselectMulti _) ->
                            ( withClearedInput, Cmd.none )

                        Just Select.ClearSingleSelectItem ->
                            ( withClearedInput, Cmd.none )

                        Just Select.FocusSet ->
                            ( withUpdatedSelectState, Cmd.none )

                        Just Select.MenuInputCleared ->
                            ( withClearedInput, Cmd.none )
            in
            ( withActionPerformed
            , Cmd.batch
                [ actionCmd
                , Cmd.map TimerSelectMsg updateCmd
                ]
            )

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
                    ( { model | errors = Uninitialized :: model.errors }, Cmd.none )

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
                    ( { model | errors = Uninitialized :: model.errors }, Cmd.none )

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
            , Css.fontFamilies [ Css.qt "Nunito" ]
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


viewBody ({ pending, remote, time, username } as model) =
    let
        ( loadingState, body ) =
            case username of
                EditingUsername _ ->
                    ( Idle, viewAuthentication )

                SelectedUsername _ ->
                    case ( time, remote.timerSet ) of
                        ( TimeInitialized initializedTime, Just timerSet ) ->
                            ( case pending of
                                PendingIdle ->
                                    Idle

                                Pending { current } ->
                                    case current of
                                        PendingOutstanding _ ->
                                            Waiting

                                        PendingError _ ->
                                            Retryable
                            , viewTimers initializedTime timerSet.value model ++ viewHistory initializedTime timerSet.value model
                            )

                        _ ->
                            ( Waiting, [] )
    in
    viewLoading loadingState ++ viewErrors model ++ body


type LoadingState
    = Waiting
    | Retryable
    | Idle


viewLoading loadingState =
    let
        cellProperties =
            [ Css.property "grid-row" "1"
            , Css.property "grid-column" "1"
            , Css.Transitions.transition [ Css.Transitions.opacity (Duration.inMilliseconds durations.transition) ]
            ]
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
                       , Css.animationName
                            (Css.Animations.keyframes
                                [ ( 0, [ Css.Animations.transform [ Css.rotate (Css.deg 0) ] ] )
                                , ( 100, [ Css.Animations.transform [ Css.rotate (Css.deg 360) ] ] )
                                ]
                            )
                       , Css.animationDuration (Css.sec (Duration.inSeconds durations.spinnerRotation))
                       , Css.animationIterationCount Css.infinite
                       , Css.property "animation-timing-function" "linear"
                       ]
                )
            ]
            [ Html.Styled.text "↻" ]
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
            [ Html.Styled.text "↺" ]
        ]
    ]


viewErrors { errors } =
    errors
        |> List.map strings.error
        |> List.map (\error -> Html.Styled.div [] [ Html.Styled.text error ])


viewAuthentication =
    [ Html.Styled.text (strings.enterUsernamePrompt ++ ": ")
    , Html.Styled.input [ Html.Styled.Events.onInput UsernameEdit ] []
    , Html.Styled.button [ Html.Styled.Events.onClick UsernameSubmit ] [ Html.Styled.text strings.submitUsername ]
    ]


viewPaused =
    Html.Styled.text strings.paused


viewTimers { now, zone } timerSet { timersEdits, timersSelectInput, timersSelectState } =
    let
        history =
            TimerSet.history timerSet

        currentTimer =
            Timeline.at now history

        timers =
            history
                |> Timeline.toList
                |> List.reverse
                |> List.filterMap Tuple.second
                |> flip List.append (TimerSet.listTimerIds timerSet)
                |> List.Extra.unique
    in
    [ timers
        |> List.map
            (\timerId ->
                Select.basicMenuItem
                    { item = StartTimer timerId
                    , label =
                        case TimerSet.get timerId timerSet of
                            Nothing ->
                                strings.unknownTimer

                            Just { name } ->
                                if name == "" then
                                    strings.unnamedTimer

                                else
                                    name
                    }
            )
        |> flip List.append
            (if String.trim timersSelectInput /= "" then
                [ Select.basicMenuItem
                    { item = AddTimer (String.trim timersSelectInput)
                    , label = strings.newTimer ++ timersSelectInput
                    }
                ]

             else
                []
            )
        |> flip Select.menuItems (Select.single Nothing)
        |> Select.state timersSelectState
        |> Select.searchable True
        |> Select.view
        |> Html.Styled.map TimerSelectMsg
    ]
        ++ (case currentTimer of
                Nothing ->
                    [ viewPaused ]

                Just currentTimerId ->
                    []
           )
        ++ List.map (\id -> viewTimer now (timerIdDict.get id timersEdits) timerSet id) timers
        ++ (if timerIdDict.isEmpty timersEdits then
                []

            else
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


viewHistoryItem zone timerSet { value, start, duration } =
    Html.Styled.div
        [ Html.Styled.Events.onClick (HistoryEditStart { timerId = value, start = start, end = Duration.addTo start duration })
        ]
        [ viewTimestamp zone start
        , Html.Styled.text " "
        , value
            |> timerDisplayName timerSet
            |> Html.Styled.text
        ]


viewHistoryEdit timerSet historyEdit =
    case historyEdit of
        Nothing ->
            []

        Just { originalTimerId, originalStart, originalEnd, zone } ->
            viewTimeSelect HistoryEditUpdateStartHours HistoryEditUpdateStartMinutes zone originalStart
                ++ [ Html.Styled.text strings.historyEditRange ]
                ++ viewTimeSelect HistoryEditUpdateEndHours HistoryEditUpdateEndMinutes zone originalEnd
                ++ [ viewSelect HistoryEditUpdateTimerId
                        ({ value = ""
                         , selected = originalTimerId == Nothing
                         , label =
                            timerDisplayName timerSet Nothing
                         }
                            :: (TimerSet.listTimerIds timerSet
                                    |> List.map
                                        (\timerId ->
                                            { value = TimerSet.timerIdToRaw timerId |> String.fromInt
                                            , selected =
                                                Just timerId == originalTimerId
                                            , label =
                                                timerDisplayName timerSet (Just timerId)
                                            }
                                        )
                               )
                        )
                   , Html.Styled.button
                        [ Html.Styled.Events.onClick HistoryEditCommit
                        ]
                        [ Html.Styled.text strings.historyEditConfirm ]
                   ]


viewSelect onInput options =
    Html.Styled.select
        [ Html.Styled.Events.onInput onInput
        ]
        (List.map
            (\option ->
                Html.Styled.option
                    [ Html.Styled.Attributes.value option.value
                    , Html.Styled.Attributes.selected option.selected
                    ]
                    [ Html.Styled.text option.label ]
            )
            options
        )


viewTimeSelect updateHours updateMinutes zone posix =
    let
        parts =
            Time.Extra.posixToParts zone posix
    in
    [ List.range 0 23
        |> List.map (\hourOption -> { value = String.fromInt hourOption, label = String.fromInt hourOption, selected = parts.hour == hourOption })
        |> viewSelect updateHours
    , List.range 0 59
        |> List.map (\minuteOption -> { value = String.fromInt minuteOption, label = String.fromInt minuteOption, selected = parts.minute == minuteOption })
        |> viewSelect updateMinutes
    ]


timerDisplayName timerSet timerId =
    timerId
        |> Maybe.map (\id -> TimerSet.get id timerSet)
        |> Maybe.Extra.unwrap strings.paused (Maybe.Extra.unwrap strings.unknownTimer .name)


viewHistory { now, zone } timerSet { historySelectedDate, historyEdit } =
    let
        date =
            historySelectedDate
                |> SelectedDate.getDate now zone

        dayStart =
            startOfDay zone date

        dayEnd =
            startOfDay zone (Date.add Date.Days 1 date)

        history =
            TimerSet.history timerSet

        dailyHistory =
            history
                |> Timeline.fold [] (::) dayStart dayEnd
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
                (\{ value, start, duration } acc ->
                    case value of
                        Nothing ->
                            acc

                        Just actualValue ->
                            if List.member actualValue acc then
                                acc

                            else
                                actualValue :: acc
                )
                dayStart
                dayEnd
                history
                |> List.reverse
    in
    [ Html.Styled.h1 []
        [ historySelectedDate
            |> SelectedDate.getDate now zone
            |> Date.toIsoString
            |> Html.Styled.text
        ]
    , Html.Styled.div []
        [ Html.Styled.button [ Html.Styled.Events.onClick (HistoryIncrementDate { days = -1 }) ] [ Html.Styled.text strings.previous ]
        , Html.Styled.button [ Html.Styled.Events.onClick (HistoryIncrementDate { days = 1 }) ] [ Html.Styled.text strings.next ]
        ]
    ]
        ++ [ Html.Styled.h2 [] [ Html.Styled.text strings.history ] ]
        ++ viewHistoryEdit timerSet historyEdit
        ++ List.map (viewHistoryItem zone timerSet) dailyHistory
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
        addIfKey { value, duration } =
            if filter value then
                Quantity.plus duration

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

                            Just nameEdit ->
                                nameEdit.name
                        )
                    , Html.Styled.Events.onInput (\updatedName -> TimerEditRename { timerId = id, name = updatedName })
                    , Html.Styled.Events.onBlur (TimerEditCommit id)
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
    , historyEditRange = " to "
    , historyEditConfirm = "Confirm"
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

                InvalidHistoryEdit raw ->
                    "Invalid history edit input: " ++ raw

                Uninitialized ->
                    "Uninitialized!"
    , newTimer = "New timer: "
    }


durations =
    { spinnerRotation = Duration.milliseconds 800
    , transition = Duration.milliseconds 150
    }


localStorageKeys =
    { username = "username"
    }


timerIdDict =
    GenericDict.makeInterface TimerSet.compareTimerId


flip f a b =
    f b a
