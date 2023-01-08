port module Client exposing (main)

import Accessibility.Styled
import Api
import Browser
import Browser.Dom
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
import Pixels
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
    { authentication : Authentication
    , calendarZoomLevel : Int
    , time : TimeModel
    , errors : List Error
    , historyEdit : Maybe HistoryEdit
    , historySelectedDate : SelectedDate.SelectedDate
    , remote : Remote
    , page : List Page
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


type Authentication
    = AuthenticationUninitialized { usernameInput : String }
    | AuthenticationInitialized { username : String }


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
    | DomNodeNotFound String


type Page
    = Calendar
    | Totals
    | Menu
    | Errors


type CalendarZoomOperation
    = ZoomIn
    | ZoomOut
    | ZoomNow


type Msg
    = ApiResponse (Result Functions.SendError Api.Response)
    | ApiRetry
    | CalendarZoomStart CalendarZoomOperation
    | CalendarZoomWithViewport CalendarZoomOperation (Result Browser.Dom.Error Browser.Dom.Viewport)
    | CalendarZoomFinished (Result Browser.Dom.Error ())
    | UpdateNow Time.Posix
    | UpdateZone Time.Zone
    | UpdateZoneError TimeZone.Error
    | HistoryEditStart { timerId : Maybe TimerSet.TimerId, start : Time.Posix, end : Time.Posix }
    | HistoryEditUpdateTimerId String
    | HistoryEditUpdateStartHours String
    | HistoryEditUpdateStartMinutes String
    | HistoryEditUpdateEndHours String
    | HistoryEditUpdateEndMinutes String
    | HistoryEditCancel
    | HistoryEditCommit
    | IncrementDate { days : Int }
    | Navigate Page
    | NavigateBack
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
        authentication =
            Json.Decode.decodeValue (Json.Decode.field localStorageKeys.username Json.Decode.string) localStorage
                |> Result.map (\username -> AuthenticationInitialized { username = username })
                |> Result.withDefault (AuthenticationUninitialized { usernameInput = "" })
    in
    ( { authentication = authentication
      , calendarZoomLevel = 1
      , errors = []
      , historyEdit = Nothing
      , historySelectedDate = SelectedDate.unselected
      , remote = { timerSet = Nothing }
      , pending = PendingIdle
      , page = []
      , time = TimeUninitialized { now = Nothing, zone = Just (TimeZone.america__new_york ()) }
      , timersEdits = timerIdDict.empty
      , timersSelectInput = ""
      , timersSelectState = Select.initState (Select.selectIdentifier "")
      }
    , Cmd.batch
        [ TimeZone.getZone
            |> Task.attempt (Result.Extra.unpack UpdateZoneError (Tuple.second >> UpdateZone))
        , case authentication of
            AuthenticationInitialized { username } ->
                fetchInitialState username

            AuthenticationUninitialized _ ->
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
                                    enqueueAll failed
                                        { model
                                            | errors = []
                                            , remote = base
                                            , pending = PendingIdle
                                            , page =
                                                case model.page of
                                                    Errors :: tail ->
                                                        tail

                                                    _ ->
                                                        model.page
                                        }

                                ( model2, cmd2 ) =
                                    enqueueAll (List.reverse queue) model1
                            in
                            ( model2, Cmd.batch [ cmd1, cmd2 ] )

        CalendarZoomStart zoomOperation ->
            ( model
            , Browser.Dom.getViewportOf ids.calendarScrollContainer
                |> Task.attempt (CalendarZoomWithViewport zoomOperation)
            )

        CalendarZoomWithViewport zoomOperation result ->
            let
                newZoomLevel =
                    max 0
                        (model.calendarZoomLevel
                            + (case zoomOperation of
                                ZoomIn ->
                                    1

                                ZoomOut ->
                                    -1

                                ZoomNow ->
                                    0
                              )
                        )

                initialPixelsPerHour =
                    pixelsPerHour model.calendarZoomLevel

                newPixelsPerHour =
                    pixelsPerHour newZoomLevel

                factor =
                    Quantity.ratio newPixelsPerHour initialPixelsPerHour

                ( error, cmd ) =
                    case result of
                        Ok { scene, viewport } ->
                            ( Nothing
                            , Browser.Dom.setViewportOf ids.calendarScrollContainer
                                0
                                (if zoomOperation == ZoomNow then
                                    case model.time of
                                        TimeInitialized { now, zone } ->
                                            let
                                                dayStart =
                                                    Time.Extra.floor Time.Extra.Day zone now

                                                dayEnd =
                                                    Time.Extra.ceiling Time.Extra.Day zone now

                                                percentThroughDay =
                                                    Quantity.ratio
                                                        (Duration.from dayStart now)
                                                        (Duration.from dayStart dayEnd)
                                            in
                                            (percentThroughDay * scene.height) - (viewport.height / 2)

                                        TimeUninitialized _ ->
                                            0

                                 else if viewport.y == 0 then
                                    0

                                 else if viewport.y + viewport.height == scene.height then
                                    -- this overshoots, but the browser will clamp and it allows us to avoid rounding errors bringing us slightly before the end of the view
                                    scene.height * factor

                                 else
                                    (viewport.y + (viewport.height / 2)) * factor - (viewport.height / 2)
                                )
                                |> Task.attempt CalendarZoomFinished
                            )

                        Err (Browser.Dom.NotFound id) ->
                            ( Just (DomNodeNotFound id), Cmd.none )
            in
            ( { model
                | calendarZoomLevel = newZoomLevel
                , errors =
                    case error of
                        Nothing ->
                            model.errors

                        Just e ->
                            e :: model.errors
                , historySelectedDate =
                    if zoomOperation == ZoomNow then
                        SelectedDate.unselected

                    else
                        model.historySelectedDate
              }
            , cmd
            )

        CalendarZoomFinished result ->
            case result of
                Ok () ->
                    ( model, Cmd.none )

                Err (Browser.Dom.NotFound id) ->
                    ( { model | errors = DomNodeNotFound id :: model.errors }, Cmd.none )

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

        HistoryEditCancel ->
            ( { model | historyEdit = Nothing }, Cmd.none )

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

        IncrementDate { days } ->
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

        Navigate page ->
            ( { model | page = page :: model.page }, Cmd.none )

        NavigateBack ->
            ( { model | page = List.drop 1 model.page }, Cmd.none )

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

        UsernameEdit input ->
            ( { model | authentication = AuthenticationUninitialized { usernameInput = input } }, Cmd.none )

        UsernameSubmit ->
            case model.authentication of
                AuthenticationUninitialized { usernameInput } ->
                    ( { model | authentication = AuthenticationInitialized { username = usernameInput } }
                    , Cmd.batch
                        [ fetchInitialState usernameInput
                        , localStorageWrites ( localStorageKeys.username, usernameInput )
                        ]
                    )

                AuthenticationInitialized _ ->
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
    case ( model.authentication, model.remote.timerSet ) of
        ( AuthenticationInitialized { username }, Just { version } ) ->
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
        [ Accessibility.Styled.toUnstyled
            (Accessibility.Styled.div
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


viewBody ({ authentication } as model) =
    [ viewFlexContainer
        { header =
            [ viewMenuIcon model
            , viewTotalsIcon
            , viewLoadingIcon model
            , viewErrorsIcon model
            ]
        , footer = []
        , body =
            case viewPage model of
                Nothing ->
                    [ Accessibility.Styled.text strings.loading ]

                Just content ->
                    content
        , scrollContainerId = Nothing
        , direction = Column
        }
    ]


type FlexDirection
    = Row
    | Column


viewFlexContainer { header, footer, body, scrollContainerId, direction } =
    Accessibility.Styled.div
        [ Html.Styled.Attributes.css
            [ Css.height (Css.pct 100)
            , Css.displayFlex
            , Css.flexDirection
                (case direction of
                    Row ->
                        Css.row

                    Column ->
                        Css.column
                )
            ]
        ]
        [ Accessibility.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.flexGrow Css.zero
                ]
            ]
            header
        , Accessibility.Styled.div
            (Html.Styled.Attributes.css
                [ Css.flexGrow (Css.num 1)
                , Css.property "flex-basis" "0"
                , Css.overflowY Css.auto
                ]
                :: (case scrollContainerId of
                        Nothing ->
                            []

                        Just id ->
                            [ Html.Styled.Attributes.id id ]
                   )
            )
            body
        , Accessibility.Styled.div
            [ Html.Styled.Attributes.css
                [ Css.flexGrow Css.zero
                ]
            ]
            footer
        ]


viewPage ({ authentication } as model) =
    case authentication of
        AuthenticationUninitialized _ ->
            viewAuthentication model

        AuthenticationInitialized _ ->
            viewPageAuthenticated model


viewPageAuthenticated ({ page, pending, remote, time } as model) =
    case Maybe.withDefault Calendar (List.head page) of
        Totals ->
            case ( time, remote.timerSet ) of
                ( TimeInitialized initializedTime, Just timerSet ) ->
                    Just (viewTotals initializedTime timerSet.value model)

                _ ->
                    Nothing

        Menu ->
            case ( time, remote.timerSet ) of
                ( TimeInitialized initializedTime, Just timerSet ) ->
                    Just (viewMenu initializedTime timerSet.value model)

                _ ->
                    Nothing

        Calendar ->
            case ( time, remote.timerSet ) of
                ( TimeInitialized initializedTime, Just timerSet ) ->
                    Just (viewCalendar initializedTime timerSet.value model)

                _ ->
                    Nothing

        Errors ->
            Just (viewErrors model)


viewMenuIcon { page } =
    viewIcon
        { onClick =
            Just
                (if page == [] then
                    Navigate Menu

                 else
                    NavigateBack
                )
        , content =
            if page == [] then
                Accessibility.Styled.text "☰"

            else
                Accessibility.Styled.div [ Html.Styled.Attributes.css [ Css.transform (Css.scaleX -1) ] ] [ Accessibility.Styled.text "➜" ]
        }


viewTotalsIcon =
    viewIcon
        { onClick =
            Just (Navigate Totals)
        , content =
            Accessibility.Styled.text "∑"
        }


viewLoadingIcon { pending } =
    let
        loading =
            case pending of
                PendingIdle ->
                    False

                Pending { current } ->
                    case current of
                        PendingOutstanding _ ->
                            True

                        PendingError _ ->
                            False
    in
    viewIcon
        { onClick = Nothing
        , content =
            Accessibility.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.Transitions.transition [ Css.Transitions.opacity (Duration.inMilliseconds durations.transition) ]
                    , Css.opacity
                        (if loading then
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
                ]
                [ Accessibility.Styled.text "↻" ]
        }


viewErrorsIcon { errors, pending } =
    let
        retryable =
            case pending of
                PendingIdle ->
                    False

                Pending { current } ->
                    case current of
                        PendingOutstanding _ ->
                            False

                        PendingError _ ->
                            True

        visible =
            retryable || errors /= []
    in
    viewIcon
        { onClick =
            if visible then
                Just (Navigate Errors)

            else
                Nothing
        , content =
            Accessibility.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.opacity
                        (if visible then
                            Css.num 100

                         else
                            Css.num 0
                        )
                    ]
                ]
                [ Accessibility.Styled.text "⚠" ]
        }


buttonSize =
    Pixels.pixels 24


viewIcon { onClick, content } =
    Accessibility.Styled.button
        ([ Html.Styled.Attributes.css
            [ Css.display Css.inlineBlock
            , Css.width (Css.px (Pixels.toFloat buttonSize))
            , Css.height (Css.px (Pixels.toFloat buttonSize))
            , Css.margin (Css.px 8)
            , Css.fontSize (Css.px (Pixels.toFloat buttonSize))
            , Css.textAlign Css.center
            , Css.lineHeight (Css.px (Pixels.toFloat buttonSize))
            , Css.backgroundColor Css.transparent
            , Css.border Css.zero
            , Css.color Css.inherit
            ]
         ]
            ++ (case onClick of
                    Just msg ->
                        [ Html.Styled.Events.onClick msg ]

                    Nothing ->
                        [ Html.Styled.Attributes.disabled True ]
               )
        )
        [ content ]


viewMenu { now } timerSet { timersEdits } =
    List.concat
        [ List.map (\id -> viewTimer now (timerIdDict.get id timersEdits) timerSet id) (TimerSet.listTimerIds timerSet)
        , if timerIdDict.isEmpty timersEdits then
            []

          else
            [ Accessibility.Styled.text "*" ]
        ]


viewErrors { errors } =
    errors
        |> List.map strings.error
        |> List.map (\error -> Accessibility.Styled.div [] [ Accessibility.Styled.text error ])
        |> (::) (viewIcon { onClick = Just ApiRetry, content = Accessibility.Styled.text "↺" })


viewAuthentication { authentication } =
    Just
        [ Accessibility.Styled.text (strings.enterUsernamePrompt ++ ": ")
        , Accessibility.Styled.inputText
            (case authentication of
                AuthenticationUninitialized { usernameInput } ->
                    usernameInput

                AuthenticationInitialized _ ->
                    ""
            )
            [ Html.Styled.Events.onInput UsernameEdit ]
        , Accessibility.Styled.button [ Html.Styled.Events.onClick UsernameSubmit ] [ Accessibility.Styled.text strings.submitUsername ]
        ]


viewPaused =
    Accessibility.Styled.text strings.paused


viewTimerSelect { now, zone } timerSet { timersSelectInput, timersSelectState } =
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
                |> List.filter (\timerId -> Just timerId /= currentTimer)

        label timerId =
            case TimerSet.get timerId timerSet of
                Nothing ->
                    strings.unknownTimer

                Just { name } ->
                    if name == "" then
                        strings.unnamedTimer

                    else
                        name
    in
    [ timers
        |> List.map
            (\timerId ->
                Select.basicMenuItem
                    { item = StartTimer timerId
                    , label = label timerId
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
        |> flip Select.menuItems
            (Select.single
                (Maybe.map
                    (\justCurrentTimer ->
                        Select.basicMenuItem
                            { item = StartTimer justCurrentTimer
                            , label = label justCurrentTimer
                            }
                    )
                    currentTimer
                )
            )
        |> Select.state timersSelectState
        |> Select.searchable True
        |> Select.view
        |> Accessibility.Styled.map TimerSelectMsg
    ]


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


endOfDay zone date =
    startOfDay zone (Date.add Date.Days 1 date)


viewHistoryEdit timerSet historyEdit =
    case historyEdit of
        Nothing ->
            []

        Just { originalTimerId, originalStart, originalEnd, zone } ->
            viewTimeSelect HistoryEditUpdateStartHours HistoryEditUpdateStartMinutes zone originalStart
                ++ [ Accessibility.Styled.text strings.historyEditRange ]
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
                   , Accessibility.Styled.button
                        [ Html.Styled.Events.onClick HistoryEditCommit
                        ]
                        [ Accessibility.Styled.text strings.historyEditConfirm ]
                   , Accessibility.Styled.button
                        [ Html.Styled.Events.onClick HistoryEditCancel
                        ]
                        [ Accessibility.Styled.text strings.historyEditCancel ]
                   ]


viewSelect onInput options =
    Accessibility.Styled.select
        [ Html.Styled.Events.onInput onInput
        ]
        (List.map
            (\option ->
                Accessibility.Styled.option
                    [ Html.Styled.Attributes.value option.value
                    , Html.Styled.Attributes.selected option.selected
                    ]
                    [ Accessibility.Styled.text option.label ]
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


pixelsPerHour calendarZoomLevel =
    Pixels.pixels 40
        |> Quantity.per Duration.hour
        |> Quantity.multiplyBy (2 ^ calendarZoomLevel |> toFloat)


type BlockTimerIds
    = BlockTimerIdsSingle (Maybe TimerSet.TimerId)
    | BlockTimerIdsMultiple { first : Maybe TimerSet.TimerId, second : Maybe TimerSet.TimerId, reversedRemainder : List (Maybe TimerSet.TimerId) }


viewCalendar ({ now, zone } as initializedTime) timerSet ({ calendarZoomLevel, historySelectedDate, historyEdit } as model) =
    let
        leftMargin =
            Pixels.pixels 60

        leftIndentMargin =
            leftMargin
                |> Quantity.plus (Pixels.pixels 24)

        minEventDuration =
            buttonSize
                |> Quantity.at_ (pixelsPerHour calendarZoomLevel)

        date =
            historySelectedDate
                |> SelectedDate.getDate now zone

        dayStart =
            startOfDay zone date

        dayEnd =
            endOfDay zone date

        history =
            TimerSet.history timerSet

        dailyHistory =
            history
                |> Timeline.fold []
                    (\{ start, duration, value } acc ->
                        case acc of
                            [] ->
                                [ { start = start, duration = duration, timerIds = BlockTimerIdsSingle value } ]

                            head :: tail ->
                                if duration |> Quantity.greaterThanOrEqualTo minEventDuration then
                                    { start = start, duration = duration, timerIds = BlockTimerIdsSingle value } :: head :: tail

                                else
                                    case head.timerIds of
                                        BlockTimerIdsSingle id ->
                                            if head.duration |> Quantity.greaterThanOrEqualTo minEventDuration then
                                                { start = start, duration = duration, timerIds = BlockTimerIdsSingle value } :: head :: tail

                                            else
                                                { start = head.start, duration = Quantity.plus duration head.duration, timerIds = BlockTimerIdsMultiple { first = id, second = value, reversedRemainder = [] } } :: tail

                                        BlockTimerIdsMultiple timerIds ->
                                            { start = head.start, duration = Quantity.plus duration head.duration, timerIds = BlockTimerIdsMultiple { timerIds | reversedRemainder = value :: timerIds.reversedRemainder } } :: tail
                    )
                    dayStart
                    (timeMin now dayEnd)

        rules =
            Time.Extra.range Time.Extra.Hour 1 zone dayStart (Time.Extra.add Time.Extra.Millisecond 1 zone dayEnd)

        timeChange =
            rules
                |> List.map (\hour -> Time.Extra.toOffset zone hour)
                |> List.Extra.unique
                |> List.length
                |> flip (>) 1

        labels =
            rules
                |> List.map
                    (\hour ->
                        let
                            twentyFourHour =
                                Time.toHour zone hour

                            timeLabel =
                                if twentyFourHour == 0 then
                                    "12am"

                                else if twentyFourHour < 12 then
                                    String.fromInt twentyFourHour ++ "am"

                                else if twentyFourHour == 12 then
                                    "12pm"

                                else
                                    String.fromInt (twentyFourHour - 12) ++ "pm"

                            offsetLabel =
                                if timeChange then
                                    let
                                        offsetMinutes =
                                            Time.Extra.toOffset zone hour

                                        offsetHours =
                                            toFloat offsetMinutes / 60
                                    in
                                    " (UTC"
                                        ++ (if offsetHours >= 0 then
                                                "+"

                                            else
                                                ""
                                           )
                                        ++ String.fromFloat offsetHours
                                        ++ ")"

                                else
                                    ""
                        in
                        Tuple.pair hour (timeLabel ++ offsetLabel)
                    )

        pixelOffset time =
            Duration.from dayStart time
                |> Quantity.at (pixelsPerHour calendarZoomLevel)

        block start duration left color title zIndex onClick =
            let
                top =
                    pixelOffset start

                height =
                    Duration.addTo start duration |> pixelOffset |> Quantity.minus top
            in
            Accessibility.Styled.button
                (Html.Styled.Attributes.css
                    [ Css.position Css.absolute
                    , Css.displayFlex
                    , Css.top (Css.px (top |> Pixels.toFloat))
                    , Css.height (Css.px (height |> Pixels.toFloat))
                    , Css.left (Css.px (Pixels.toFloat left))
                    , Css.right Css.zero
                    , Css.backgroundColor color
                    , Css.overflow Css.hidden
                    , Css.fontSize (Css.px (Pixels.toFloat buttonSize))
                    , Css.lineHeight (Css.px (Pixels.toFloat buttonSize))
                    , Css.border Css.zero
                    , Css.color Css.inherit
                    , Css.textAlign Css.start
                    , Css.zIndex (Css.int zIndex)
                    ]
                    :: (case onClick of
                            Just msg ->
                                [ Html.Styled.Events.onClick msg ]

                            Nothing ->
                                []
                       )
                )
                [ Accessibility.Styled.text title ]
    in
    [ viewFlexContainer
        { header =
            [ Accessibility.Styled.div [ Html.Styled.Attributes.css [ Css.textAlign Css.center ] ]
                [ viewIcon
                    { onClick = Just (IncrementDate { days = -1 })
                    , content = Accessibility.Styled.text "<"
                    }
                , historySelectedDate
                    |> SelectedDate.getDate now zone
                    |> Date.toIsoString
                    |> Accessibility.Styled.text
                , viewIcon
                    { onClick =
                        Just (CalendarZoomStart ZoomIn)
                    , content =
                        Accessibility.Styled.text "+"
                    }
                , viewIcon
                    { onClick =
                        Just (CalendarZoomStart ZoomNow)
                    , content =
                        Accessibility.Styled.text "⌖"
                    }
                , viewIcon
                    { onClick =
                        Just (CalendarZoomStart ZoomOut)
                    , content =
                        Accessibility.Styled.text "-"
                    }
                , viewIcon
                    { onClick = Just (IncrementDate { days = 1 })
                    , content = Accessibility.Styled.text ">"
                    }
                ]
            ]
                ++ viewTimerSelect initializedTime timerSet model
                ++ viewHistoryEdit timerSet historyEdit
        , footer = []
        , body =
            [ Accessibility.Styled.div
                [ Html.Styled.Attributes.css
                    [ Css.height (Css.px 960)
                    , Css.position Css.relative
                    ]
                ]
                ((labels
                    |> List.drop 1
                    |> List.map
                        (\( hour, label ) ->
                            Accessibility.Styled.div
                                [ Html.Styled.Attributes.css
                                    [ Css.position Css.absolute
                                    , Css.width (Css.pct 100)
                                    , Css.bottom
                                        (Css.calc (Css.pct 100)
                                            Css.minus
                                            (Css.px (Pixels.toFloat (pixelOffset hour)))
                                        )
                                    , Css.borderBottomWidth (Css.px 1)
                                    , Css.borderBottomStyle Css.solid
                                    , Css.borderBottomColor colors.gridline
                                    ]
                                ]
                                [ Accessibility.Styled.text label ]
                        )
                 )
                    ++ (dailyHistory
                            |> List.map
                                (\{ start, duration, timerIds } ->
                                    let
                                        blockStart =
                                            if duration |> Quantity.lessThan minEventDuration then
                                                minEventDuration
                                                    |> Quantity.minus duration
                                                    |> Quantity.divideBy 2
                                                    |> Duration.subtractFrom start
                                                    |> posixMin (Duration.subtractFrom dayEnd minEventDuration)
                                                    |> posixMax dayStart

                                            else
                                                start

                                        blockDuration =
                                            Quantity.max minEventDuration duration

                                        margin =
                                            if duration |> Quantity.lessThan minEventDuration then
                                                leftIndentMargin

                                            else
                                                leftMargin

                                        color =
                                            case timerIds of
                                                BlockTimerIdsSingle Nothing ->
                                                    colors.paused

                                                BlockTimerIdsSingle (Just id) ->
                                                    colors.jewel (TimerSet.timerIdToRaw id)

                                                BlockTimerIdsMultiple _ ->
                                                    colors.cluster

                                        name =
                                            case timerIds of
                                                BlockTimerIdsSingle id ->
                                                    timerDisplayName timerSet id

                                                BlockTimerIdsMultiple { first, second, reversedRemainder } ->
                                                    first
                                                        :: second
                                                        :: (reversedRemainder |> List.reverse)
                                                        |> List.map (\id -> timerDisplayName timerSet id)
                                                        |> strings.list

                                        action =
                                            case timerIds of
                                                BlockTimerIdsSingle id ->
                                                    Just (HistoryEditStart { timerId = id, start = start, end = Duration.addTo start duration })

                                                BlockTimerIdsMultiple _ ->
                                                    Just (CalendarZoomStart ZoomIn)

                                        zIndex =
                                            if duration |> Quantity.lessThan minEventDuration then
                                                1

                                            else
                                                0
                                    in
                                    block blockStart blockDuration margin color name zIndex action
                                )
                       )
                )
            ]
        , scrollContainerId = Just ids.calendarScrollContainer
        , direction = Column
        }
    ]


posixMin a b =
    Time.millisToPosix (min (Time.posixToMillis a) (Time.posixToMillis b))


posixMax a b =
    Time.millisToPosix (max (Time.posixToMillis a) (Time.posixToMillis b))


viewTotals { now, zone } timerSet { historySelectedDate, historyEdit } =
    let
        date =
            historySelectedDate
                |> SelectedDate.getDate now zone

        dayStart =
            startOfDay zone date

        dayEnd =
            endOfDay zone date

        history =
            TimerSet.history timerSet

        viewTotalLine text predicate =
            Accessibility.Styled.div []
                [ viewDuration (sumDuration predicate dayStart (timeMin now dayEnd) history)
                , Accessibility.Styled.text (" " ++ text)
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
    [ Accessibility.Styled.h2 []
        [ historySelectedDate
            |> SelectedDate.getDate now zone
            |> Date.toIsoString
            |> Accessibility.Styled.text
        ]
    ]
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
    Accessibility.Styled.button
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
        [ Accessibility.Styled.text text
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
            Accessibility.Styled.div [] [ Accessibility.Styled.text strings.unknownTimer ]

        Just { name, activity, category } ->
            Accessibility.Styled.div []
                [ Accessibility.Styled.inputText
                    (case edit of
                        Nothing ->
                            name

                        Just nameEdit ->
                            nameEdit.name
                    )
                    [ Html.Styled.Attributes.placeholder strings.unnamedTimer
                    , Html.Styled.Events.onInput (\updatedName -> TimerEditRename { timerId = id, name = updatedName })
                    , Html.Styled.Events.onBlur (TimerEditCommit id)
                    ]
                , Accessibility.Styled.text " "
                , viewActCatToggle TimerToggleActivity id activity TimerSet.Active strings.abbreviationActive
                , viewActCatToggle TimerToggleActivity id activity TimerSet.Reactive strings.abbreviationReactive
                , viewActCatToggle TimerToggleActivity id activity TimerSet.Proactive strings.abbreviationProactive
                , Accessibility.Styled.text " "
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
    Accessibility.Styled.span []
        [ Accessibility.Styled.text (pad hours ++ ":" ++ pad minutes ++ ":" ++ pad seconds)
        , Accessibility.Styled.small [] [ Accessibility.Styled.text ("." ++ pad hundredths) ]
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
    Accessibility.Styled.span []
        [ Accessibility.Styled.text (String.fromInt hours ++ ":" ++ pad minutes ++ ":" ++ pad seconds)
        , Accessibility.Styled.small [] [ Accessibility.Styled.text ("." ++ pad hundredths) ]
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
    , emeraldGreen = Css.hex "50C878"
    , sapphireBlue = Css.hex "0F52BA"
    , amethystPurple = Css.hex "9B30FF"
    , rubyRed = Css.hex "DF0101"
    , topazYellow = Css.hex "FFC125"
    , turquoise = Css.hex "40E0D0"
    , charcoalGrey = Css.hex "36454F"
    }


colors =
    { gridline = rawColors.silver
    , running = rawColors.spanishGreen
    , paused = rawColors.blackCoral
    , toggleOff = rawColors.silver
    , toggleOn = rawColors.middleBlue
    , jewel =
        \index ->
            case index |> modBy 6 of
                0 ->
                    rawColors.emeraldGreen

                1 ->
                    rawColors.sapphireBlue

                2 ->
                    rawColors.amethystPurple

                3 ->
                    rawColors.rubyRed

                4 ->
                    rawColors.topazYellow

                _ ->
                    rawColors.turquoise
    , cluster = rawColors.charcoalGrey
    }


strings =
    { appTitle = "Timetrack"
    , calendarZoomIn = "+"
    , calendarZoomOut = "-"
    , unnamedTimer = "Unnamed Timer"
    , enterUsernamePrompt = "Enter username"
    , submitUsername = "Submit"
    , paused = "Paused"
    , unknownTimer = "Unknown Timer"
    , previous = "prev"
    , next = "next"
    , abbreviationActive = "A"
    , abbreviationReactive = "R"
    , abbreviationProactive = "P"
    , abbreviationOperational = "O"
    , abbreviationHelpful = "H"
    , abbreviationProductive = "P"
    , total = "Total"
    , history = "History"
    , historyEditRange = " to "
    , historyEditCancel = "Cancel"
    , historyEditConfirm = "Confirm"
    , loading = "Loading..."
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

                DomNodeNotFound id ->
                    "Dom node not found: " ++ id
    , newTimer = "New timer: "
    , list = \values -> String.join ", " values
    }


durations =
    { spinnerRotation = Duration.milliseconds 800
    , transition = Duration.milliseconds 150
    }


ids =
    { calendarScrollContainer = "a"
    }


localStorageKeys =
    { username = "username"
    }


timerIdDict =
    GenericDict.makeInterface TimerSet.compareTimerId


flip f a b =
    f b a
