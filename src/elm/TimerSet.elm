module TimerSet exposing (Activity(..), Category(..), Timer, TimerId, TimerSet, addTimer, create, get, history, keyTimerId, listTimerIds, reset, startTimer, timerIdFromRaw, timerIdToRaw, updateTimer)

import Duration
import List.Extra
import Quantity
import Time
import Timeline


type TimerSet
    = TimerSet
        { timers : List Timer
        , history : Timeline.Timeline TimerId
        }


type Activity
    = Active
    | Reactive
    | Proactive


type Category
    = Operational
    | Helpful
    | Productive


type alias Timer =
    { name : String
    , activity : Maybe Activity
    , category : Maybe Category
    }


type TimerId
    = TimerId Int


timerIdFromRaw : Int -> TimerId
timerIdFromRaw =
    TimerId


timerIdToRaw : TimerId -> Int
timerIdToRaw (TimerId id) =
    id


create : List Timer -> Timeline.Timeline TimerId -> TimerSet
create timers history_ =
    TimerSet
        { timers = timers
        , history = history_
        }


listTimerIds : TimerSet -> List TimerId
listTimerIds (TimerSet { timers }) =
    List.indexedMap (\index _ -> TimerId index) timers


get : TimerId -> TimerSet -> Maybe Timer
get (TimerId id) (TimerSet { timers }) =
    List.Extra.getAt id timers


history : TimerSet -> Timeline.Timeline TimerId
history (TimerSet timerSet) =
    timerSet.history


addTimer : TimerSet -> ( TimerSet, TimerId )
addTimer (TimerSet timerSet) =
    ( TimerSet
        { timerSet
            | timers =
                timerSet.timers
                    ++ [ { name = ""
                         , activity = Nothing
                         , category = Nothing
                         }
                       ]
        }
    , TimerId (List.length timerSet.timers)
    )


updateTimer : TimerId -> (Timer -> Timer) -> TimerSet -> TimerSet
updateTimer (TimerId id) update (TimerSet timerSet) =
    TimerSet
        { timerSet
            | timers =
                timerSet.timers
                    |> List.Extra.updateAt id update
        }


reset : TimerSet -> TimerSet
reset (TimerSet timerSet) =
    TimerSet { timers = [], history = Timeline.empty }


startTimer : Maybe TimerId -> Time.Posix -> TimerSet -> TimerSet
startTimer timerId now (TimerSet timerSet) =
    TimerSet
        { timerSet
            | history = Timeline.set timerId now Nothing timerSet.history
        }


keyTimerId : TimerId -> List Int
keyTimerId (TimerId id) =
    [ id ]
