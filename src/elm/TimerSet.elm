module TimerSet exposing (Activity(..), Category(..), Timer, TimerId, TimerSet, addTimer, compareTimerId, create, empty, get, history, listTimerIds, setTimer, timerIdFromRaw, timerIdToRaw, updateTimer)

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


compareTimerId (TimerId a) (TimerId b) =
    compare a b


timerIdFromRaw : Int -> TimerId
timerIdFromRaw =
    TimerId


timerIdToRaw : TimerId -> Int
timerIdToRaw (TimerId id) =
    id


empty : TimerSet
empty =
    create [] Timeline.empty


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


addTimer : String -> TimerSet -> ( TimerSet, TimerId )
addTimer name (TimerSet timerSet) =
    ( TimerSet
        { timerSet
            | timers =
                timerSet.timers
                    ++ [ { name = name
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


setTimer : Maybe TimerId -> Time.Posix -> Maybe Time.Posix -> TimerSet -> TimerSet
setTimer timerId start end (TimerSet timerSet) =
    TimerSet
        { timerSet
            | history = Timeline.set timerId start end timerSet.history
        }
