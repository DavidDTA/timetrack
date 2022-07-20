module SelectedDate exposing (SelectedDate, fromDate, getDate, unselected)

import Date


type SelectedDate
    = Unselected
    | SpecificDate Date.Date


unselected =
    Unselected


fromDate now zone date =
    if date == Date.fromPosix zone now then
        Unselected

    else
        SpecificDate date


getDate now zone selectedDate =
    case selectedDate of
        Unselected ->
            Date.fromPosix zone now

        SpecificDate date ->
            date
