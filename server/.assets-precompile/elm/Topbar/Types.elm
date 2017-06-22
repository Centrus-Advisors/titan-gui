module Topbar.Types exposing (..)

import Topbar.TimeTracker.Main as TimeTracker


type alias Model =
    { timeTracker : TimeTracker.TimeTracker
    }


type Msg
    = TimeTrackerMsg TimeTracker.Msg
