module Topbar.View exposing (..)

import Topbar.Types exposing (..)
import Topbar.TimeTracker.Main as TimeTracker
import Html exposing (Html)


root : Model -> Html Msg
root model =
    TimeTracker.view model.timeTracker
        |> Html.map TimeTrackerMsg
