module Deliverable.TimeTracking.Main
    exposing
        ( TimeTracking
        , Msg
        , view
        , update
        , init
        )

import Deliverable.TimeTracking.View
import Deliverable.TimeTracking.Types
import Deliverable.TimeTracking.State


type alias TimeTracking =
    Deliverable.TimeTracking.Types.Model


type alias Msg =
    Deliverable.TimeTracking.Types.Msg


update =
    Deliverable.TimeTracking.State.update


init =
    Deliverable.TimeTracking.State.init


view =
    Deliverable.TimeTracking.View.view
