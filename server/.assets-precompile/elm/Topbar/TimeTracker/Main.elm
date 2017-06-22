module Topbar.TimeTracker.Main
    exposing
        ( view
        , TimeTracker
        , Msg
        , init
        , update
        , subscriptions
        )

import Topbar.TimeTracker.View
import Topbar.TimeTracker.Types
import Topbar.TimeTracker.State


view =
    Topbar.TimeTracker.View.root


update =
    Topbar.TimeTracker.State.update


init =
    Topbar.TimeTracker.State.init


subscriptions =
    Topbar.TimeTracker.State.subscriptions


type alias TimeTracker =
    Topbar.TimeTracker.Types.Model


type alias Msg =
    Topbar.TimeTracker.Types.Msg
