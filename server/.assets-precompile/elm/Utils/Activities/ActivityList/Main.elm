module Utils.Activities.ActivityList.Main
    exposing
        ( Msg
        , ActivityList
        , init
        , update
        , view
        )

import Utils.Activities.ActivityList.Types
import Utils.Activities.ActivityList.State
import Utils.Activities.ActivityList.View


type alias Msg =
    Utils.Activities.ActivityList.Types.Msg


type alias ActivityList =
    Utils.Activities.ActivityList.Types.ActivityList


init =
    Utils.Activities.ActivityList.State.init


update =
    Utils.Activities.ActivityList.State.update


view =
    Utils.Activities.ActivityList.View.view
