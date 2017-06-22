module Utils.Activities.Filters.Main
    exposing
        ( Msg
        , Filters
        , update
        , view
        , init
        , getPartialSearch
        )

import Utils.Activities.Filters.Types
import Utils.Activities.Filters.State
import Utils.Activities.Filters.View


type alias Msg =
    Utils.Activities.Filters.Types.Msg


type alias Filters =
    Utils.Activities.Filters.Types.Filters


init =
    Utils.Activities.Filters.State.init


update =
    Utils.Activities.Filters.State.update


getPartialSearch =
    Utils.Activities.Filters.State.getPartialSearch


view =
    Utils.Activities.Filters.View.view
