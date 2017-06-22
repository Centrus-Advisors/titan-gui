module Utils.Activities.Main
    exposing
        ( Activities
        , Msg
        , initFromProject
        , initFromCompany
        , initFromContact
        , initFromUser
        , initEmpty
        , update
        , view
        )

import Utils.Activities.Types
import Utils.Activities.State
import Html exposing (text)
import Utils.Activities.View


type alias Msg =
    Utils.Activities.Types.Msg


type alias Activities =
    Utils.Activities.Types.Activities


initFromProject =
    Utils.Activities.State.initFromProject


initFromCompany =
    Utils.Activities.State.initFromCompany


initFromContact =
    Utils.Activities.State.initFromContact


initFromUser =
    Utils.Activities.State.initFromUser


initEmpty =
    Utils.Activities.State.initEmpty


update =
    Utils.Activities.State.update


view =
    Utils.Activities.View.view
