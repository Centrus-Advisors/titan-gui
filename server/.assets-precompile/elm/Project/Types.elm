module Project.Types exposing (..)

import Common.Types exposing (Project)
import Project.Details.Details as Details
import RemoteData exposing (RemoteData, WebData)
import Project.Deliverables.Deliverables as Deliverables
import Http
import Utils.Ui
import Time exposing (Time)
import Date exposing (Date)
import Utils.Activities.Main as Activities


type alias Model =
    { projectID : String
    , details : Details.Model
    , deliverables : Deliverables.Model
    , activities : Activities.Activities
    , tab : Tab
    , todayDate : Date
    }


type Msg
    = ChangeTab Tab
    | DetailsMsg Details.Msg
    | DeliverablesMsg Deliverables.Msg
    | ActivitiesMsg Activities.Msg


type Tab
    = DetailsTab
    | MilestonesTab
    | DeliverablesTab
