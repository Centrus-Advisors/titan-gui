module NewDeliverable.Types exposing (..)

import Common.Types
    exposing
        ( UserS
        , ContactS
        , ProjectS
        , DeliverableStructure
        , DeliverableStatus(Completed, Cancelled, Active)
        , DeliverableType
        )
import Date exposing (Date)
import Table
import DatePicker
import RemoteData exposing (WebData)
import Utils.SearchBox as SearchBox


type alias Config msg =
    { onUpdate : Msg -> msg
    , onCancel : msg
    , onCreated : Deliverable -> msg
    }


type alias Model msg =
    { newDeliverable : NewDeliverable
    , submission : WebData Deliverable
    , config : Config msg
    }


type alias Deliverable =
    Common.Types.Deliverable


type alias Project =
    ProjectS


type Msg
    = Change FieldMsg
    | Cancel
    | Create NewDeliverable
    | CreationUpdate (WebData Deliverable)


type FieldMsg
    = Title String
    | Milestone String
    | DType DeliverableType
    | StartDate DatePicker.Msg
    | Deadline DatePicker.Msg
    | Owner (SearchBox.Msg UserS)
    | DProject (SearchBox.Msg Project)


type alias NewDeliverable =
    { project : SearchBox.SearchBox ProjectS
    , title : String
    , milestone : String
    , type_ : DeliverableType
    , startDate : DatePicker.DatePicker
    , deadline : DatePicker.DatePicker
    , owner : SearchBox.SearchBox UserS
    }
