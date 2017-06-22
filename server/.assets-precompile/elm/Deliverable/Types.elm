module Deliverable.Types exposing (..)

import Common.Types
    exposing
        ( Deliverable
        , UserS
        , CompanyS
        , ProjectS
        , DeliverableStatus(Completed, Cancelled, Active)
        , Cost
        , DeliverableType
        )
import Deliverable.TimeTracking.Main as TimeTracking
import RemoteData exposing (WebData)
import Date exposing (Date)
import DatePicker
import RemoteData exposing (WebData)
import Utils.SearchBox as SearchBox


type alias DeliverableID =
    String


type Msg
    = TimeTrackingMsg TimeTracking.Msg
    | Reload DeliverableID
    | FetchDeliverableUpdate (WebData DeliverableEdit)
    | FetchCostUpdate (WebData Cost)
    | SetEditing Bool
    | SubmissionUpdate (WebData DeliverableEdit)
    | Submit EditableDeliverable
    | Delete EditableDeliverable
    | DeletionUpdate (WebData ())
    | Change FieldMsg


type FieldMsg
    = Title String
    | Type DeliverableType
    | Milestone String
    | Description String
    | Status String
    | HourlyRate String
    | StartDate DatePicker.Msg
    | Deadline DatePicker.Msg
    | Owner (SearchBox.Msg UserS)
    | Assignees AssigneesMsg
    | ToggleDeletionModal Bool


type alias Index =
    Int


type AssigneesMsg
    = AAdd
    | ARemove Index
    | ASearch Index (SearchBox.Msg UserS)


type alias Model =
    { timeTracking : TimeTracking.TimeTracking
    , deliverable : WebData DeliverableEdit
    , cost : WebData Cost
    , todayDate : Date
    , submission : WebData DeliverableEdit
    , deletion : WebData ()
    , editable : Maybe EditableDeliverable
    }


type DeliverableEdit
    = Editable Deliverable
    | NonEditable Deliverable


type alias EditableDeliverable =
    { id : String
    , title : String
    , type_ : DeliverableType
    , milestone : String
    , description : String
    , status : DeliverableStatus
    , hourly_rate : String
    , startDate : DatePicker.DatePicker
    , deadline : DatePicker.DatePicker
    , owner : SearchBox.SearchBox UserS
    , assignees : List (SearchBox.SearchBox UserS)
    , project : ProjectS
    , created_by : UserS
    , deletionModalShowing : Bool
    }
