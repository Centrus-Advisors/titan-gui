module Project.Details.Types exposing (..)

import Common.Types
    exposing
        ( Project
        , ProjectType
        , UserS
        , ContactS
        , CompanyS
        , ProjectContact
        , Position
        , PositionRate
        , Cost
        )
import Project.Details.NewContact as NewContact
import RemoteData exposing (RemoteData, WebData)
import Date exposing (Date)
import DatePicker
import Utils.SearchBox as SearchBox
import Utils.FileStorage.Main as FileStorage
import Utils.FileStorage.Dropbox as Dropbox


type alias Model =
    { project : WebData ProjectWithPermission
    , cost : WebData Cost
    , editable : Maybe EditableProject
    , submission : WebData ProjectWithPermission
    , deletion : WebData ()
    , reportModal : Maybe ReportModalState
    , fileStorage : FileStorage.FileStorage
    }


type ProjectWithPermission
    = Editable Project
    | NonEditable Project


type alias ProjectID =
    String


type Msg
    = DoNothing
    | CostFetch (WebData Cost)
    | ProjectFetch (WebData ProjectWithPermission)
    | Change FieldMsg
    | SetEditing Bool
    | Save EditableProject
    | SubmissionUpdate (WebData ProjectWithPermission)
    | Delete EditableProject
    | DeletionUpdate (WebData ())
    | ShowReportModal Bool
    | ReportModalFromDate DatePicker.Msg
    | ReportModalToDate DatePicker.Msg
    | FileStorageMsg FileStorage.Msg


type FieldMsg
    = Name String
    | Description String
    | Type String
    | StartDate DatePicker.Msg
    | EndDate DatePicker.Msg
    | Budget String
    | Notes String
    | CompanySearch (SearchBox.Msg CompanyS)
    | OwnerSearch (SearchBox.Msg UserS)
    | Contacts ContactsMsg
    | Rates RatesMsg
    | RequiredFiles RequiredFilesMsg
    | NewContact
    | NewContactCancel
    | NewContactUpdate NewContact.Msg
    | NewContactCreated ContactS
    | ToggleDeletionModal Bool


type alias Index =
    Int


type RequiredFilesMsg
    = FAdd
    | FRemove Index
    | FName Index String


type RatesMsg
    = RAdd
    | RRemove Index
    | RHourlyRate Index String
    | RSearch Index (SearchBox.Msg Position)


type ContactsMsg
    = CAdd
    | CRemove Index
    | CDescription Index String
    | CSearch Index (SearchBox.Msg ContactS)


type alias EditableProject =
    { id : String
    , name : String
    , description : String
    , type_ : ProjectType
    , startDate : DatePicker.DatePicker
    , endDate : DatePicker.DatePicker
    , budget : String
    , requiredFiles : List String
    , notes : String
    , company : SearchBox.SearchBox CompanyS
    , owner : SearchBox.SearchBox UserS
    , rates : List EditablePositionRate
    , created_by : UserS
    , contacts : List EditableRelatedContact
    , newContact : Maybe NewContact.Model
    , deletionModalShowing : Bool
    }


type alias EditableRelatedContact =
    { description : String
    , contact : SearchBox.SearchBox ContactS
    }


type alias EditablePositionRate =
    { hourly_rate : String
    , position : SearchBox.SearchBox Position
    }


type alias ReportModalState =
    { fromDate : DatePicker.DatePicker
    , toDate : DatePicker.DatePicker
    }
