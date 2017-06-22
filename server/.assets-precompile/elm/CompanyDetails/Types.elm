module CompanyDetails.Types exposing (..)

import Common.Types
import RemoteData exposing (..)
import Http
import Utils.Activities.Main as Activities
import Utils.FileStorage.Main as FileStorage


type alias Model =
    { company : WebData Company
    , editableCompany : Maybe EditableCompany
    , submission : RemoteData Http.Error Company
    , activities : Activities.Activities
    , fileStorage : FileStorage.FileStorage
    }


type alias Company =
    Common.Types.Company


type alias Contact =
    Common.Types.ContactS


type alias Phone =
    Common.Types.Phone


type alias EditableCompany =
    { id : String
    , name : String
    , location : String
    , phones : List Phone
    , address : String
    , postcode : String
    , email : String
    , sector : String
    , classification : String
    , investorClassification : String
    , investorType : String
    , website : String
    , requiredFiles : List String
    , relatedContacts : List EditableCompanyContact
    , archived : Bool
    , notes : String
    }


type alias CompanyContact =
    { relation : String
    , contact : Maybe Contact
    }


type alias EditableCompanyContact =
    { relation : String
    , contact : Maybe Contact
    , search : SuggestionSearch Contact
    }


type alias SuggestionSearch a =
    { suggestions : RemoteData Http.Error (List a)
    , text : String
    }


type alias Index =
    Int


type Msg
    = DoNothing
    | FetchCompanyUpdate (WebData Company)
    | UpdateField CompanyFieldUpdate
    | EnableEditing Company
    | SaveEditing EditableCompany
    | CancelEditing
    | SubmissionUpdate (RemoteData Http.Error Company)
    | ActivitiesMsg Activities.Msg
    | FileStorageMsg FileStorage.Msg


type CompanyFieldUpdate
    = CompanyName String
    | CompanyLocation String
    | CompanyAddPhone
    | CompanyRemovePhone Int
    | CompanyUpdatePhone Int String String
    | CompanyAddress String
    | CompanyPostcode String
    | CompanyEmail String
    | CompanySector String
    | CompanyClassification String
    | CompanyInvestorClassification String
    | CompanyInvestorType String
    | CompanyWebsite String
    | CompanyArchived Bool
    | CompanyNotes String
    | RContact Index RelatedContactEvent
    | RequiredFiles RequiredFilesMsg


type RelatedContactEvent
    = Search String
    | SearchUpdate (RemoteData Http.Error (List Contact))
    | ChooseContact (Maybe Contact)
    | BlurSearchBox
    | SetRelation String
    | RemoveContact
    | AddContact


type RequiredFilesMsg
    = FAdd
    | FRemove Index
    | FName Index String
