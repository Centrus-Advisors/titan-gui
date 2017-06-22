module ContactDetails.Types exposing (..)

import RemoteData exposing (..)
import Http
import Utils.Activities.Main as Activities
import Common.Types exposing (Contact, CompanyS)
import Utils.FileStorage.Main as FileStorage


type alias Model =
    { loadedModel : RemoteData Http.Error LoadedModel
    , activities : Activities.Activities
    , fileStorage : FileStorage.FileStorage
    }


type alias LoadedModel =
    { contact : Contact
    , editableContact : Maybe Contact
    , submission : RemoteData Http.Error Contact
    , companySearch : SuggestionSearch CompanyS
    }


type alias SuggestionSearch a =
    { suggestions : RemoteData Http.Error (List a)
    , text : String
    }


type Msg
    = ContactFetch (RemoteData Http.Error Contact)
    | UpdateField ContactFieldUpdate
    | EnableEditing
    | SaveEditing
    | CancelEditing
    | SubmissionUpdate (RemoteData Http.Error Contact)
    | SearchCompany String
    | SearchCompanyUpdate (RemoteData Http.Error (List CompanyS))
    | SearchCompanyBoxBlur
    | ChooseCompany (Maybe CompanyS)
    | ActivitiesMsg Activities.Msg
    | FileStorageMsg FileStorage.Msg


type ContactFieldUpdate
    = ContactName String
    | ContactPosition String
    | ContactAddPhone
    | ContactRemovePhone Int
    | ContactUpdatePhone Int String String
    | ContactEmail String
    | ContactLocation String
    | ContactGender String
    | ContactAddress String
    | ContactNotes String
    | RequiredFiles RequiredFilesMsg


type alias Index =
    Int


type RequiredFilesMsg
    = FAdd
    | FRemove Index
    | FName Index String
