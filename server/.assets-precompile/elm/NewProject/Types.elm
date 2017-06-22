module NewProject.Types exposing (..)

import Utils.SearchBox as SearchBox
import RemoteData exposing (WebData)
import Common.Types exposing (CompanyS, UserS, ProjectType, ProjectS)
import DatePicker exposing (DatePicker)
import Date exposing (Date)


type alias Model =
    { modal : Maybe NewProject
    , submission : WebData ProjectS
    , todayDate : Date
    }


type alias NewProject =
    { name : String
    , type_ : ProjectType
    , startDate : DatePicker.DatePicker
    , endDate : DatePicker.DatePicker
    , company : SearchBox.SearchBox CompanyS
    , owner : SearchBox.SearchBox UserS
    , budget : String
    , description : String
    }


type Msg
    = Submit
    | SubmissionUpdate (WebData ProjectS)
    | GoToProjectPage ProjectS
    | OpenModal
    | CloseModal
    | Change ChangeMsg


type ChangeMsg
    = PName String
    | PType String
    | PStartDate DatePicker.Msg
    | PEndDate DatePicker.Msg
    | PCompany (SearchBox.Msg CompanyS)
    | POwner (SearchBox.Msg UserS)
    | PBudget String
    | PDescription String
