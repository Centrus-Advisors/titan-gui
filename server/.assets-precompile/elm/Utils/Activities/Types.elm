module Utils.Activities.Types exposing (..)

import RemoteData exposing (WebData)
import Date exposing (Date)
import Utils.Api as Api
import Utils.SearchBox as SearchBox
import DatePicker
import Date exposing (Date)
import Utils.Activities.CommonTypes exposing (..)
import Utils.Activities.ActivityList.Main as ActivityList
import Utils.Activities.Filters.Main as Filters
import Common.Types exposing (ContactS, ProjectS)


type Activities
    = Activities Model


type alias Model =
    { activityList : ActivityList.ActivityList
    , filters : Filters.Filters
    , sourceID : SourceID
    , sourceDocument : WebData SourceDocument
    , todayDate : Date
    , newActivity : Maybe EditableActivity
    , newActivitySubmission : WebData Activity
    , newTag : Maybe String
    , newTagSubmission : WebData ActivityTag
    }


type Msg
    = ActivityListMsg ActivityList.Msg
    | SourceDocumentUpdate (WebData SourceDocument)
    | FiltersMsg Filters.Msg
    | CreateNewActivity
    | CancelNewActivity
    | NewActivity NewActivityMsg
    | NewActivitySubmit
    | NewActivitySubmissionUpdate (WebData Activity)
    | NewTagCreate
    | NewTagCancel
    | NewTagSubmit
    | NewTagChange String
    | NewTagSubmissionUpdate (WebData ActivityTag)


type NewActivityMsg
    = ChangeDate DatePicker.Msg
    | ChangeDescription String
    | ChangeCategory String
    | SearchTag (SearchBox.Msg ActivityTag)
    | SearchContact (SearchBox.Msg ContactS)
    | SearchProject (SearchBox.Msg ProjectS)
    | RemoveContact ContactS
    | RemoveTag ActivityTag
