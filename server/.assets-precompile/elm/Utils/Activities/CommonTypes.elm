module Utils.Activities.CommonTypes exposing (..)

import Date exposing (Date)
import Utils.SearchBox as SearchBox
import DatePicker
import Common.Types exposing (ContactS, ProjectS, UserStructure)


type alias Activity =
    { id : String
    , date : Date
    , description : String
    , category : String
    , tags : List ActivityTag
    , contacts : List ContactS
    , project : Maybe ProjectS
    , created_by : UserStructure String ContactS String
    }


type alias EditableActivity =
    { sourceDocument : SourceDocument
    , date : DatePicker.DatePicker
    , description : String
    , category : ActivityCategory
    , tags : List ActivityTag
    , tagSearch : SearchBox.SearchBox ActivityTag
    , contacts : List ContactS
    , contactSearch : SearchBox.SearchBox ContactS
    , project : SearchBox.SearchBox ProjectS
    }



-- NoSource is used for users without contacts


type SourceID
    = ProjectID String
    | ContactID String
    | CompanyID String
    | UserID String
    | NoSource


type SourceDocument
    = ProjectDoc ProjectS
    | ContactDoc ContactS


type ActivityCategory
    = PhoneCall
    | Meeting
    | Email
    | Other


type alias ActivityTag =
    { id : String
    , name : String
    }


type alias PartialSearch =
    { search : String
    , search_fields : List String
    , filters : List ( String, String )
    }
