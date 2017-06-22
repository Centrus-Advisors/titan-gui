module Utils.Activities.Filters.Types exposing (..)

import DatePicker
import Utils.SearchBox as SearchBox
import Date exposing (Date)
import Common.Types exposing (ContactS)
import Utils.Activities.CommonTypes exposing (ActivityTag, SourceID, PartialSearch)


type alias Model =
    { sourceID : SourceID
    , todayDate : Date
    , textSearch : String
    , filter : Filter
    }


type Filters
    = Filters Model


type Msg
    = TextUpdate String
    | DateUpdate DatePicker.Msg
    | TagUpdate (SearchBox.Msg ActivityTag)
    | ContactUpdate (SearchBox.Msg ContactS)
    | SelectFilter String


type Filter
    = NoFilter
    | DateFilter DatePicker.DatePicker
    | TagFilter (SearchBox.SearchBox ActivityTag)
    | ContactFilter (SearchBox.SearchBox ContactS)
