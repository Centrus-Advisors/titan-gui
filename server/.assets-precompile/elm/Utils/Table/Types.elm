module Utils.Table.Types exposing (..)

import RemoteData exposing (WebData)
import Html exposing (Html)
import Utils.Api as Api
import Date exposing (Date)


type Table doc
    = Table (Model doc)


type alias Model doc =
    { page : Int
    , maxItems : Int
    , filter :
        Maybe FilterConfig
        -- This is a Result because this comes from JavaScript so the parsing may fail
    , hiddenFilters : Result String (List ( String, String ))
    , sortOrder : SortOrder
    , sortColumn : Maybe (ColumnConfig doc)
    , searchText : String
    , searchColumn : Maybe (ColumnConfig doc)
    , lastRequestID : RequestID
    , config : Config doc
    , data :
        WebData (Api.SearchData doc)
        -- We keep the last data to show something while new data is being loaded
    , lastData : WebData (Api.SearchData doc)
    }


type SortOrder
    = Asc
    | Desc


type alias RequestID =
    Int


type Msg doc
    = SetMaxItems Int
    | SetPage Int
    | DataFetch RequestID (WebData (Api.SearchData doc))
    | SortBy (ColumnConfig doc)
    | SetFilter FilterConfig
    | SetHiddenFilters (Result String (List ( String, String )))



-- CONFIGURATION


type alias Config doc =
    { columns : List (ColumnConfig doc)
    , filters : List FilterConfig
    , defaultSortColumn : String
    , search : Api.SearchObj -> Cmd (WebData (Api.SearchData doc))
    }


type alias ColumnConfig doc =
    { title : String
    , fieldName : String
    , renderCell : doc -> Html (Msg doc)
    }


type FilterConfig
    = TextFilter (Filter String)
    | DateFilter (Filter Date)


type alias Filter a =
    { name : String
    , applyFilter : a -> List ( String, String )
    , value : a
    }
