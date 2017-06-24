module DownloadForm.Types exposing (..)

import RemoteData exposing (WebData)
import Date exposing (Date)
import DatePicker exposing (DatePicker)


type alias Model =
    { fromDate : DatePicker
    , toDate : DatePicker
    }


type Msg
    = FromDate DatePicker.Msg
    | ToDate DatePicker.Msg
