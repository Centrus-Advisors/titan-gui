module DownloadForm.Types exposing (..)

import RemoteData exposing (WebData)
import Date exposing (Date)
import DatePicker exposing (DatePicker)


type alias Model =
    { date : DatePicker }


type Msg
    = ChangeDate DatePicker.Msg
