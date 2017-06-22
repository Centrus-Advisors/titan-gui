module Deliverable.TimeTracking.Types exposing (..)

import Common.Types exposing (ContactS, UserStructure, TimeRecordingStructure, CostBreakdown)
import RemoteData exposing (WebData)
import Date exposing (Date)
import DatePicker
import RemoteData exposing (WebData)
import Table
import TimePicker


type alias ID =
    String


type Msg
    = FetchUpdate (WebData (List TimeRecording))
    | SetTableState Table.State
    | ToggleNewRecording Bool
    | Submit
    | SubmissionUpdate (WebData TimeRecording)
    | NewRecording NewRecordingMsg
    | Reload
    | WarnDeletion (Maybe TimeRecording)
    | Delete TimeRecording
    | DeletionUpdate (WebData ())


type NewRecordingMsg
    = StartDate DatePicker.Msg
    | FromTimePicker TimePicker.Msg
    | ToggleFromTimePicker Bool
    | ToTimePicker TimePicker.Msg
    | ToggleToTimePicker Bool


type alias Model =
    { todayDate : Date
    , deliverable : ID
    , recordings : List TimeRecording
    , fetchRecordings : WebData (List TimeRecording)
    , tableState : Table.State
    , newRecording : Maybe NewTimeRecording
    , submission : WebData TimeRecording
    , deletionStatus : WebData ()
    , deletionWarning : Maybe TimeRecording
    }


type alias TimeRecording =
    TimeRecordingStructure String String User


type alias User =
    UserStructure String ContactS String


type alias NewTimeRecording =
    { deliverable : ID
    , startDate : DatePicker.DatePicker
    , fromTime : Date
    , fromTimePicker : Maybe TimePicker.Model
    , toTime : Date
    , toTimePicker : Maybe TimePicker.Model
    }
