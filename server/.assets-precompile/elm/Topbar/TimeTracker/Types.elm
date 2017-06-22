module Topbar.TimeTracker.Types exposing (..)

import Common.Types exposing (Deliverable, TimeRecordingStructure, UserStructure, ContactS, ProjectS, DeliverableS)
import RemoteData exposing (WebData)
import Utils.SearchBox as SearchBox
import Date exposing (Date)
import Time exposing (Time)
import RemoteData exposing (RemoteData, WebData)
import Utils.SearchBox as SearchBox
import NewDeliverable.Main as NewDeliverable


type alias Model =
    { status : RemoteData String TrackingStatus
    , currentTime : Time
    , creationModal : Maybe CreationModal
    }


type alias CreationModal =
    { deliverable : SearchBox.SearchBox Deliverable
    , fromDate : Date
    , toDate : Date
    , newDeliverable : Maybe (NewDeliverable.Model Msg)
    , submission : WebData TimeRecording
    }


type Msg
    = DoNothing
    | SendStatus TrackingStatus
    | ReceivedStatus (RemoteData String TrackingStatus)
    | CurrentTime Time
    | CancelCreation
    | CreateRecording TrackingStatus
    | SubMsgModal ModalMsg


type ModalMsg
    = SearchBoxMsg (SearchBox.Msg Deliverable)
    | Submit Deliverable
    | SubmissionUpdate (WebData TimeRecording)
    | CreateNewDeliverable
    | NewDeliverableMsg NewDeliverable.Msg
    | NewDeliverableCreated Deliverable
    | NewDeliverableCancel


type TrackingStatus
    = Recording Date
    | NotRecording


type alias TimeRecording =
    TimeRecordingStructure ProjectS DeliverableS User


type alias User =
    UserStructure String ContactS String
