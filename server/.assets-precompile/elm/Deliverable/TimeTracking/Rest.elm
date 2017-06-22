module Deliverable.TimeTracking.Rest exposing (fetchTimeRecordings, createNewTimeRecording, deleteTimeRecording)

import Deliverable.TimeTracking.Types exposing (NewTimeRecording, TimeRecording)
import Json.Decode exposing (Decoder, float, int, string, nullable)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Decode.Extra exposing (date, fromResult)
import Json.Encode
import Http
import Date exposing (Date)
import Utils.Http
import Utils.Api as Api exposing (defaultSearchObj)
import Utils.Routes as Routes
import RemoteData exposing (WebData)
import Common.Types exposing (UserS, CostBreakdown)
import Common.Decoders exposing (userStructure, contactS, timeRecordingStructure)
import DatePicker
import Time
import Date.Extra exposing (Interval(Day))


timeRecordingContent : Api.Content
timeRecordingContent =
    { embed = [ "created_by.contact" ]
    , fields = []
    }


timeRecordingDecoder : Decoder TimeRecording
timeRecordingDecoder =
    timeRecordingStructure string string (userStructure string contactS string)


costBreakdownDecoder : Decoder CostBreakdown
costBreakdownDecoder =
    decode CostBreakdown
        |> optional "positionRate" (nullable float) Nothing
        |> optional "projectPositionRate" (nullable float) Nothing
        |> optional "deliverableRate" (nullable float) Nothing


fetchTimeRecordings : String -> Cmd (WebData (List TimeRecording))
fetchTimeRecordings deliverableID =
    let
        searchObj =
            { defaultSearchObj
                | search = deliverableID
                , search_fields = [ "deliverable" ]
                , embed = timeRecordingContent.embed
                , fields = timeRecordingContent.fields
                , max_results = 200
                , sort = "startDate"
            }
    in
        Api.search timeRecordingDecoder searchObj (Routes.timeRecordingApi "")
            |> Utils.Http.attemptWithRemoteData


encodeNewTimeRecording : NewTimeRecording -> Http.Body
encodeNewTimeRecording t =
    let
        startDate =
            case DatePicker.getDate t.startDate of
                Nothing ->
                    ""

                Just aDate ->
                    let
                        zeroHourStartDate =
                            Date.Extra.floor Day aDate
                                |> Date.toTime

                        zeroHourFromTime =
                            Date.Extra.floor Day t.fromTime
                                |> Date.toTime

                        startTime =
                            ((Date.toTime t.fromTime) - zeroHourFromTime)
                    in
                        toString <| floor <| (zeroHourStartDate + startTime)

        duration =
            (Date.toTime t.toTime - Date.toTime t.fromTime)
                |> floor

        fields =
            [ ( "deliverable", Json.Encode.string t.deliverable )
            , ( "startDate", Json.Encode.string startDate )
            , ( "duration", Json.Encode.int duration )
            ]
    in
        Utils.Http.toJsonBody fields


createNewTimeRecording : NewTimeRecording -> Cmd (WebData TimeRecording)
createNewTimeRecording newTimeRecording =
    let
        content =
            timeRecordingContent

        decoder =
            timeRecordingDecoder

        endpoint =
            Routes.timeRecordingApi ""

        body =
            encodeNewTimeRecording newTimeRecording
    in
        Api.create decoder content endpoint body
            |> Utils.Http.attemptWithRemoteData


deleteTimeRecording : TimeRecording -> Cmd (WebData ())
deleteTimeRecording recording =
    Api.delete (Routes.timeRecordingApi recording.id)
        |> Utils.Http.attemptWithRemoteData
