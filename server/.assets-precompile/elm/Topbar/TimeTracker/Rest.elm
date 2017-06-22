module Topbar.TimeTracker.Rest
    exposing
        ( decodeTrackingStatus
        , encodeTrackingStatus
        , searchDeliverables
        , createTimeRecording
        )

import Topbar.TimeTracker.Types exposing (..)
import Common.Types
    exposing
        ( Deliverable
        )
import Common.Decoders
import Json.Decode exposing (Decoder, int, string, nullable)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Decode.Extra exposing (date)
import Json.Encode
import Date exposing (Date)
import RemoteData exposing (WebData)
import Utils.Http
import Utils.Api as Api
import Utils.Routes as Routes
import Http


type alias RawTrackingStatus =
    { status : String
    , startTime : Maybe Date
    }


rawTrackingStatusDecoder : Decoder RawTrackingStatus
rawTrackingStatusDecoder =
    decode RawTrackingStatus
        |> required "status" string
        |> optional "startTime" (nullable date) Nothing


trackingStatusDecoder : Decoder TrackingStatus
trackingStatusDecoder =
    rawTrackingStatusDecoder
        |> Json.Decode.andThen
            (\v ->
                case ( v.status, v.startTime ) of
                    ( "recording", Just aTime ) ->
                        Json.Decode.succeed <| Recording aTime

                    ( "notRecording", _ ) ->
                        Json.Decode.succeed <| NotRecording

                    _ ->
                        Json.Decode.fail <| "Invalid tracking status: '" ++ (toString v) ++ "'"
            )


decodeTrackingStatus : Json.Decode.Value -> Result String TrackingStatus
decodeTrackingStatus s =
    Json.Decode.decodeValue trackingStatusDecoder s


encodeTrackingStatus : TrackingStatus -> Json.Encode.Value
encodeTrackingStatus t =
    case t of
        NotRecording ->
            Json.Encode.object
                [ ( "status", Json.Encode.string "notRecording" )
                , ( "startTime", Json.Encode.null )
                ]

        Recording date ->
            Json.Encode.object
                [ ( "status", Json.Encode.string "recording" )
                , ( "startTime", Json.Encode.string <| toString date )
                ]


deliverableContent =
    { fields = []
    }


searchDeliverables : String -> Cmd (WebData (List Deliverable))
searchDeliverables searchString =
    let
        searchObj =
            { search = searchString
            , search_fields = [ "title" ]
            , embed = [ "owner", "project", "created_by" ]
            , max_results = 15
            , sort = "title"
            , fields = []
            , page = 0
            , filters = []
            }
    in
        Api.search Common.Decoders.deliverable searchObj (Routes.deliverableApi "")
            |> Utils.Http.attemptWithRemoteData


encodeNewTimeRecording : Deliverable -> Date -> Date -> Http.Body
encodeNewTimeRecording deliverable startDate endDate =
    let
        duration =
            Date.toTime endDate - Date.toTime startDate

        fields =
            [ ( "deliverable", Json.Encode.string deliverable.id )
            , ( "startDate", Json.Encode.string <| toString startDate )
            , ( "duration", Json.Encode.int <| floor duration )
            ]
    in
        Utils.Http.toJsonBody fields


timeRecordingDecoder : Decoder TimeRecording
timeRecordingDecoder =
    Common.Decoders.timeRecordingStructure
        Common.Decoders.projectS
        Common.Decoders.deliverableS
        (Common.Decoders.userStructure string Common.Decoders.contactS string)


createTimeRecording : Deliverable -> Date -> Date -> Cmd (WebData TimeRecording)
createTimeRecording deliverable startDate endDate =
    let
        content =
            { fields = []
            , embed =
                [ "project"
                , "deliverable"
                , "created_by.contact"
                ]
            }

        decoder =
            timeRecordingDecoder

        endpoint =
            Routes.timeRecordingApi ""

        body =
            encodeNewTimeRecording deliverable startDate endDate
    in
        Api.create decoder content endpoint body
            |> Utils.Http.attemptWithRemoteData
