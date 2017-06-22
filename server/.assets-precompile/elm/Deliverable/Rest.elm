module Deliverable.Rest
    exposing
        ( deliverableDecoder
        , deliverableContent
        , searchUsers
        , fetchDeliverable
        , saveDeliverable
        , fetchDeliverableCost
        , deleteDeliverable
        , delivStatusToString
        , stringToDelivStatus
        )

import Deliverable.Types exposing (..)
import Common.Types
    exposing
        ( Deliverable
        , DeliverableStructure
        , UserS
        , CompanyS
        , ProjectS
        , DeliverableStatus(Completed, Cancelled, Active)
        , Cost
        )
import Common.Decoders
import Common.Encoders exposing (encodeDeliverableType)
import Json.Decode exposing (Decoder, string, bool, at, nullable, int, float)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Decode.Extra exposing (date, fromResult)
import Json.Encode
import Http
import Utils.Http
import Utils.Api as Api exposing (defaultSearchObj)
import Utils.Routes as Routes
import RemoteData exposing (WebData, RemoteData)
import Utils.SearchBox as SearchBox
import DatePicker


deliverableContent =
    { fields = []
    , embed = [ "owner", "project", "created_by", "assignees" ]
    }


deliverableDecoder : Decoder Deliverable
deliverableDecoder =
    Common.Decoders.deliverable


deliverableTypeDecoder : Decoder DeliverableEdit
deliverableTypeDecoder =
    let
        assignType deliv canEdit =
            if canEdit then
                Editable deliv
            else
                NonEditable deliv
    in
        Json.Decode.value
            |> Json.Decode.andThen
                (\v ->
                    Result.map2
                        assignType
                        (Json.Decode.decodeValue deliverableDecoder v)
                        (Json.Decode.decodeValue (at [ "editable" ] bool) v)
                        |> fromResult
                )


encodeEditableDeliverable : EditableDeliverable -> Http.Body
encodeEditableDeliverable d =
    let
        fields =
            [ ( "title", Json.Encode.string d.title )
            , ( "type", encodeDeliverableType d.type_ )
            , ( "milestone", Json.Encode.string d.milestone )
            , ( "description", Json.Encode.string d.description )
            , ( "status", encodeDeliverableStatus d.status )
            , ( "hourly_rate", Json.Encode.string d.hourly_rate )
            , ( "startDate", Json.Encode.string (DatePicker.getDate d.startDate |> Maybe.map toString |> Maybe.withDefault "") )
            , ( "deadline", Json.Encode.string (DatePicker.getDate d.deadline |> Maybe.map toString |> Maybe.withDefault "") )
            , ( "owner", encodeSearchBox d.owner )
            , ( "assignees", Json.Encode.list (List.map encodeSearchBox d.assignees) )
            , ( "project", Json.Encode.string d.project.id )
            , ( "created_by", Json.Encode.string d.created_by.id )
            ]
    in
        Utils.Http.toJsonBody fields


encodeDeliverableStatus : DeliverableStatus -> Json.Decode.Value
encodeDeliverableStatus status =
    Json.Encode.string <| delivStatusToString status


delivStatusToString : DeliverableStatus -> String
delivStatusToString status =
    case status of
        Completed ->
            "completed"

        Active ->
            "active"

        Cancelled ->
            "cancelled"


stringToDelivStatus : String -> Maybe DeliverableStatus
stringToDelivStatus v =
    case v of
        "completed" ->
            Just Completed

        "cancelled" ->
            Just Cancelled

        "active" ->
            Just Active

        _ ->
            Nothing


encodeSearchBox a =
    SearchBox.getChosen a
        |> Maybe.map .id
        |> Maybe.map Json.Encode.string
        |> Maybe.withDefault Json.Encode.null


searchUsers : String -> Cmd (RemoteData Http.Error (List UserS))
searchUsers searchString =
    let
        searchObj =
            { defaultSearchObj
                | search = searchString
                , search_fields = [ "email" ]
                , max_results = 5
                , sort = "email"
            }
    in
        Api.search Common.Decoders.userS searchObj (Routes.userApi "")
            |> Utils.Http.attemptWithRemoteData


fetchDeliverable : String -> Cmd (WebData DeliverableEdit)
fetchDeliverable id =
    Api.fetch deliverableTypeDecoder deliverableContent (Routes.deliverableApi id)
        |> Utils.Http.attemptWithRemoteData


saveDeliverable : EditableDeliverable -> Cmd (WebData DeliverableEdit)
saveDeliverable deliv =
    let
        endpoint =
            Routes.deliverableApi deliv.id

        body =
            encodeEditableDeliverable deliv
    in
        Api.save deliverableTypeDecoder deliverableContent endpoint body
            |> Utils.Http.attemptWithRemoteData


deleteDeliverable : EditableDeliverable -> Cmd (WebData ())
deleteDeliverable { id } =
    Api.delete (Routes.deliverableApi id)
        |> Utils.Http.attemptWithRemoteData


fetchDeliverableCost : DeliverableID -> Cmd (WebData Cost)
fetchDeliverableCost deliverableID =
    let
        decoder =
            Common.Decoders.cost

        content =
            { fields = []
            , embed = []
            }

        endpoint =
            Routes.deliverableCostApi deliverableID
    in
        Api.fetch decoder content endpoint
            |> Utils.Http.attemptWithRemoteData
