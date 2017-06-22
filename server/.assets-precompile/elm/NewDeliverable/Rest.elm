module NewDeliverable.Rest exposing (createDeliverable, searchUsers, searchProjects)

import NewDeliverable.Types exposing (..)
import Common.Types
    exposing
        ( DeliverableStructure
        , DeliverableStatus(Cancelled, Completed, Active)
        , UserS
        )
import Common.Decoders
import Common.Encoders exposing (encodeDeliverableType)
import Project.Common.Rest exposing (userDecoder)
import Json.Decode exposing (Decoder, string, float, bool, nullable)
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Decode.Extra exposing (date)
import Json.Encode
import Http
import Utils.Http
import Utils.Api as Api
import Utils.Routes as Routes
import RemoteData exposing (WebData)
import DatePicker
import Utils.SearchBox as SearchBox


deliverableContent =
    { fields = []
    , embed = [ "owner", "created_by", "project" ]
    }


deliverableDecoder : Decoder Deliverable
deliverableDecoder =
    Common.Decoders.deliverable


encodeNewDeliverable : NewDeliverable -> Http.Body
encodeNewDeliverable d =
    let
        fields =
            [ ( "title", Json.Encode.string d.title )
            , ( "type", encodeDeliverableType d.type_ )
            , ( "milestone", Json.Encode.string d.milestone )
            , ( "startDate", Json.Encode.string (DatePicker.getDate d.startDate |> Maybe.map toString |> Maybe.withDefault "") )
            , ( "deadline", Json.Encode.string (DatePicker.getDate d.deadline |> Maybe.map toString |> Maybe.withDefault "") )
            , ( "owner", encodeSearchBox d.owner )
            , ( "project", encodeSearchBox d.project )
            ]
    in
        Utils.Http.toJsonBody fields


encodeSearchBox a =
    SearchBox.getChosen a
        |> Maybe.map .id
        |> Maybe.map Json.Encode.string
        |> Maybe.withDefault Json.Encode.null


createDeliverable : NewDeliverable -> Cmd (WebData Deliverable)
createDeliverable newDeliverable =
    let
        decoder =
            deliverableDecoder

        content =
            deliverableContent

        endpoint =
            Routes.deliverableApi ""

        body =
            encodeNewDeliverable newDeliverable
    in
        Api.create decoder content endpoint body
            |> Utils.Http.attemptWithRemoteData


searchUsers : String -> Cmd (WebData (List UserS))
searchUsers searchString =
    let
        config =
            { search = searchString
            , search_fields = [ "email" ]
            , fields = []
            , embed = []
            , sort = "email"
            , max_results = 5
            , page = 0
            , filters = []
            }
    in
        Api.search userDecoder config (Routes.userApi "")
            |> Utils.Http.attemptWithRemoteData


searchProjects : String -> Cmd (WebData (List Project))
searchProjects searchString =
    let
        config =
            { search = searchString
            , search_fields = [ "name" ]
            , fields = []
            , embed = []
            , sort = "name"
            , max_results = 5
            , page = 0
            , filters = []
            }
    in
        Api.search Common.Decoders.projectS config (Routes.projectApi "")
            |> Utils.Http.attemptWithRemoteData
