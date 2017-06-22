module Project.Details.Rest
    exposing
        ( encodeEditableProject
        , searchCompanies
        , searchPositions
        , fetchCost
        , fetchProject
        , saveProject
        , deleteProject
        )

import Project.Details.Types exposing (..)
import Common.Types
    exposing
        ( Cost
        , Project
        , CompanyS
        , Position
        , ProjectType(Retainer, Standard)
        )
import Common.Decoders
import Project.Common.Rest exposing (contactDecoder, companyDecoder, positionDecoder)
import Http
import RemoteData exposing (WebData)
import Json.Encode
import Json.Decode exposing (Decoder, string, float, int, at, bool)
import Json.Decode.Pipeline exposing (required, optional, decode)
import Json.Decode.Extra exposing (fromResult)
import Json.Encode.Extra
import Utils.Http
import Utils.Api as Api
import Utils.Routes as Routes
import DatePicker
import Utils.SearchBox as SearchBox


-----------------------------------------------
----- ENCODERS
-----------------------------------------------


encodeEditableProject : EditableProject -> Http.Body
encodeEditableProject project =
    let
        contacts =
            List.map
                (\c ->
                    Json.Encode.object
                        [ ( "description", Json.Encode.string c.description )
                        , ( "contact", encodeSearchBox c.contact )
                        ]
                )
                project.contacts

        rates =
            List.map
                (\r ->
                    Json.Encode.object
                        [ ( "position", encodeSearchBox r.position )
                        , ( "hourly_rate", Json.Encode.string r.hourly_rate )
                        ]
                )
                project.rates

        type_ =
            case project.type_ of
                Retainer ->
                    "retainer"

                Standard ->
                    "standard"

        fields =
            [ ( "name", Json.Encode.string project.name )
            , ( "description", Json.Encode.string project.description )
            , ( "type", Json.Encode.string type_ )
            , ( "startDate", Json.Encode.string (DatePicker.getDate project.startDate |> Maybe.map toString |> Maybe.withDefault "") )
            , ( "endDate", Json.Encode.string (DatePicker.getDate project.endDate |> Maybe.map toString |> Maybe.withDefault "") )
            , ( "budget", Json.Encode.string project.budget )
            , ( "requiredFiles", Json.Encode.list <| List.map Json.Encode.string project.requiredFiles )
            , ( "notes", Json.Encode.string project.notes )
            , ( "company", encodeSearchBox project.company )
            , ( "owner", encodeSearchBox project.owner )
            , ( "contacts", Json.Encode.list contacts )
            , ( "rates", Json.Encode.list rates )
            , ( "created_by", Json.Encode.string project.created_by.id )
            ]
    in
        Utils.Http.toJsonBody fields


encodeSearchBox a =
    SearchBox.getChosen a
        |> Maybe.map .id
        |> Maybe.map Json.Encode.string
        |> Maybe.withDefault Json.Encode.null


searchCompanies : String -> Cmd (WebData (List CompanyS))
searchCompanies searchString =
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
        Api.search companyDecoder config (Routes.companyApi "")
            |> Utils.Http.attemptWithRemoteData


searchPositions : String -> Cmd (WebData (List Position))
searchPositions searchString =
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
        Api.search positionDecoder config (Routes.positionApi "")
            |> Utils.Http.attemptWithRemoteData


fetchCost : ProjectID -> Cmd (WebData Cost)
fetchCost projectID =
    let
        decoder =
            Common.Decoders.cost

        content =
            { fields = []
            , embed = []
            }

        endpoint =
            Routes.projectCostApi projectID
    in
        Api.fetch decoder content endpoint
            |> Utils.Http.attemptWithRemoteData


projectWithPermissionDecoder : Decoder ProjectWithPermission
projectWithPermissionDecoder =
    let
        assignType proj canEdit =
            if canEdit then
                Editable proj
            else
                NonEditable proj
    in
        Json.Decode.value
            |> Json.Decode.andThen
                (\v ->
                    Result.map2
                        assignType
                        (Json.Decode.decodeValue Common.Decoders.project v)
                        (Json.Decode.decodeValue (at [ "editable" ] bool) v)
                        |> fromResult
                )


projectContent =
    { fields = []
    , embed = [ "created_by", "company", "contacts.contact", "owner", "rates.position" ]
    }


fetchProject : ProjectID -> Cmd (WebData ProjectWithPermission)
fetchProject projectID =
    let
        decoder =
            projectWithPermissionDecoder

        endpoint =
            Routes.projectApi projectID

        content =
            projectContent
    in
        Api.fetch decoder content endpoint
            |> Utils.Http.attemptWithRemoteData


saveProject : EditableProject -> Cmd (WebData ProjectWithPermission)
saveProject project =
    let
        decoder =
            projectWithPermissionDecoder

        endpoint =
            Routes.projectApi project.id

        content =
            projectContent

        body =
            encodeEditableProject project
    in
        Api.save decoder content endpoint body
            |> Utils.Http.attemptWithRemoteData


deleteProject : EditableProject -> Cmd (WebData ())
deleteProject { id } =
    Api.delete (Routes.projectApi id)
        |> Utils.Http.attemptWithRemoteData
