module NewProject.Rest exposing (searchUsers, searchCompanies, createProject)

import NewProject.Types exposing (..)
import Common.Types exposing (UserS, CompanyS, ProjectType(Standard, Retainer), ProjectS)
import Common.Decoders
import Json.Encode
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Decode exposing (Decoder, string, nullable, list)
import Utils.Api as Api exposing (SearchObj)
import Utils.Routes as Routes
import Utils.Http
import Http
import RemoteData exposing (WebData)
import Utils.SearchBox as SearchBox
import DatePicker


encodeNewProject : NewProject -> Http.Body
encodeNewProject project =
    let
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
            , ( "company", encodeSearchBox project.company )
            , ( "owner", encodeSearchBox project.owner )
            ]
    in
        Utils.Http.toJsonBody fields


encodeSearchBox a =
    SearchBox.getChosen a
        |> Maybe.map .id
        |> Maybe.map Json.Encode.string
        |> Maybe.withDefault Json.Encode.null


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
        Api.search Common.Decoders.userS config (Routes.userApi "")
            |> Utils.Http.attemptWithRemoteData


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
        Api.search Common.Decoders.companyS config (Routes.companyApi "")
            |> Utils.Http.attemptWithRemoteData


createProject : NewProject -> Cmd (WebData ProjectS)
createProject newProject =
    let
        content =
            { fields = []
            , embed = []
            }

        body =
            encodeNewProject newProject

        endpoint =
            Routes.projectApi ""

        decoder =
            Common.Decoders.projectS
    in
        Api.create decoder content endpoint body
            |> Utils.Http.attemptWithRemoteData
