module Utils.Activities.Rest
    exposing
        ( fetchContact
        , fetchProject
        , searchContacts
        , searchActivityTag
        , searchProjects
        , createActivity
        , createActivityTag
          -- Decoders
        , activityDecoder
        )

import Utils.Activities.CommonTypes exposing (..)
import Common.Decoders
import Common.Types
    exposing
        ( ContactS
        , ProjectS
        , UserStructure
        )
import Utils.Api as Api exposing (SearchObj)
import Utils.Routes as Routes
import Utils.Http
import Json.Decode exposing (Decoder, string, nullable, list)
import Json.Decode.Pipeline exposing (decode, optional, required)
import RemoteData exposing (WebData)
import Json.Decode.Extra exposing (date)
import Utils.SearchBox as SearchBox
import DatePicker
import Date exposing (Date)
import Json.Encode
import Http
import Task exposing (Task)


activityDecoder : Decoder Activity
activityDecoder =
    decode Activity
        |> required "_id" string
        |> required "date" date
        |> required "description" string
        |> required "category" string
        |> required "tags" (list activityTagDecoder)
        |> required "contacts" (list contactDecoder)
        |> optional "project" (nullable projectDecoder) Nothing
        |> required "created_by" userDecoder


activityTagDecoder : Decoder ActivityTag
activityTagDecoder =
    decode ActivityTag
        |> required "_id" string
        |> required "name" string


contactDecoder : Decoder ContactS
contactDecoder =
    Common.Decoders.contactS


projectDecoder : Decoder ProjectS
projectDecoder =
    Common.Decoders.projectS


userDecoder : Decoder (UserStructure String ContactS String)
userDecoder =
    Common.Decoders.userStructure string contactDecoder string


searchContacts searchString =
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
        Api.search contactDecoder config (Routes.contactApi "")
            |> Utils.Http.attemptWithRemoteData


searchActivityTag searchString =
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
        Api.search activityTagDecoder config (Routes.activityTagApi "")
            |> Utils.Http.attemptWithRemoteData


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
        Api.search projectDecoder config (Routes.projectApi "")
            |> Utils.Http.attemptWithRemoteData


fetchProject id =
    let
        content =
            { fields = [], embed = [] }
    in
        Api.fetch projectDecoder content (Routes.projectApi id)
            |> Utils.Http.attemptWithRemoteData


fetchContact id =
    let
        content =
            { fields = [], embed = [] }
    in
        Api.fetch contactDecoder content (Routes.contactApi id)
            |> Utils.Http.attemptWithRemoteData


createActivityTag : String -> Cmd (WebData ActivityTag)
createActivityTag txt =
    let
        body =
            Utils.Http.toJsonBody
                [ ( "name", Json.Encode.string txt ) ]

        content =
            { fields = [], embed = [] }

        endpoint =
            Routes.activityTagApi ""
    in
        Api.create activityTagDecoder content endpoint body
            |> Utils.Http.attemptWithRemoteData


activityContent : Api.Content
activityContent =
    { fields = []
    , embed = [ "contacts", "project", "created_by.contact", "tags" ]
    }


createActivity : EditableActivity -> Cmd (WebData Activity)
createActivity a =
    let
        body =
            encodeEditableActivity a

        endpoint =
            Routes.activityApi ""
    in
        Api.create activityDecoder activityContent endpoint body
            |> Utils.Http.attemptWithRemoteData


encodeEditableActivity : EditableActivity -> Http.Body
encodeEditableActivity a =
    let
        category =
            case a.category of
                PhoneCall ->
                    "Phone Call"

                Meeting ->
                    "Meeting"

                Email ->
                    "Email"

                Other ->
                    "Other"

        fields =
            [ ( "date", Json.Encode.string (DatePicker.getDate a.date |> Maybe.map toString |> Maybe.withDefault "") )
            , ( "description", Json.Encode.string a.description )
            , ( "category", Json.Encode.string category )
            , ( "tags", encodeDocumentList a.tags )
            , ( "contacts", encodeDocumentList a.contacts )
            , ( "project", SearchBox.getChosen a.project |> Maybe.map .id |> Maybe.withDefault "" |> Json.Encode.string )
            ]
    in
        Utils.Http.toJsonBody fields


encodeDocumentList l =
    l
        |> List.map .id
        |> List.map Json.Encode.string
        |> Json.Encode.list
