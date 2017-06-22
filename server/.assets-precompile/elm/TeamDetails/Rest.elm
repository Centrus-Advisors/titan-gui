module TeamDetails.Rest
    exposing
        ( fetchTeam
        , fetchTeamMembers
        , saveTeam
        , searchTeamUser
        , searchAnyUser
        , makeNewMember
        , removeMember
        , deleteTeam
        )

import TeamDetails.Types exposing (..)
import Common.Decoders
import Common.Types exposing (UserStructure, TeamStructure, ContactS)
import RemoteData exposing (WebData)
import Utils.Api as Api
import Utils.Http
import Utils.Routes as Routes
import Json.Decode exposing (Decoder, string, nullable, bool, at)
import Json.Decode.Extra exposing (date, fromResult)
import Json.Decode.Pipeline exposing (decode, required, optional, hardcoded)
import Json.Encode
import Http
import Utils.SearchBox as SearchBox
import Date exposing (Date)


teamDecoder : Decoder Team
teamDecoder =
    Common.Decoders.teamStructure userDecoder


userDecoder : Decoder User
userDecoder =
    Common.Decoders.userStructure string Common.Decoders.contactS string


tupleTeamDecoder : Decoder ( Team, Bool )
tupleTeamDecoder =
    Json.Decode.value
        |> Json.Decode.andThen
            (\v ->
                Result.map2
                    (,)
                    (Json.Decode.decodeValue teamDecoder v)
                    (Json.Decode.decodeValue (at [ "editable" ] bool) v)
                    |> fromResult
            )


encodeTeam : EditableTeam -> Http.Body
encodeTeam editable =
    let
        fields =
            case SearchBox.getChosen editable.leader of
                Just user ->
                    [ ( "name", Json.Encode.string editable.name )
                    , ( "leader", Json.Encode.string user.id )
                    ]

                Nothing ->
                    [ ( "name", Json.Encode.string editable.name )
                    ]
    in
        Utils.Http.toJsonBody fields


encodeUserWithTeam : Maybe String -> Http.Body
encodeUserWithTeam mTeamId =
    let
        fields =
            case mTeamId of
                Just teamId ->
                    [ ( "team", Json.Encode.string teamId ) ]

                Nothing ->
                    [ ( "team", Json.Encode.null ) ]
    in
        Utils.Http.toJsonBody fields


teamContent : Api.Content
teamContent =
    { fields = []
    , embed = [ "leader.contact" ]
    }


userContent : Api.Content
userContent =
    { fields = []
    , embed = [ "contact" ]
    }


fetchTeam : String -> Cmd (WebData ( Team, Bool ))
fetchTeam teamId =
    Api.fetch tupleTeamDecoder teamContent (Routes.teamApi teamId)
        |> Utils.Http.attemptWithRemoteData


fetchTeamMembers : String -> Cmd (WebData (List User))
fetchTeamMembers teamId =
    let
        config =
            { search = ""
            , search_fields = []
            , fields = []
            , embed = [ "contact" ]
            , sort = "email"
            , max_results = 100
            , page = 0
            , filters = [ ( "team", teamId ) ]
            }
    in
        Api.search userDecoder config (Routes.userApi "")
            |> Utils.Http.attemptWithRemoteData


saveTeam : EditableTeam -> Cmd (WebData Team)
saveTeam editable =
    let
        body =
            encodeTeam editable

        route =
            (Routes.teamApi editable.id)
    in
        Api.save teamDecoder teamContent route body
            |> Utils.Http.attemptWithRemoteData


deleteTeam : EditableTeam -> Cmd (WebData ())
deleteTeam editable =
    let
        route =
            (Routes.teamApi editable.id)
    in
        Api.delete route
            |> Utils.Http.attemptWithRemoteData


searchTeamUser : String -> String -> Cmd (WebData (List User))
searchTeamUser teamId searchString =
    let
        config =
            { search = searchString
            , search_fields = [ "email" ]
            , fields = []
            , embed = [ "contact" ]
            , sort = "email"
            , max_results = 5
            , page = 0
            , filters = [ ( "team", teamId ) ]
            }
    in
        Api.search userDecoder config (Routes.userApi "")
            |> Utils.Http.attemptWithRemoteData


searchAnyUser : String -> Cmd (WebData (List User))
searchAnyUser searchString =
    let
        config =
            { search = searchString
            , search_fields = [ "email" ]
            , fields = []
            , embed = [ "contact" ]
            , sort = "email"
            , max_results = 5
            , page = 0
            , filters = []
            }
    in
        Api.search userDecoder config (Routes.userApi "")
            |> Utils.Http.attemptWithRemoteData


makeNewMember : String -> String -> Cmd (WebData ())
makeNewMember teamId userId =
    let
        body =
            encodeUserWithTeam (Just teamId)

        decoder =
            Json.Decode.succeed ()
    in
        Api.save decoder userContent (Routes.userApi userId) body
            |> Utils.Http.attemptWithRemoteData


removeMember : User -> Cmd (WebData ())
removeMember user =
    let
        body =
            encodeUserWithTeam Nothing

        decoder =
            Json.Decode.succeed ()

        route =
            (Routes.userApi user.id)
    in
        Api.save decoder userContent route body
            |> Utils.Http.attemptWithRemoteData
