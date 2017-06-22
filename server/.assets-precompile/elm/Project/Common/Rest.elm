module Project.Common.Rest exposing (..)

import Common.Decoders
import Common.Types
    exposing
        ( Project
        , ProjectStructure
        , ProjectType(..)
        , ProjectContact
        , ContactS
        , CompanyS
        , UserS
        , Position
        , PositionRate
        )
import Json.Decode exposing (Decoder, string, list, at, nullable, int, float, bool)
import Json.Decode.Extra exposing (fromResult)
import Json.Decode.Pipeline exposing (decode, required, requiredAt, optional)
import RemoteData exposing (WebData)
import Json.Encode
import Utils.Http
import Http
import Json.Decode.Extra exposing (date)
import Utils.Routes as Routes
import Utils.Api as Api


-----------------------------------------------
----- DECODERS
-----------------------------------------------


projectDecoder : Decoder Project
projectDecoder =
    Common.Decoders.project


contactDecoder : Decoder ContactS
contactDecoder =
    Common.Decoders.contactS


companyDecoder : Decoder CompanyS
companyDecoder =
    Common.Decoders.companyS


userDecoder : Decoder UserS
userDecoder =
    Common.Decoders.userS


positionDecoder : Decoder Position
positionDecoder =
    Common.Decoders.position


tupleProjectDecoder : Decoder ( Project, Bool )
tupleProjectDecoder =
    Json.Decode.value
        |> Json.Decode.andThen
            (\v ->
                Result.map2
                    (,)
                    (Json.Decode.decodeValue projectDecoder v)
                    (Json.Decode.decodeValue (at [ "editable" ] bool) v)
                    |> fromResult
            )


projectTypeDecoder : Decoder ProjectType
projectTypeDecoder =
    string
        |> Json.Decode.andThen
            (\t ->
                if t == "retainer" then
                    Json.Decode.succeed Retainer
                else if t == "standard" then
                    Json.Decode.succeed Standard
                else
                    Json.Decode.fail <| "Invalid project type: " ++ t
            )


searchContacts : String -> Cmd (WebData (List ContactS))
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
