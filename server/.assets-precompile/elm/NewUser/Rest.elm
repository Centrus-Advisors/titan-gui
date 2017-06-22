module NewUser.Rest exposing (searchContacts, searchPositions, createContact, createUser)

import NewUser.Types exposing (..)
import Common.Types exposing (ContactS, Position, Permission(ADMINISTRATOR, STANDARD, PRIVILEGED))
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


contactDecoder : Decoder ContactS
contactDecoder =
    Common.Decoders.contactS


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


positionDecoder : Decoder Position
positionDecoder =
    Common.Decoders.position


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


createUser : EditableUser -> Cmd (WebData ())
createUser u =
    let
        body =
            encodeUser u

        endpoint =
            Routes.userApi ""
    in
        Api.create (Json.Decode.succeed ()) noEmbeddedContent endpoint body
            |> Utils.Http.attemptWithRemoteData


noEmbeddedContent : Api.Content
noEmbeddedContent =
    { fields = []
    , embed = []
    }


encodeUser : EditableUser -> Http.Body
encodeUser u =
    let
        permission =
            case u.permission of
                ADMINISTRATOR ->
                    "ADMINISTRATOR"

                STANDARD ->
                    "STANDARD"

                PRIVILEGED ->
                    "PRIVILEGED"

        position =
            SearchBox.getChosen u.position
                |> Maybe.map .id
                |> Maybe.withDefault ""

        fields =
            [ ( "contact", Json.Encode.string u.contact.id )
            , ( "permission", Json.Encode.string permission )
            , ( "position", Json.Encode.string position )
            , ( "email", Json.Encode.string u.email1 )
            , ( "password", Json.Encode.string u.pass1 )
            , ( "active", Json.Encode.bool u.active )
            ]
    in
        Utils.Http.toJsonBody fields


createContact : EditableContact -> Cmd (WebData ContactS)
createContact u =
    let
        body =
            encodeContact u

        endpoint =
            Routes.contactApi ""
    in
        Api.create contactDecoder noEmbeddedContent endpoint body
            |> Utils.Http.attemptWithRemoteData


encodeContact : EditableContact -> Http.Body
encodeContact c =
    let
        fields =
            [ ( "name", Json.Encode.string c.name )
            , ( "email", Json.Encode.string c.email )
            ]
    in
        Utils.Http.toJsonBody fields
