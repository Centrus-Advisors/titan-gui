module ContactDetails.Rest exposing (fetchContact, searchCompany, submitContactUpdate)

import ContactDetails.Types exposing (LoadedModel)
import Common.Types exposing (ContactStructure, Contact, Phone, CompanyS)
import Common.Decoders
import Json.Decode exposing (Decoder, string, list, at, nullable)
import Json.Decode.Pipeline exposing (decode, required, requiredAt, optional)
import RemoteData exposing (WebData)
import Json.Encode
import Task
import Utils.Api as Api
import Utils.Routes as Routes
import Utils.Http


contactDecoder : Decoder Contact
contactDecoder =
    Common.Decoders.contact


loadedModelDecoder : Decoder LoadedModel
loadedModelDecoder =
    Json.Decode.andThen
        (\contact ->
            Json.Decode.succeed
                { contact = contact
                , editableContact = Nothing
                , submission = RemoteData.NotAsked
                , companySearch =
                    { suggestions = RemoteData.NotAsked
                    , text = ""
                    }
                }
        )
        Common.Decoders.contact


searchCompany : String -> Cmd (WebData (List CompanyS))
searchCompany searchString =
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


contactContent =
    { fields = []
    , embed = [ "created_by", "company", "related.name" ]
    }


fetchContact : String -> Cmd (WebData Contact)
fetchContact contactId =
    Api.fetch contactDecoder contactContent (Routes.contactApi contactId)
        |> Utils.Http.attemptWithRemoteData


submitContactUpdate : Contact -> Cmd (WebData Contact)
submitContactUpdate contact =
    let
        phones =
            List.map
                (\p ->
                    Json.Encode.object
                        [ ( "description", Json.Encode.string p.description )
                        , ( "number", Json.Encode.string p.number )
                        ]
                )
                contact.phones

        company =
            case contact.company of
                Just comp ->
                    Json.Encode.string comp.id

                Nothing ->
                    Json.Encode.null

        fields =
            [ ( "name", Json.Encode.string contact.name )
            , ( "position", Json.Encode.string contact.position )
            , ( "email", Json.Encode.string contact.email )
            , ( "requiredFiles", Json.Encode.list <| List.map Json.Encode.string contact.requiredFiles )
            , ( "location", Json.Encode.string contact.location )
            , ( "address", Json.Encode.string contact.address )
            , ( "notes", Json.Encode.string contact.notes )
            , ( "phones", Json.Encode.list phones )
            , ( "company", company )
            ]
                ++ if String.length contact.gender > 1 then
                    [ ( "gender", Json.Encode.string contact.gender ) ]
                   else
                    []

        body =
            Utils.Http.toJsonBody fields
    in
        Api.save contactDecoder contactContent (Routes.contactApi contact.id) body
            |> Utils.Http.attemptWithRemoteData
