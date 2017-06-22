module CompanyDetails.Rest exposing (companyDecoder, submitCompanyUpdate, searchContact, fetchCompany)

import CompanyDetails.Types exposing (EditableCompany, Company, Phone, Contact, CompanyContact)
import Common.Decoders
import Json.Decode exposing (Decoder, string, list, bool, nullable)
import Json.Decode.Pipeline exposing (decode, required, requiredAt, optional, optionalAt, hardcoded)
import Utils.Http
import Utils.Routes as Routes
import Utils.Api as Api
import RemoteData exposing (WebData)
import Task
import Json.Encode
import Json.Decode


companyDecoder : Decoder Company
companyDecoder =
    Common.Decoders.company


contactDecoder : Decoder Contact
contactDecoder =
    Common.Decoders.contactS


relatedContactDecoder : Decoder CompanyContact
relatedContactDecoder =
    decode CompanyContact
        |> required "relation" string
        |> required "contact" (nullable contactDecoder)


companyContent =
    { fields = []
    , embed = [ "relatedContacts.contact", "created_by" ]
    }


submitCompanyUpdate : EditableCompany -> Cmd (WebData Company)
submitCompanyUpdate company =
    let
        phones =
            List.map
                (\p ->
                    Json.Encode.object
                        [ ( "description", Json.Encode.string p.description )
                        , ( "number", Json.Encode.string p.number )
                        ]
                )
                company.phones

        relatedContacts =
            List.map
                (\r ->
                    Json.Encode.object
                        [ ( "relation", Json.Encode.string r.relation )
                        , ( "contact"
                          , case r.contact of
                                Just contact ->
                                    Json.Encode.string contact.id

                                Nothing ->
                                    Json.Encode.null
                          )
                        ]
                )
                company.relatedContacts

        fields =
            [ ( "name", Json.Encode.string company.name )
            , ( "location", Json.Encode.string company.location )
            , ( "phones", Json.Encode.list phones )
            , ( "relatedContacts", Json.Encode.list relatedContacts )
            , ( "address", Json.Encode.string company.address )
            , ( "postcode", Json.Encode.string company.postcode )
            , ( "email", Json.Encode.string company.email )
            , ( "sector", Json.Encode.string company.sector )
            , ( "classification", Json.Encode.string company.classification )
            , ( "investorClassification", Json.Encode.string company.investorClassification )
            , ( "investorType", Json.Encode.string company.investorType )
            , ( "website", Json.Encode.string company.website )
            , ( "requiredFiles", Json.Encode.list <| List.map Json.Encode.string company.requiredFiles )
            , ( "archived", Json.Encode.bool company.archived )
            , ( "notes", Json.Encode.string company.notes )
            ]

        body =
            Utils.Http.toJsonBody fields

        route =
            (Routes.companyApi company.id)
    in
        Api.save companyDecoder companyContent route body
            |> Utils.Http.attemptWithRemoteData


searchContact : String -> Cmd (WebData (List Contact))
searchContact searchString =
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


fetchCompany : String -> Cmd (WebData Company)
fetchCompany companyID =
    let
        decoder =
            companyDecoder

        endpoint =
            Routes.companyApi companyID

        content =
            companyContent
    in
        Api.fetch decoder content endpoint
            |> Utils.Http.attemptWithRemoteData
