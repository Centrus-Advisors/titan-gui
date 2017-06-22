module CompanyDetails.State exposing (..)

import CompanyDetails.Types exposing (..)
import CompanyDetails.Rest exposing (submitCompanyUpdate, searchContact, companyDecoder, fetchCompany)
import List.Extra exposing (..)
import Utils.Delay
import Utils.Activities.Main as Activities
import Time exposing (Time)
import RemoteData exposing (WebData)
import Json.Decode
import Utils.FileStorage.Main as FileStorage


emptySearch =
    { text = ""
    , suggestions = RemoteData.NotAsked
    }


init : { time : Time, companyId : String } -> ( Model, Cmd Msg )
init { time, companyId } =
    let
        ( activities, activitiesMsg ) =
            Activities.initFromCompany time companyId

        ( fileStorage, fileStorageMsg ) =
            FileStorage.initFromCompany companyId
    in
        { company = RemoteData.Loading
        , editableCompany = Nothing
        , submission = RemoteData.NotAsked
        , activities = activities
        , fileStorage = fileStorage
        }
            ! [ Cmd.map ActivitiesMsg activitiesMsg
              , Cmd.map FileStorageMsg fileStorageMsg
              , fetchCompany companyId
                    |> Cmd.map FetchCompanyUpdate
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        FetchCompanyUpdate status ->
            { model | company = status } ! []

        UpdateField fieldUpdate ->
            model.editableCompany
                |> Maybe.map (updateCompanyField fieldUpdate)
                |> Maybe.map (Tuple.mapFirst (\v -> { model | editableCompany = Just v }))
                |> Maybe.withDefault ( model, Cmd.none )

        EnableEditing company ->
            ( { model
                | editableCompany = Just <| toEditableCompany company
                , submission = RemoteData.NotAsked
              }
            , Cmd.none
            )

        SaveEditing editableCompany ->
            ( { model
                | submission = RemoteData.Loading
              }
            , submitCompanyUpdate editableCompany
                |> Cmd.map SubmissionUpdate
            )

        CancelEditing ->
            ( { model
                | editableCompany = Nothing
                , submission = RemoteData.NotAsked
              }
            , Cmd.none
            )

        SubmissionUpdate update ->
            case update of
                RemoteData.Success _ ->
                    ( { model
                        | submission = update
                        , company = update
                        , editableCompany = Nothing
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | submission = update }
                    , Cmd.none
                    )

        ActivitiesMsg dMsg ->
            Activities.update dMsg model.activities
                |> Tuple.mapFirst (\a -> { model | activities = a })
                |> Tuple.mapSecond (Cmd.map ActivitiesMsg)

        FileStorageMsg subMsg ->
            FileStorage.update subMsg model.fileStorage
                |> Tuple.mapFirst (\fs -> { model | fileStorage = fs })
                |> Tuple.mapSecond (Cmd.map FileStorageMsg)


updateCompanyField : CompanyFieldUpdate -> EditableCompany -> ( EditableCompany, Cmd Msg )
updateCompanyField fieldUpdate company =
    case fieldUpdate of
        CompanyName val ->
            { company | name = val } ! []

        CompanyLocation val ->
            { company | location = val } ! []

        CompanyAddPhone ->
            let
                emptyPhone =
                    { number = "", description = "" }
            in
                { company | phones = company.phones ++ [ emptyPhone ] } ! []

        CompanyRemovePhone idx ->
            { company | phones = List.Extra.removeAt idx company.phones } ! []

        CompanyUpdatePhone idx desc num ->
            let
                newPhone =
                    { description = desc, number = num }

                updatedPhones =
                    List.Extra.setAt idx newPhone company.phones
            in
                case updatedPhones of
                    Just phones ->
                        { company | phones = phones } ! []

                    Nothing ->
                        company ! []

        CompanyAddress val ->
            { company | address = val } ! []

        CompanyPostcode val ->
            { company | postcode = val } ! []

        CompanyEmail val ->
            { company | email = val } ! []

        CompanySector val ->
            { company | sector = val } ! []

        CompanyClassification val ->
            { company | classification = val } ! []

        CompanyInvestorClassification val ->
            { company | investorClassification = val } ! []

        CompanyInvestorType val ->
            { company | investorType = val } ! []

        CompanyWebsite val ->
            { company | website = val } ! []

        CompanyArchived val ->
            { company | archived = val } ! []

        CompanyNotes val ->
            { company | notes = val } ! []

        RContact index event ->
            let
                updateRelatedContacts newVal =
                    { company | relatedContacts = newVal }

                setRelatedContact idx updateFunc =
                    List.Extra.updateIfIndex
                        ((==) idx)
                        updateFunc
                        company.relatedContacts
            in
                case event of
                    Search text ->
                        let
                            search =
                                { text = text
                                , suggestions = RemoteData.Loading
                                }
                        in
                            ( updateRelatedContacts <|
                                setRelatedContact index
                                    (\relatedContact ->
                                        { relatedContact | search = search }
                                    )
                            , searchContact text
                                |> Cmd.map (\r -> RContact index (SearchUpdate r) |> UpdateField)
                            )

                    SearchUpdate update ->
                        ( updateRelatedContacts <|
                            setRelatedContact index
                                (\relatedContact ->
                                    { relatedContact
                                        | search =
                                            { text = relatedContact.search.text
                                            , suggestions = update
                                            }
                                    }
                                )
                        , Cmd.none
                        )

                    ChooseContact contact ->
                        ( updateRelatedContacts <|
                            setRelatedContact index
                                (\relatedContact ->
                                    { relatedContact
                                        | contact = contact
                                        , search = emptySearch
                                    }
                                )
                        , Cmd.none
                        )

                    BlurSearchBox ->
                        ( company
                        , Utils.Delay.delay 200 (RContact index (SearchUpdate RemoteData.NotAsked) |> UpdateField)
                        )

                    RemoveContact ->
                        ( updateRelatedContacts <|
                            List.Extra.removeAt index company.relatedContacts
                        , Cmd.none
                        )

                    AddContact ->
                        ( updateRelatedContacts <|
                            company.relatedContacts
                                ++ [ { relation = "", contact = Nothing, search = emptySearch } ]
                        , Cmd.none
                        )

                    SetRelation txt ->
                        ( updateRelatedContacts <|
                            setRelatedContact index
                                (\relatedContact ->
                                    { relatedContact
                                        | relation = txt
                                    }
                                )
                        , Cmd.none
                        )

        RequiredFiles subMsg ->
            { company | requiredFiles = updateRequiredFiles subMsg company.requiredFiles }
                ! []


updateRequiredFiles : RequiredFilesMsg -> List String -> List String
updateRequiredFiles msg requiredFiles =
    case msg of
        FAdd ->
            requiredFiles ++ [ "" ]

        FRemove idx ->
            List.Extra.removeAt idx requiredFiles

        FName idx name ->
            List.Extra.updateIfIndex
                ((==) idx)
                (always name)
                requiredFiles


toEditableCompany : Company -> EditableCompany
toEditableCompany company =
    { id = company.id
    , name = company.name
    , location = company.location
    , phones = company.phones
    , address = company.address
    , postcode = company.postcode
    , email = company.email
    , sector = company.sector
    , classification = company.classification
    , investorClassification = company.investorClassification
    , investorType = company.investorType
    , website = company.website
    , requiredFiles = company.requiredFiles
    , relatedContacts = List.map toEditableRelatedContact company.relatedContacts
    , archived = company.archived
    , notes = company.notes
    }


toEditableRelatedContact : CompanyContact -> EditableCompanyContact
toEditableRelatedContact cc =
    { relation = cc.relation
    , contact = cc.contact
    , search = { text = "", suggestions = RemoteData.NotAsked }
    }
