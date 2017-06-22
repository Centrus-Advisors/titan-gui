module ContactDetails.State exposing (..)

import ContactDetails.Types exposing (..)
import Common.Types exposing (Contact, CompanyS, Phone)
import List.Extra exposing (..)
import RemoteData
import Utils.Http
import ContactDetails.Rest exposing (fetchContact, searchCompany, submitContactUpdate)
import ContactDetails.Headers as Headers
import Utils.Activities.Main as Activities
import Utils.FileStorage.Main as FileStorage
import Time exposing (Time)
import Process
import Task


init : { time : Time, contactId : String } -> ( Model, Cmd Msg )
init { time, contactId } =
    let
        ( activities, activitiesMsg ) =
            Activities.initFromContact time contactId

        ( fileStorage, fileStorageMsg ) =
            FileStorage.initFromContact contactId
    in
        { loadedModel = RemoteData.Loading
        , activities = activities
        , fileStorage = fileStorage
        }
            ! [ fetchContact contactId
                    |> Cmd.map ContactFetch
              , Cmd.map ActivitiesMsg activitiesMsg
              , Cmd.map FileStorageMsg fileStorageMsg
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg topModel =
    case msg of
        ContactFetch result ->
            case result of
                RemoteData.Success contact ->
                    let
                        loadedModel =
                            RemoteData.Success
                                { contact = contact
                                , editableContact = Nothing
                                , submission = RemoteData.NotAsked
                                , companySearch =
                                    { suggestions = RemoteData.NotAsked
                                    , text = ""
                                    }
                                }
                    in
                        ( { topModel | loadedModel = loadedModel }
                        , Headers.set contact
                        )

                RemoteData.Failure err ->
                    ( { topModel | loadedModel = RemoteData.Failure err }, Cmd.none )

                RemoteData.Loading ->
                    ( { topModel | loadedModel = RemoteData.Loading }, Cmd.none )

                RemoteData.NotAsked ->
                    ( { topModel | loadedModel = RemoteData.NotAsked }, Cmd.none )

        ActivitiesMsg dMsg ->
            Activities.update dMsg topModel.activities
                |> Tuple.mapFirst (\a -> { topModel | activities = a })
                |> Tuple.mapSecond (Cmd.map ActivitiesMsg)

        FileStorageMsg subMsg ->
            FileStorage.update subMsg topModel.fileStorage
                |> Tuple.mapFirst (\fs -> { topModel | fileStorage = fs })
                |> Tuple.mapSecond (Cmd.map FileStorageMsg)

        _ ->
            case topModel.loadedModel of
                RemoteData.Success model ->
                    subUpdate msg model
                        |> Tuple.mapFirst (\v -> { topModel | loadedModel = RemoteData.Success v })

                _ ->
                    ( topModel, Cmd.none )


subUpdate : Msg -> LoadedModel -> ( LoadedModel, Cmd Msg )
subUpdate msg model =
    case msg of
        ContactFetch _ ->
            ( model, Cmd.none )

        ActivitiesMsg dMsg ->
            ( model, Cmd.none )

        FileStorageMsg subMsg ->
            ( model, Cmd.none )

        UpdateField fieldUpdate ->
            let
                editableContact =
                    model.editableContact
                        |> Maybe.map (\c -> updateContactField c fieldUpdate)
            in
                ( { model | editableContact = editableContact }
                , Cmd.none
                )

        EnableEditing ->
            ( { model
                | editableContact = Just model.contact
                , submission = RemoteData.NotAsked
              }
            , Cmd.none
            )

        SaveEditing ->
            case model.editableContact of
                Just aContact ->
                    ( { model
                        | submission = RemoteData.Loading
                      }
                    , submitContactUpdate aContact
                        |> Cmd.map SubmissionUpdate
                    )

                Nothing ->
                    ( model, Cmd.none )

        CancelEditing ->
            ( { model
                | editableContact = Nothing
                , submission = RemoteData.NotAsked
              }
            , Cmd.none
            )

        SubmissionUpdate update ->
            case update of
                RemoteData.Success upToDateContact ->
                    ( { model
                        | submission = update
                        , contact = upToDateContact
                        , editableContact = Nothing
                      }
                    , Headers.set upToDateContact
                    )

                _ ->
                    ( { model | submission = update }
                    , Cmd.none
                    )

        SearchCompany text ->
            let
                companySearch =
                    model.companySearch
            in
                ( { model
                    | companySearch =
                        { companySearch
                            | text = text
                            , suggestions = RemoteData.Loading
                        }
                  }
                , searchCompany text
                    |> Cmd.map SearchCompanyUpdate
                )

        SearchCompanyUpdate update ->
            let
                companySearch =
                    model.companySearch
            in
                ( { model | companySearch = { companySearch | suggestions = update } }
                , Cmd.none
                )

        SearchCompanyBoxBlur ->
            ( model
            , delay 200 (SearchCompanyUpdate RemoteData.NotAsked)
            )

        ChooseCompany company ->
            let
                companySearch =
                    model.companySearch

                editableContact =
                    model.editableContact
                        |> Maybe.map (\c -> { c | company = company })
            in
                ( { model
                    | editableContact = editableContact
                    , companySearch = { companySearch | text = "", suggestions = RemoteData.NotAsked }
                  }
                , Cmd.none
                )


delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


updateContactField : Contact -> ContactFieldUpdate -> Contact
updateContactField contact fieldUpdate =
    case fieldUpdate of
        ContactName val ->
            { contact | name = val }

        ContactPosition val ->
            { contact | position = val }

        ContactAddPhone ->
            { contact | phones = contact.phones ++ [ Phone "" "" ] }

        ContactRemovePhone idx ->
            { contact | phones = List.Extra.removeAt idx contact.phones }

        ContactUpdatePhone idx desc num ->
            let
                newPhone =
                    { description = desc, number = num }

                updatedPhones =
                    List.Extra.setAt idx newPhone contact.phones
            in
                case updatedPhones of
                    Just phones ->
                        { contact | phones = phones }

                    Nothing ->
                        contact

        ContactEmail val ->
            { contact | email = val }

        ContactLocation val ->
            { contact | location = val }

        ContactAddress val ->
            { contact | address = val }

        ContactNotes val ->
            { contact | notes = val }

        ContactGender val ->
            { contact | gender = val }

        RequiredFiles subMsg ->
            { contact | requiredFiles = updateRequiredFiles subMsg contact.requiredFiles }


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
