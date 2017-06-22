module Project.Details.State exposing (init, update, getProject)

import Project.Details.Types exposing (..)
import Common.Types
    exposing
        ( Project
        , Position
        , ProjectType(Retainer, Standard)
        , UserS
        , ContactS
        , CompanyS
        , ProjectContact
        , Cost
        )
import Project.Common.Rest exposing (searchContacts, searchUsers)
import Project.Details.Rest
    exposing
        ( encodeEditableProject
        , searchCompanies
        , searchPositions
        , fetchCost
        , fetchProject
        , saveProject
        , deleteProject
        )
import Project.Details.NewContact as NewContact
import Utils.SearchBox as SearchBox
import RemoteData
import Utils.Routes
import Utils.Api
import Utils.Http
import List.Extra
import Maybe.Extra
import DatePicker
import Date.Extra
import Date exposing (Date)
import Utils.Time exposing (weekDayDayMonthYear)
import Utils.FileStorage.Main as FileStorage
import Json.Encode
import Navigation


defaultSettings =
    let
        datePickerDefault =
            DatePicker.defaultSettings
    in
        { datePickerDefault
            | pickedDate = Nothing
            , inputClassList = [ ( "form-control", True ) ]
            , dateFormatter = weekDayDayMonthYear
        }


init : ProjectID -> ( Model, Cmd Msg )
init projectID =
    let
        ( fileStorage, fileStorageMsg ) =
            FileStorage.initFromProject projectID
    in
        { project = RemoteData.Loading
        , cost = RemoteData.Loading
        , editable = Nothing
        , submission = RemoteData.NotAsked
        , deletion = RemoteData.NotAsked
        , reportModal = Nothing
        , fileStorage = fileStorage
        }
            ! [ fetchCost projectID
                    |> Cmd.map CostFetch
              , fetchProject projectID
                    |> Cmd.map ProjectFetch
              , fileStorageMsg |> Cmd.map FileStorageMsg
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            ( model, Cmd.none )

        CostFetch status ->
            { model | cost = status } ! []

        ProjectFetch status ->
            { model | project = status } ! []

        Change fieldMsg ->
            model.editable
                |> Maybe.map (updateField fieldMsg)
                |> Maybe.map (Tuple.mapFirst (\e -> { model | editable = Just e }))
                |> Maybe.withDefault ( model, Cmd.none )

        SetEditing enable ->
            case ( enable, model.project ) of
                ( True, RemoteData.Success projectType ) ->
                    case toEditable projectType of
                        Nothing ->
                            model ! []

                        Just ( editable, cmd ) ->
                            { model | editable = Just editable }
                                ! [ cmd ]

                _ ->
                    { model | editable = Nothing } ! []

        Save editable ->
            { model | submission = RemoteData.Loading }
                ! [ saveProject editable
                        |> Cmd.map SubmissionUpdate
                  ]

        SubmissionUpdate status ->
            case status of
                RemoteData.Success _ ->
                    { model
                        | project = status
                        , submission = status
                        , editable = Nothing
                    }
                        ! []

                _ ->
                    { model | submission = status } ! []

        Delete editable ->
            { model | deletion = RemoteData.Loading }
                ! [ deleteProject editable
                        |> Cmd.map DeletionUpdate
                  ]

        DeletionUpdate status ->
            case status of
                RemoteData.Success _ ->
                    { model
                        | deletion = status
                        , editable = Nothing
                    }
                        ! []

                _ ->
                    { model | deletion = status } ! []

        ShowReportModal shouldShow ->
            if shouldShow then
                let
                    fromDate =
                        getProject model
                            |> Maybe.map .startDate

                    toDate =
                        getProject model
                            |> Maybe.map (\p -> p.endDate |> Maybe.withDefault p.startDate)

                    ( fromDatePicker, _ ) =
                        DatePicker.init
                            { defaultSettings | pickedDate = fromDate }

                    ( toDatePicker, _ ) =
                        DatePicker.init
                            { defaultSettings | pickedDate = toDate }
                in
                    { model
                        | reportModal =
                            Just
                                { fromDate = fromDatePicker
                                , toDate = toDatePicker
                                }
                    }
                        ! []
            else
                { model | reportModal = Nothing } ! []

        ReportModalFromDate subMsg ->
            case model.reportModal of
                Nothing ->
                    model ! []

                Just reportModal ->
                    let
                        ( newPicker, newPickerMsg, _ ) =
                            DatePicker.update subMsg reportModal.fromDate

                        newReportModal =
                            model.reportModal
                                |> Maybe.map (\r -> { r | fromDate = newPicker })
                    in
                        ( { model | reportModal = newReportModal }
                        , Cmd.map ReportModalFromDate newPickerMsg
                        )

        ReportModalToDate subMsg ->
            case model.reportModal of
                Nothing ->
                    model ! []

                Just reportModal ->
                    let
                        ( newPicker, newPickerMsg, _ ) =
                            DatePicker.update subMsg reportModal.toDate

                        newReportModal =
                            model.reportModal
                                |> Maybe.map (\r -> { r | toDate = newPicker })
                    in
                        ( { model | reportModal = newReportModal }
                        , Cmd.map ReportModalFromDate newPickerMsg
                        )

        FileStorageMsg subMsg ->
            FileStorage.update subMsg model.fileStorage
                |> Tuple.mapFirst (\fs -> { model | fileStorage = fs })
                |> Tuple.mapSecond (Cmd.map FileStorageMsg)


updateField : FieldMsg -> EditableProject -> ( EditableProject, Cmd Msg )
updateField fieldMsg proj =
    case fieldMsg of
        Name val ->
            ( { proj | name = val }, Cmd.none )

        Description val ->
            ( { proj | description = val }, Cmd.none )

        Type val ->
            if val == "Retainer" then
                ( { proj | type_ = Retainer }, Cmd.none )
            else
                ( { proj | type_ = Standard }, Cmd.none )

        StartDate subMsg ->
            let
                ( newPicker, newPickerMsg, _ ) =
                    DatePicker.update subMsg proj.startDate
            in
                ( { proj | startDate = newPicker }
                , Cmd.map (Change << StartDate) newPickerMsg
                )

        EndDate subMsg ->
            let
                ( newPicker, newPickerMsg, _ ) =
                    DatePicker.update subMsg proj.endDate
            in
                ( { proj | endDate = newPicker }
                , Cmd.map (Change << EndDate) newPickerMsg
                )

        Budget val ->
            ( { proj | budget = val }, Cmd.none )

        Notes val ->
            ( { proj | notes = val }, Cmd.none )

        CompanySearch searchMsg ->
            SearchBox.update searchMsg proj.company
                |> Tuple.mapSecond (Cmd.map (CompanySearch >> Change))
                |> Tuple.mapFirst (\c -> { proj | company = c })

        OwnerSearch searchMsg ->
            SearchBox.update searchMsg proj.owner
                |> Tuple.mapSecond (Cmd.map (OwnerSearch >> Change))
                |> Tuple.mapFirst (\c -> { proj | owner = c })

        Contacts contactsMsg ->
            updateContacts contactsMsg proj.contacts
                |> Tuple.mapFirst (\c -> { proj | contacts = c })

        Rates ratesMsg ->
            updateRates ratesMsg proj.rates
                |> Tuple.mapFirst (\c -> { proj | rates = c })

        RequiredFiles subMsg ->
            { proj
                | requiredFiles =
                    updateRequiredFiles subMsg proj.requiredFiles
            }
                ! []

        NewContact ->
            { proj | newContact = Just NewContact.empty } ! []

        NewContactCancel ->
            { proj | newContact = Nothing } ! []

        NewContactUpdate subMsg ->
            proj.newContact
                |> Maybe.map (NewContact.update newContactConfig subMsg)
                |> Maybe.map (Tuple.mapFirst (\n -> { proj | newContact = Just n }))
                |> Maybe.withDefault ( proj, Cmd.none )

        NewContactCreated contact ->
            { proj
                | newContact = Nothing
                , contacts = proj.contacts ++ [ newRelatedContact (Just contact) ]
            }
                ! []

        ToggleDeletionModal showing ->
            { proj | deletionModalShowing = showing }
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


updateContacts : ContactsMsg -> List EditableRelatedContact -> ( List EditableRelatedContact, Cmd Msg )
updateContacts contactsMsg contacts =
    case contactsMsg of
        CAdd ->
            ( contacts ++ [ newRelatedContact Nothing ]
            , Cmd.none
            )

        CRemove idx ->
            ( List.Extra.removeAt idx contacts
            , Cmd.none
            )

        CDescription idx desc ->
            ( List.Extra.updateIfIndex
                ((==) idx)
                (\c -> { c | description = desc })
                contacts
            , Cmd.none
            )

        CSearch idx searchMsg ->
            let
                setContact newContact relatedContact =
                    { relatedContact
                        | contact = newContact
                    }

                updateContactInList c =
                    List.Extra.updateIfIndex
                        ((==) idx)
                        (setContact c)
                        contacts
            in
                List.Extra.getAt idx contacts
                    |> Maybe.map .contact
                    --> Maybe ContactS
                    |>
                        Maybe.map (contactSearch idx searchMsg)
                    --> Maybe (ContactS, Cmd Msg)
                    |>
                        Maybe.map (Tuple.mapFirst updateContactInList)
                    --> Maybe (List EditableRelatedContact, Cmd Msg)
                    |>
                        Maybe.withDefault ( contacts, Cmd.none )


updateRates : RatesMsg -> List EditablePositionRate -> ( List EditablePositionRate, Cmd Msg )
updateRates ratesMsg posRates =
    case ratesMsg of
        RAdd ->
            ( posRates ++ [ emptyRate ]
            , Cmd.none
            )

        RRemove idx ->
            ( List.Extra.removeAt idx posRates
            , Cmd.none
            )

        RHourlyRate idx desc ->
            ( List.Extra.updateIfIndex
                ((==) idx)
                (\c -> { c | hourly_rate = desc })
                posRates
            , Cmd.none
            )

        RSearch idx searchMsg ->
            let
                setPos newPosition posRate =
                    { posRate
                        | position = newPosition
                    }

                updatePosInList c =
                    List.Extra.updateIfIndex
                        ((==) idx)
                        (setPos c)
                        posRates
            in
                List.Extra.getAt idx posRates
                    |> Maybe.map .position
                    --> Maybe Rate
                    |>
                        Maybe.map (positionSearch idx searchMsg)
                    --> Maybe (Rate, Cmd Msg)
                    |>
                        Maybe.map (Tuple.mapFirst updatePosInList)
                    --> Maybe (List EditablePositionRate, Cmd Msg)
                    |>
                        Maybe.withDefault ( posRates, Cmd.none )


contactSearch idx searchMsg contact =
    SearchBox.update searchMsg contact
        |> Tuple.mapSecond (Cmd.map (CSearch idx >> Contacts >> Change))


positionSearch idx searchMsg pos =
    SearchBox.update searchMsg pos
        |> Tuple.mapSecond (Cmd.map (RSearch idx >> Rates >> Change))


searchBoxConfig : String -> (String -> Cmd (RemoteData.WebData (List { a | name : String }))) -> SearchBox.Config { a | name : String }
searchBoxConfig placeholder searchFunc =
    { renderItem = .name
    , renderError = Utils.Http.errorMessage
    , onSearch = searchFunc
    , placeholder = placeholder
    }


userSearchBoxConfig : SearchBox.Config UserS
userSearchBoxConfig =
    { renderItem = .email
    , renderError = Utils.Http.errorMessage
    , onSearch = searchUsers
    , placeholder = "User email"
    }


newRelatedContact : Maybe ContactS -> EditableRelatedContact
newRelatedContact mContact =
    { description = ""
    , contact = SearchBox.init (searchBoxConfig "Contact name" searchContacts) mContact
    }


emptyRate : EditablePositionRate
emptyRate =
    { hourly_rate = "0"
    , position = SearchBox.init (searchBoxConfig "Position name" searchPositions) Nothing
    }


newContactConfig =
    { onUpdate = NewContactUpdate >> Change
    , onCreated = NewContactCreated >> Change
    , onCancel = NewContactCancel |> Change
    }


toEditable : ProjectWithPermission -> Maybe ( EditableProject, Cmd Msg )
toEditable projectType =
    case projectType of
        NonEditable _ ->
            Nothing

        Editable project ->
            let
                contacts =
                    List.map
                        (\rc ->
                            { description = rc.description
                            , contact = SearchBox.init (searchBoxConfig "Contact name" searchContacts) (Just rc.contact)
                            }
                        )
                        project.contacts

                rates =
                    List.map
                        (\r ->
                            { hourly_rate = r.hourly_rate
                            , position = SearchBox.init (searchBoxConfig "Position name" searchPositions) (Just r.position)
                            }
                        )
                        project.rates

                ( startDatePicker, startDatePickerMsg ) =
                    DatePicker.init
                        { defaultSettings | pickedDate = Just project.startDate }

                ( endDatePicker, endDatePickerMsg ) =
                    DatePicker.init
                        { defaultSettings | pickedDate = project.endDate }

                editable =
                    { id = project.id
                    , name = project.name
                    , description = project.description
                    , type_ = project.type_
                    , startDate = startDatePicker
                    , endDate = endDatePicker
                    , budget = project.budget
                    , requiredFiles = project.requiredFiles
                    , notes = project.notes
                    , company = SearchBox.init (searchBoxConfig "Company name" searchCompanies) (Just project.company)
                    , owner = SearchBox.init userSearchBoxConfig (Just project.owner)
                    , contacts = contacts
                    , rates = rates
                    , created_by = project.created_by
                    , newContact = Nothing
                    , deletionModalShowing = False
                    }

                cmd =
                    Cmd.batch
                        [ Cmd.map (Change << StartDate) startDatePickerMsg
                        , Cmd.map (Change << EndDate) endDatePickerMsg
                        ]
            in
                Just ( editable, cmd )


getProject : Model -> Maybe Project
getProject model =
    case model.project of
        RemoteData.Success projWPermission ->
            case projWPermission of
                Editable project ->
                    Just project

                NonEditable project ->
                    Just project

        _ ->
            Nothing
