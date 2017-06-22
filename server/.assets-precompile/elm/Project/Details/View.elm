module Project.Details.View exposing (root)

import Project.Details.Types exposing (..)
import Project.Details.NewContact as NewContact
import Common.Types exposing (Project, ProjectType(..), ProjectContact, ContactS, Cost)
import Common.View
import Html exposing (Html, div, text, ul, li, text, span, i, a, input, h4, strong, img, p, label, button, textarea, select, option, ul, li, br)
import Html.Attributes exposing (class, style, rows, src, href, type_, value, selected, placeholder, target, for, checked)
import Html.Events exposing (onClick, onInput, on, targetValue)
import Utils.Api as Api
import Utils.FlashMessages as FlashMessages
import Utils.FileStorage.Main as FileStorage
import Utils.Routes as Routes
import Utils.SearchBox as SearchBox
import Utils.Ui as Ui exposing (formTitle, inputField, textAreaField, textField)
import Date.Extra
import Date exposing (Date)
import DatePicker
import Json.Decode
import Date.Extra
import RemoteData exposing (WebData)
import Utils.Number
import Http exposing (encodeUri)
import Utils.Time exposing (weekDayDayMonthYear)
import List.Extra


requiredFilesListConfig =
    { onAddItem = FAdd |> RequiredFiles |> Change
    , onRemoveItem = FRemove >> RequiredFiles >> Change
    }


contactsListConfig =
    { onAddItem = Change (Contacts CAdd)
    , onRemoveItem = CRemove >> Contacts >> Change
    }


ratesListConfig =
    { onAddItem = Change (Rates RAdd)
    , onRemoveItem = RRemove >> Rates >> Change
    }


root : Model -> Html Msg
root model =
    showWhenBothLoaded (view model) model.cost model.project


view : Model -> Cost -> ProjectWithPermission -> Html Msg
view model cost projectWPermission =
    let
        project =
            case projectWPermission of
                Editable proj ->
                    proj

                NonEditable proj ->
                    proj
    in
        div []
            [ h4
                [ class "clearfix" ]
                [ strong [] [ text "DETAILS" ]
                , toggleEditingButtons projectWPermission model.editable
                ]
            , Ui.submissionInfo "Changes saved." model.submission
            , Ui.submissionInfo "Project Deleted." model.deletion
            , case model.editable of
                Just editableProj ->
                    editableDetails editableProj

                Nothing ->
                    readOnlyDetails cost project model.fileStorage
            , model.reportModal
                |> Maybe.map (reportModal project)
                |> Maybe.withDefault (text "")
            ]


showWhenBothLoaded : (Cost -> ProjectWithPermission -> Html Msg) -> WebData Cost -> WebData ProjectWithPermission -> Html Msg
showWhenBothLoaded showFunc wCost wProjWPermission =
    case ( wCost, wProjWPermission ) of
        ( RemoteData.Success cost, RemoteData.Success projWPermission ) ->
            showFunc cost projWPermission

        ( RemoteData.Failure err1, RemoteData.Failure err2 ) ->
            div []
                [ Api.errorMessage err1
                    |> FlashMessages.failure
                , Api.errorMessage err2
                    |> FlashMessages.failure
                ]

        ( RemoteData.Failure err, _ ) ->
            Api.errorMessage err
                |> FlashMessages.failure

        ( _, RemoteData.Failure err ) ->
            Api.errorMessage err
                |> FlashMessages.failure

        ( RemoteData.Loading, RemoteData.Loading ) ->
            Ui.spinner

        ( RemoteData.Loading, _ ) ->
            Ui.spinner

        ( _, RemoteData.Loading ) ->
            Ui.spinner

        ( _, _ ) ->
            text ""



--------------------------------------------------------------------------------
--------- READ ONLY
--------------------------------------------------------------------------------


readOnlyDetails : Cost -> Project -> FileStorage.FileStorage -> Html Msg
readOnlyDetails cost project fileStorage =
    div []
        [ div
            [ onClick (ShowReportModal True) ]
            [ Common.View.costView cost
            ]
        , FileStorage.view fileStorage project.requiredFiles
            |> Html.map FileStorageMsg
        , formTitle "Title"
            [ textField project.name
            ]
        , formTitle "Type"
            [ case project.type_ of
                Retainer ->
                    textField "Retainer"

                Standard ->
                    textField "Standard"
            ]
        , formTitle "Company"
            [ a
                [ href <| Routes.companyPage project.company.id
                , class "form-control"
                ]
                [ text project.company.name ]
            ]
        , formTitle "Owner"
            [ a
                [ href <| Routes.userPage project.owner.id
                , class "form-control"
                ]
                [ text project.owner.email ]
            ]
        , div
            [ class "row" ]
            [ div
                [ class "col-sm-6" ]
                [ formTitle "Start Date"
                    [ textField <| weekDayDayMonthYear project.startDate
                    ]
                ]
            , div
                [ class "col-sm-6" ]
                [ formTitle "End Date"
                    [ textField (project.endDate |> Maybe.map weekDayDayMonthYear |> Maybe.withDefault "")
                    ]
                ]
            ]
        , formTitle "Budget"
            [ div [ class "input-group" ]
                [ span [ class "input-group-addon" ] [ i [ class "fa fa-gbp" ] [] ]
                , textField project.budget
                ]
            ]
        , formTitle "Description"
            [ textField project.description
            ]
        , formTitle "Related Contacts"
            [ div [] (List.map readOnlyContactRow project.contacts)
            ]
        , formTitle "Notes"
            [ textField project.notes
            ]
        , formTitle "Created by"
            [ textField project.created_by.email
            ]
        ]


toggleEditingButtons : ProjectWithPermission -> Maybe EditableProject -> Html Msg
toggleEditingButtons projWPermission mEditable =
    case projWPermission of
        NonEditable _ ->
            text ""

        Editable _ ->
            case mEditable of
                Nothing ->
                    a
                        [ class "pull-right text-primary" ]
                        [ i
                            [ class "text-big fa fa-pencil"
                            , style [ ( "cursor", "pointer" ) ]
                            , onClick (SetEditing True)
                            ]
                            []
                        ]

                Just editableProj ->
                    div [ class "pull-right text-primary" ]
                        [ button
                            [ class "btn btn-default waves-effect m-r-10"
                            , type_ "button"
                            , onClick (SetEditing False)
                            ]
                            [ text "Cancel" ]
                        , button
                            [ class "btn btn-primary waves-effect waves-light"
                            , type_ "button"
                            , onClick (Save editableProj)
                            ]
                            [ text "Save" ]
                        ]


readOnlyContactRow : ProjectContact ContactS -> Html Msg
readOnlyContactRow relatedContact =
    div
        [ class "row" ]
        [ div
            [ class "col-md-6" ]
            [ p
                [ class "form-control m-t-10" ]
                [ text relatedContact.description ]
            ]
        , div
            [ class "col-md-6" ]
            [ a
                [ href <| Routes.contactPage relatedContact.contact.id
                , class "form-control m-t-10"
                ]
                [ text relatedContact.contact.name ]
            ]
        ]



--------------------------------------------------------------------------------
--------- EDITABLE
--------------------------------------------------------------------------------


editableDetails : EditableProject -> Html Msg
editableDetails project =
    div []
        [ formTitle "Required Files"
            [ Ui.editableList
                requiredFilesListConfig
                "Add File +"
                (List.indexedMap
                    editableRequiredFileRow
                    project.requiredFiles
                )
            ]
        , formTitle "Title"
            [ inputField "A title" project.name (Name >> Change) ]
        , formTitle "Type"
            [ select
                [ class "form-control"
                , on "change" (Json.Decode.map (Type >> Change) targetValue)
                ]
                [ option [ selected (project.type_ == Standard) ] [ text "Standard" ]
                , option [ selected (project.type_ == Retainer) ] [ text "Retainer" ]
                ]
            ]
        , formTitle "Company"
            [ SearchBox.view project.company
                |> Html.map (CompanySearch >> Change)
            ]
        , formTitle "Owner"
            [ SearchBox.view project.owner
                |> Html.map (OwnerSearch >> Change)
            ]
        , div
            [ class "row" ]
            [ div
                [ class "col-sm-6" ]
                [ formTitle "Start Date"
                    [ DatePicker.view project.startDate
                        |> Html.map (StartDate >> Change)
                    ]
                ]
            , div
                [ class "col-sm-6" ]
                [ formTitle "End Date"
                    [ DatePicker.view project.endDate
                        |> Html.map (EndDate >> Change)
                    ]
                ]
            ]
        , formTitle "Budget"
            [ div [ class "input-group" ]
                [ span [ class "input-group-addon" ] [ i [ class "fa fa-gbp" ] [] ]
                , inputField "Type a budget" project.budget (Budget >> Change)
                ]
            ]
        , formTitle "Description"
            [ textAreaField "Project description" project.description (Description >> Change) ]
        , button
            [ class "btn btn-xs btn-primary"
            , style [ ( "float", "right" ) ]
            , onClick (NewContact |> Change)
            , Html.Attributes.title "Click here to add a new user to the database."
            ]
            [ text "Create a contact" ]
        , project.newContact
            |> Maybe.map (NewContact.view >> Html.map (NewContactUpdate >> Change))
            |> Maybe.withDefault (text "")
        , formTitle "Related Contacts"
            [ Ui.editableList
                contactsListConfig
                "New Contact +"
                (List.indexedMap editableContactRow project.contacts)
            ]
        , formTitle "Notes"
            [ textAreaField "Project notes" project.notes (Notes >> Change) ]
        , formTitle "Position Rates"
            [ Ui.editableList
                ratesListConfig
                "New Position Rate +"
                (List.indexedMap editablePositionRateRow project.rates)
            ]
        , formTitle "Created by"
            [ textField project.created_by.email
            ]
        , div
            [ class "text-right" ]
            [ button
                [ class "btn btn-danger"
                , onClick <| Change <| ToggleDeletionModal True
                ]
                [ text "Delete" ]
            ]
        , if project.deletionModalShowing then
            confirmDeletionModal project
          else
            text ""
        ]


editableContactRow : Int -> EditableRelatedContact -> Html Msg
editableContactRow idx relatedContact =
    div [ class "row" ]
        [ div [ class "col-md-6 m-b-10" ]
            [ inputField "Relation" relatedContact.description (CDescription idx >> Contacts >> Change)
            ]
        , div [ class "col-md-6 m-b-10" ]
            [ SearchBox.view relatedContact.contact
                |> Html.map (CSearch idx >> Contacts >> Change)
            ]
        ]


editablePositionRateRow : Int -> EditablePositionRate -> Html Msg
editablePositionRateRow idx positionRate =
    div [ class "row" ]
        [ div [ class "col-md-6 m-b-10" ]
            [ SearchBox.view positionRate.position
                |> Html.map (RSearch idx >> Rates >> Change)
            ]
        , div [ class "col-md-6 m-b-10" ]
            [ inputField "Hourly rate" positionRate.hourly_rate (RHourlyRate idx >> Rates >> Change)
            ]
        ]


editableRequiredFileRow : Int -> String -> Html Msg
editableRequiredFileRow idx fileName =
    div
        [ class "m-b-10" ]
        [ inputField
            "Type a file name"
            fileName
            (FName idx >> RequiredFiles >> Change)
        ]


reportModal : Project -> ReportModalState -> Html Msg
reportModal project modalState =
    Ui.modal
        [ Ui.modalHeader "Project Report" (ShowReportModal False)
        , Ui.modalBody
            [ p
                []
                [ text """
                  Select a date interval for which to see a report on the
                  amount of work done in the project and it's summed cost.
                  """
                ]
            , div
                [ class "row" ]
                [ div
                    [ class "col-sm-6" ]
                    [ formTitle "From Date"
                        [ DatePicker.view modalState.fromDate
                            |> Html.map ReportModalFromDate
                        ]
                    ]
                , div
                    [ class "col-sm-6" ]
                    [ formTitle "To Date"
                        [ DatePicker.view modalState.toDate
                            |> Html.map ReportModalToDate
                        ]
                    ]
                ]
            ]
        , Ui.modalFooter
            [ button [ class "btn btn-default waves-effect", onClick (ShowReportModal False) ]
                [ text "Cancel" ]
            , a
                [ class "btn btn-primary waves-effect waves-light"
                , target "blank"
                , href <|
                    "/projects/cost-report/"
                        ++ project.id
                        ++ "?fromDate="
                        ++ (DatePicker.getDate modalState.fromDate |> Maybe.map Date.Extra.toIsoString |> Maybe.withDefault "" |> encodeUri)
                        ++ "&toDate="
                        ++ (DatePicker.getDate modalState.toDate |> Maybe.map Date.Extra.toIsoString |> Maybe.withDefault "" |> encodeUri)
                ]
                [ text "Open Report" ]
            ]
        ]


confirmDeletionModal : EditableProject -> Html Msg
confirmDeletionModal editable =
    Ui.modal
        [ Ui.modalHeader "Delete project permanently" (Change <| ToggleDeletionModal False)
        , Ui.modalBody
            [ p []
                [ text "Are you sure you want to"
                , strong [] [ text " permanently delete " ]
                , text "the following project?"
                ]
            , h4 []
                [ text editable.name ]
            , br [] []
            , p []
                [ strong []
                    [ text "Important!" ]
                ]
            , p []
                [ text
                    """
                    This action cannot be undone. Once a project is deleted all deliverables, activities
                    and time recordings for that project are lost.
                    """
                ]
            ]
        , Ui.modalFooter
            [ button [ class "btn btn-default waves-effect", onClick (Change (ToggleDeletionModal False)) ]
                [ text "Cancel" ]
            , button [ class "btn btn-danger waves-effect waves-light", onClick (Delete editable) ]
                [ text "Delete project PERMANENTLY" ]
            ]
        ]
