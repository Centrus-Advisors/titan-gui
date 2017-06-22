module Deliverable.View exposing (..)

import Deliverable.Types exposing (..)
import Common.Types
    exposing
        ( Deliverable
        , DeliverableStatus(Completed, Cancelled, Active)
        , Cost
        )
import Common.View exposing (deliverableTypeSelector, deliverableTypeToString, deliverableStatus)
import Deliverable.TimeTracking.Main as TimeTracking
import RemoteData exposing (WebData)
import Html exposing (Html, div, text, h4, br, a, p, strong, i, button, input, label, span, select, option, img, textarea)
import Html.Attributes exposing (class, href, type_, style, value, disabled, for, id, checked, selected, src, rows)
import Html.Events exposing (onClick, onInput, on, targetValue)
import Utils.Routes as Routes
import Date exposing (Date)
import Utils.Api as Api
import Utils.FlashMessages as FlashMessages
import Utils.SearchBox as SearchBox
import DatePicker
import Utils.Ui as Ui exposing (formTitle, inputField, textAreaField, textField, formField)
import Date.Extra
import Json.Decode
import Utils.Number
import Utils.Time as TimeUtils exposing (weekDayDayMonthYear)


root : Model -> Html Msg
root model =
    showWhenBothLoaded (view model) model.cost model.deliverable


view : Model -> Cost -> DeliverableEdit -> Html Msg
view model cost delivType =
    div [ class "row" ]
        [ div
            [ class "col-md-3" ]
            [ div
                [ class "card-box" ]
                [ basicInfo model.todayDate (getDeliverable delivType) ]
            ]
        , div
            [ class "col-md-9" ]
            [ div
                [ class "card-box" ]
                [ div
                    [ class "row" ]
                    [ div
                        [ class "col-md-6" ]
                        [ details model cost delivType
                        ]
                    , div
                        [ class "col-md-6" ]
                        [ TimeTracking.view model.timeTracking
                            |> Html.map TimeTrackingMsg
                        ]
                    ]
                ]
            ]
        ]


showWhenBothLoaded : (Cost -> DeliverableEdit -> Html Msg) -> WebData Cost -> WebData DeliverableEdit -> Html Msg
showWhenBothLoaded showFunc wCost wDelivType =
    case ( wCost, wDelivType ) of
        ( RemoteData.Success cost, RemoteData.Success delivType ) ->
            showFunc cost delivType

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


getDeliverable : DeliverableEdit -> Deliverable
getDeliverable delivType =
    case delivType of
        Editable deliverable ->
            deliverable

        NonEditable deliverable ->
            deliverable


basicInfo : Date -> Deliverable -> Html Msg
basicInfo todayDate deliverable =
    div
        [ class "text-center" ]
        [ div
            [ class "thumb-xl member-thumb m-b-10 center-block" ]
            [ img
                [ class "img-circle img-thumbnail"
                , src "/assets/images/placeholders/placeholder-deliverable.svg"
                ]
                []
            ]
        , p
            [ class "text-muted" ]
            [ text "Deliverable" ]
        , h4
            []
            [ text deliverable.title
            ]
        , p
            [ class "text-muted" ]
            [ a
                [ href <| Routes.projectPage deliverable.project.id ]
                [ text deliverable.project.name ]
            ]
        , p
            []
            [ deliverableStatus todayDate deliverable ]
        ]


details : Model -> Cost -> DeliverableEdit -> Html Msg
details model cost delivType =
    div
        []
        [ h4
            [ class "clearfix" ]
            [ strong [] [ text "DETAILS" ]
            , toggleEditingButtons delivType model.editable
            ]
        , Ui.submissionInfo "Deliverable deleted." model.deletion
        , Ui.submissionInfo "Changes saved." model.submission
        , case model.editable of
            Nothing ->
                readOnlyDetails cost delivType

            Just editable ->
                editableDetails model editable
        ]


readOnlyDetails : Cost -> DeliverableEdit -> Html Msg
readOnlyDetails cost delivType =
    let
        deliverable =
            getDeliverable delivType
    in
        div []
            [ Common.View.costView cost
            , formTitle "Title"
                [ textField deliverable.title ]
            , formTitle "Type"
                [ textField (deliverableTypeToString deliverable.type_) ]
            , formTitle "Milestone"
                [ textField deliverable.milestone ]
            , formTitle "Description"
                [ textField deliverable.description ]
            , div
                [ class "row" ]
                [ div
                    [ class "col-md-6" ]
                    [ formTitle "Start Date"
                        [ textField <| weekDayDayMonthYear deliverable.startDate ]
                    ]
                , div
                    [ class "col-md-6" ]
                    [ formTitle "Deadline"
                        [ textField (deliverable.deadline |> Maybe.map weekDayDayMonthYear |> Maybe.withDefault "") ]
                    ]
                ]
            , formTitle "Owner"
                [ formField [ userLink deliverable.owner ] ]
            , formTitle "Assignees"
                [ if List.isEmpty deliverable.assignees then
                    textField "No assignees"
                  else
                    formField
                        (deliverable.assignees
                            |> List.map userLink
                            |> List.intersperse (br [] [])
                        )
                ]
            , formTitle "Project"
                [ textField deliverable.project.name ]
            , formTitle "Created by"
                [ textField deliverable.created_by.email ]
            ]


userLink user =
    a
        [ href <| Routes.userPage user.id ]
        [ text user.email ]


editableDetails : Model -> EditableDeliverable -> Html Msg
editableDetails model deliverable =
    div []
        [ formTitle "Title"
            [ inputField "Deliverable title" deliverable.title (Title >> Change) ]
        , formTitle "Type"
            [ deliverableTypeSelector (Type >> Change) deliverable.type_ ]
        , formTitle "Milestone"
            [ inputField "Milestone name" deliverable.milestone (Milestone >> Change) ]
        , formTitle "Description"
            [ textAreaField "Deliverable description" deliverable.description (Description >> Change) ]
        , div
            [ class "row" ]
            [ div
                [ class "col-md-6" ]
                [ formTitle "Start Date"
                    [ DatePicker.view deliverable.startDate
                        |> Html.map (StartDate >> Change)
                    ]
                ]
            , div
                [ class "col-md-6" ]
                [ formTitle "Deadline"
                    [ DatePicker.view deliverable.deadline
                        |> Html.map (Deadline >> Change)
                    ]
                ]
            ]
        , div
            [ class "row" ]
            [ div
                [ class "col-md-6" ]
                [ formTitle "Hourly Rate"
                    [ inputField "Pounds per one hour" deliverable.hourly_rate (HourlyRate >> Change) ]
                ]
            , div
                [ class "col-md-6" ]
                [ formTitle "Status"
                    [ select
                        [ class "form-control"
                        , on "change" (Json.Decode.map (Status >> Change) targetValue)
                        ]
                        [ option [ selected (deliverable.status == Active), value "active" ] [ text "Active" ]
                        , option [ selected (deliverable.status == Completed), value "completed" ] [ text "Completed" ]
                        , option [ selected (deliverable.status == Cancelled), value "cancelled" ] [ text "Cancelled" ]
                        ]
                    ]
                ]
            ]
        , formTitle "Owner"
            [ SearchBox.view deliverable.owner
                |> Html.map (Owner >> Change)
            ]
        , formTitle "Assignees"
            [ Ui.editableList
                assigneeListConfig
                "New Assignee +"
                (List.indexedMap assigneeSearch deliverable.assignees)
            ]
        , formTitle "Project"
            [ textField deliverable.project.name ]
        , formTitle "Created by"
            [ textField deliverable.created_by.email ]
        , div
            [ class "text-right" ]
            [ button
                [ class "btn btn-danger"
                , onClick <| Change <| ToggleDeletionModal True
                ]
                [ text "Delete" ]
            ]
        , if deliverable.deletionModalShowing then
            confirmDeletionModal deliverable
          else
            text ""
        ]


assigneeListConfig =
    { onAddItem = Change (Assignees AAdd)
    , onRemoveItem = ARemove >> Assignees >> Change
    }


assigneeSearch idx assignee =
    div [ class "m-b-10" ]
        [ SearchBox.view assignee ]
        |> Html.map (ASearch idx >> Assignees >> Change)


confirmDeletionModal : EditableDeliverable -> Html Msg
confirmDeletionModal editable =
    Ui.modal
        [ Ui.modalHeader "Delete deliverable permanently" (Change <| ToggleDeletionModal False)
        , Ui.modalBody
            [ p []
                [ text "Are you sure you want to"
                , strong [] [ text " permanently delete " ]
                , text "the following deliverable?"
                ]
            , h4 []
                [ text editable.title ]
            , br [] []
            , p []
                [ strong []
                    [ text "Important!" ]
                ]
            , p []
                [ text
                    """
                    This action cannot be undone. Once a deliverable is deleted all time
                    recordings for that deliverable are lost and their cost is removed from
                    the project's total cost.
                    """
                ]
            , br [] []
            , p []
                [ strong []
                    [ text "Cancel instead" ]
                ]
            , p []
                [ text
                    """
                    If the deliverable was requested but is not going forward, set it's status
                    as "Cancelled" instead of completely deleting the it. The time recordings for
                    the deliverable will not be added to the project but there will still be a record
                    that they happened. Cancelling a deliverable leaves no record of its existence
                    in the project.
                    """
                ]
            ]
        , Ui.modalFooter
            [ button [ class "btn btn-default waves-effect", onClick (Change (ToggleDeletionModal False)) ]
                [ text "Cancel" ]
            , button [ class "btn btn-danger waves-effect waves-light", onClick (Delete editable) ]
                [ text "Delete Deliverable Permanently" ]
            ]
        ]


checkBox : Bool -> Bool -> (Bool -> Msg) -> Html Msg
checkBox isDisabled isChecked msg =
    div [ class "form-group col-md-4" ]
        [ div
            [ class "checkbox checkbox-success"
            , onClick (not isChecked |> msg)
            ]
            [ input
                [ id "deliverable-completed-checkbox"
                , type_ "checkbox"
                , checked isChecked
                , disabled isDisabled
                ]
                []
            , label
                [ for "deliverable-completed-checkbox"
                ]
                [ text "Completed" ]
            ]
        ]


toggleEditingButtons : DeliverableEdit -> Maybe EditableDeliverable -> Html Msg
toggleEditingButtons delivType mEditable =
    case delivType of
        Editable _ ->
            case mEditable of
                Just editable ->
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
                            , onClick (Submit editable)
                            ]
                            [ text "Save" ]
                        ]

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

        NonEditable _ ->
            text ""
