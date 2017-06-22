module NewDeliverable.View exposing (root)

import NewDeliverable.Types exposing (..)
import Common.Types
    exposing
        ( DeliverableStatus(Cancelled, Completed, Active)
        , DeliverableType
            ( DReport
            , DMeeting
            , DAnalysis
            , DPhone
            , DEmail
            , DBusinessDevelopment
            , DServiceTicket
            , DTechnical
            , DOther
            )
        )
import Common.View exposing (deliverableTypeSelector)
import Html exposing (Html, div, text, strong, thead, tbody, h4, h3, tr, th, td, input, label, table, a, span, button)
import Html.Attributes exposing (class, href, value, style, type_, selected, placeholder, disabled)
import Html.Events exposing (onInput, onClick, on, targetValue)
import Utils.SearchBox as SearchBox
import DatePicker
import Utils.Ui as Ui
import RemoteData exposing (WebData)
import Json.Decode


root : Model msg -> Html Msg
root model =
    case model.submission of
        RemoteData.Success _ ->
            Ui.modal
                [ Ui.modalHeader "New Deliverable" (Cancel)
                , Ui.modalBody
                    [ Ui.submissionInfo "Deliverable created." model.submission ]
                ]

        _ ->
            Ui.modal
                [ Ui.modalHeader "New Deliverable" (Cancel)
                , Ui.modalBody
                    [ Ui.submissionInfo "Deliverable created." model.submission
                    , newDeliverableForm model.newDeliverable
                    ]
                , Ui.modalFooter
                    [ button [ class "btn btn-default waves-effect", onClick (Cancel) ]
                        [ text "Cancel" ]
                    , button [ class "btn btn-primary waves-effect waves-light", onClick (Create model.newDeliverable) ]
                        [ text "Save" ]
                    ]
                ]


newDeliverableForm d =
    div []
        [ Ui.formTitle "Project"
            [ SearchBox.view d.project
                |> Html.map (DProject >> Change)
            ]
        , Ui.formTitle "Title"
            [ Ui.inputField "Deliverable title" d.title (Title >> Change) ]
        , Ui.formTitle "Type"
            [ deliverableTypeSelector (DType >> Change) d.type_ ]
        , Ui.formTitle "Milestone"
            [ Ui.inputField "Milestone name" d.milestone (Milestone >> Change) ]
        , div
            [ class "row" ]
            [ div
                [ class "col-sm-6" ]
                [ Ui.formTitle "Start Date"
                    [ DatePicker.view d.startDate
                        |> Html.map (StartDate >> Change)
                    ]
                ]
            , div
                [ class "col-sm-6" ]
                [ Ui.formTitle "Deadline"
                    [ DatePicker.view d.deadline
                        |> Html.map (Deadline >> Change)
                    ]
                ]
            ]
        , Ui.formTitle "Owner"
            [ SearchBox.view d.owner
                |> Html.map (Owner >> Change)
            ]
        ]
