module NewProject.View exposing (root)

import NewProject.Types exposing (..)
import Common.Types exposing (ProjectType(Standard, Retainer), ProjectS)
import Html exposing (Html, div, a, text, button, small, label, p, input, h4, select, option, i, span, textarea)
import Html.Attributes exposing (disabled, class, checked, type_, name, placeholder, href, style, value, selected)
import Html.Events exposing (onClick, onInput, on, targetValue)
import Utils.Ui as Ui
import Utils.SearchBox as SearchBox
import RemoteData exposing (WebData)
import DatePicker
import Json.Decode


root : Model -> Html Msg
root model =
    case model.modal of
        Nothing ->
            text ""

        Just modal ->
            case model.submission of
                RemoteData.Success _ ->
                    successModal model.submission

                _ ->
                    editProjectModal modal model.submission


successModal : WebData ProjectS -> Html Msg
successModal submission =
    Ui.modal
        [ Ui.modalHeader "New Project" CloseModal
        , Ui.modalBody
            [ Ui.submissionInfo "Project Created" submission ]
        , Ui.modalFooter
            [ button
                [ class "btn btn-default waves-effect"
                , onClick (CloseModal)
                ]
                [ text "Done" ]
            ]
        ]


editProjectModal : NewProject -> WebData ProjectS -> Html Msg
editProjectModal project submission =
    Ui.modal
        [ Ui.modalHeader "New Project" CloseModal
        , Ui.modalBody
            [ Ui.submissionInfo "Project Created" submission
            , div [ class "row" ]
                [ div [ class "col-md-6" ]
                    [ header "Name"
                        [ inputField "Project Name" project.name (PName >> Change) ]
                    ]
                , div [ class "col-md-6" ]
                    [ header "Type"
                        [ select
                            [ class "form-control"
                            , on "change" (Json.Decode.map (PType >> Change) targetValue)
                            ]
                            [ option [ selected (project.type_ == Standard) ] [ text "Standard" ]
                            , option [ selected (project.type_ == Retainer) ] [ text "Retainer" ]
                            ]
                        ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-6" ]
                    [ header "Start date"
                        [ div [ class "input-group" ]
                            [ DatePicker.view project.startDate
                                |> Html.map (PStartDate >> Change)
                            , span
                                [ class "input-group-addon bg-primary b-0 text-white" ]
                                [ i [ class "ion-calendar" ] [] ]
                            ]
                        ]
                    ]
                , div [ class "col-md-6" ]
                    [ header "End date"
                        [ div [ class "input-group" ]
                            [ DatePicker.view project.endDate
                                |> Html.map (PEndDate >> Change)
                            , span
                                [ class "input-group-addon bg-primary b-0 text-white" ]
                                [ i [ class "ion-calendar" ] [] ]
                            ]
                        ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-12" ]
                    [ header "Company"
                        [ SearchBox.view project.company
                            |> Html.map (PCompany >> Change)
                        ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-6" ]
                    [ header "Owner"
                        [ SearchBox.view project.owner
                            |> Html.map (POwner >> Change)
                        ]
                    ]
                , div
                    [ class "col-md-6" ]
                    [ header "Budget"
                        [ inputField "00.00" project.budget (PBudget >> Change)
                        ]
                    ]
                ]
            , div [ class "row" ]
                [ div [ class "col-md-12" ]
                    [ header "Description"
                        [ textarea
                            [ class "form-control"
                            , value project.description
                            , onInput (PDescription >> Change)
                            , placeholder "Project description"
                            ]
                            []
                        ]
                    ]
                ]
            ]
        , Ui.modalFooter
            [ button
                [ class "btn btn-default waves-effect"
                , onClick CloseModal
                ]
                [ text "Done" ]
            , button
                [ class "btn btn-primary waves-effect waves-light"
                , onClick Submit
                ]
                [ text "Create Project" ]
            ]
        ]


header : String -> List (Html Msg) -> Html Msg
header txt content =
    div [ class "form-group" ]
        ([ label [] [ text txt ] ]
            ++ content
        )


inputField : String -> String -> (String -> Msg) -> Html Msg
inputField placeholderTxt contentTxt onInputMsg =
    input
        [ class "form-control"
        , style [ ( "min-height", "34px" ), ( "height", "auto" ) ]
        , type_ "text"
        , value contentTxt
        , placeholder placeholderTxt
        , onInput onInputMsg
        ]
        []
