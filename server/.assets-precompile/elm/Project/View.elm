module Project.View exposing (root)

import Project.Types exposing (..)
import Project.Details.Details as Details
import Project.Deliverables.Deliverables as Deliverables
import Common.Types exposing (Project)
import Html exposing (Html, div, text, ul, li, text, span, i, a, input, h4, strong, img, p, label, button)
import Html.Attributes exposing (class, style, src, href, type_, value)
import Html.Events exposing (onClick, onInput)
import RemoteData
import Utils.Api
import Utils.FlashMessages
import Utils.Routes as Routes
import Utils.Activities.Main as Activities
import Utils.Ui as Ui


root : Model -> Html Msg
root model =
    div [ class "row" ]
        [ tabs model
        , div
            [ class "col-md-9" ]
            [ div [ class "card-box" ]
                [ case model.tab of
                    DetailsTab ->
                        detailsTab model

                    MilestonesTab ->
                        milestonesTab model

                    DeliverablesTab ->
                        deliverablesTab model
                ]
            ]
        ]


spinner =
    div [ class "spinningCircle" ] []


tabs : Model -> Html Msg
tabs model =
    let
        classForTab aTab =
            if model.tab == aTab then
                "list-group-item selected-tab-info"
            else
                "list-group-item"

        projectName =
            Details.getProject model.details
                |> Maybe.map projectDetails
                |> Maybe.withDefault (text "")
    in
        div
            [ class "col-md-3" ]
            [ projectName
            , ul [ class "list-group list-group-tabs" ]
                [ li [ class <| classForTab DetailsTab, onClick <| ChangeTab DetailsTab ]
                    [ text "Details" ]
                , li [ class <| classForTab DeliverablesTab, onClick <| ChangeTab DeliverablesTab ]
                    [ text "Deliverables" ]
                  -- , li [ class <| classForTab MilestonesTab, onClick <| ChangeTab MilestonesTab ]
                  --     [ text "Milestones" ]
                ]
            ]


projectDetails : Project -> Html Msg
projectDetails project =
    div [ class "card-box text-center" ]
        [ div
            [ class "thumb-xl member-thumb m-b-10 center-block" ]
            [ img
                [ class "img-circle img-thumbnail"
                , src "/assets/images/placeholders/placeholder-project.svg"
                ]
                []
            ]
        , p
            [ class "text-muted" ]
            [ text "Project" ]
        , h4
            []
            [ text project.name
            ]
        , p
            [ class "text-muted " ]
            [ a
                [ href <| Routes.companyPage project.company.id ]
                [ text project.company.name ]
            ]
        ]


detailsTab : Model -> Html Msg
detailsTab model =
    div [ class "row" ]
        [ div
            [ class "col-md-6" ]
            [ Details.view model.details
                |> Html.map DetailsMsg
            ]
        , div
            [ class "col-md-6" ]
            [ Activities.view model.activities
                |> Html.map ActivitiesMsg
            ]
        ]


milestonesTab arg =
    div [] [ text "Milestones tab" ]


deliverablesTab : Model -> Html Msg
deliverablesTab model =
    Deliverables.view model.todayDate model.deliverables
        |> Html.map DeliverablesMsg
