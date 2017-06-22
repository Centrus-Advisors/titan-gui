module Utils.Activities.ActivityList.View exposing (view)

import Utils.Activities.CommonTypes exposing (..)
import Utils.Activities.ActivityList.Types exposing (..)
import Common.Types exposing (ProjectS, ContactS)
import Html exposing (Html, div, text, h4, strong, i, a, input, select, option, p, span, button, small, label, textarea)
import Html.Attributes exposing (class, href, style, disabled, selected, type_, name, placeholder, value, rows)
import Html.Events exposing (onClick, onInput, on, targetValue)
import Date.Extra
import Date exposing (Date)
import Utils.Ui as Ui
import Utils.Api
import Utils.Routes as Routes
import Utils.Time exposing (dayMonthYear, timeOfDay)
import Utils.FlashMessages
import List.Extra
import RemoteData exposing (WebData)


view : ActivityList -> Html Msg
view (ActivityList model) =
    div
        []
        [ Ui.submissionInfo "Activity Deleted." model.deletionStatus
        , activityList model.activities
        , loadingStatus model.fetchStatus
        , model.deletionWarning
            |> Maybe.map deletionWarning
            |> Maybe.withDefault (text "")
        ]


spinner =
    div [ class "spinningCircle" ] []


loadMoreBtn =
    div
        [ class "text-center m-t-20" ]
        [ button
            [ class "btn btn-muted btn-sm"
            , onClick (Fetch LoadingMore)
            ]
            [ text "Load More" ]
        ]


loadingStatus : WebData (List a) -> Html Msg
loadingStatus status =
    case status of
        RemoteData.Failure err ->
            Utils.Api.errorMessage err
                |> Utils.FlashMessages.failure

        RemoteData.Loading ->
            spinner

        RemoteData.NotAsked ->
            loadMoreBtn

        RemoteData.Success aList ->
            if List.length aList > 0 then
                loadMoreBtn
            else
                div
                    [ class "text-center text-muted" ]
                    [ text "No more activities to load" ]


activityList : List Activity -> Html Msg
activityList activityList =
    div []
        (activityList
            |> List.Extra.groupWhile (\x y -> sameDay x.date y.date)
            |> List.map formatActivityGroup
        )


formatActivityGroup : List Activity -> Html Msg
formatActivityGroup activityList =
    let
        date =
            List.head activityList
                |> Maybe.map .date
                |> Maybe.map dateMark
                |> Maybe.withDefault (text "")
    in
        div []
            ([ date ]
                ++ (List.map formatActivity activityList)
            )


formatActivity : Activity -> Html Msg
formatActivity activity =
    div [ class "activity-item" ]
        [ categoryIcon activity.category
        , p
            [ class "clearfix" ]
            [ strong [] [ text activity.category ]
            , span [ class "pull-right font-13 text-muted" ] [ text <| timeOfDay activity.date ]
            ]
        , p
            []
            [ text activity.description ]
        , div [ class "clearfix" ]
            [ creatorLink activity
            , activity.project
                |> Maybe.map formatProject
                |> Maybe.map (\proj -> p [] [ proj ])
                |> Maybe.withDefault (text "")
            ]
        , contactList activity.contacts
        , div [ class "clearfix" ]
            [ a
                [ class "fa fa-trash-o pull-right activity-delete text-muted"
                , onClick (WarnDeletion <| Just activity)
                ]
                []
            , tagList activity.tags
            ]
        ]


creatorLink activity =
    a
        [ class "text-muted font-13 pull-right text-muted"
        , href <| Routes.userPage activity.created_by.id
        ]
        [ case activity.created_by.contact of
            Just contact ->
                text contact.name

            Nothing ->
                text activity.created_by.email
        ]


contactList contacts =
    if List.length contacts > 0 then
        p [] (List.map formatContact contacts)
    else
        text ""


tagList tags =
    if List.length tags > 0 then
        p [] (List.map formatTag tags)
    else
        text ""


dateMark : Date -> Html Msg
dateMark date =
    div [ class "activity-item" ]
        [ div
            [ class "activity-item-icon activity-item-icon-time-mark" ]
            []
        , span
            [ class "activity-time-mark" ]
            [ text <| dayMonthYear date ]
        ]


categoryIcon : String -> Html Msg
categoryIcon category =
    if category == "Phone Call" then
        div
            [ class "activity-item-icon activity-item-icon-call" ]
            [ i [ class "fa fa-phone" ] [] ]
    else if category == "Email" then
        div
            [ class "activity-item-icon activity-item-icon-email" ]
            [ i [ class "fa fa-envelope" ] [] ]
    else if category == "Meeting" then
        div
            [ class "activity-item-icon activity-item-icon-meeting" ]
            [ i [ class "md md-group" ] [] ]
    else if category == "Other" then
        div
            [ class "activity-item-icon activity-item-icon-other" ]
            [ i [ class "fa fa-comment-o" ] [] ]
    else
        div
            [ class "activity-item-icon" ]
            [ i [ class "fa fa-question-circle-o" ] [] ]


formatContact : ContactS -> Html Msg
formatContact contact =
    a
        [ class "activity-item-label activity-item-label-contact m-r-5"
        , href <| Routes.contactPage contact.id
        ]
        [ text contact.name ]


formatProject : ProjectS -> Html Msg
formatProject project =
    a
        [ class "activity-item-label activity-item-label-project m-r-5"
        , href <| Routes.projectPage project.id
        ]
        [ text project.name ]


formatTag : ActivityTag -> Html Msg
formatTag { name } =
    span
        [ class "activity-item-label activity-item-label-tag m-r-5" ]
        [ text name ]


sameDay : Date -> Date -> Bool
sameDay x y =
    (Date.year x == Date.year y)
        && (Date.month x == Date.month y)
        && (Date.day x == Date.day y)


deletionWarning activity =
    Ui.modal
        [ Ui.modalHeader "Activity deletion confirmation" (WarnDeletion Nothing)
        , Ui.modalBody
            [ text "Are you sure you want to delete this activity?"
            , formatActivity activity
            ]
        , Ui.modalFooter
            [ button [ class "btn btn-default waves-effect", onClick (WarnDeletion Nothing) ]
                [ text "Cancel" ]
            , button [ class "btn btn-danger waves-effect waves-light", onClick (Delete activity) ]
                [ text "Delete" ]
            ]
        ]
