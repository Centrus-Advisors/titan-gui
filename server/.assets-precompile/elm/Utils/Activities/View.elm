module Utils.Activities.View exposing (view)

import Html exposing (Html, div, text, h4, strong, i, a, input, select, option, p, span, button, small, label, textarea)
import Html.Attributes exposing (class, href, style, disabled, selected, type_, name, placeholder, value, rows, title)
import Html.Events exposing (onClick, onInput, on, targetValue)
import Utils.Activities.CommonTypes exposing (..)
import Utils.Activities.Types exposing (..)
import Common.Types exposing (ContactS)
import Utils.Ui as Ui
import Utils.Api
import Utils.Routes as Routes
import Utils.FlashMessages
import Date.Extra
import Date exposing (Date)
import RemoteData exposing (WebData)
import List.Extra
import Json.Decode
import Utils.SearchBox as SearchBox
import DatePicker
import Utils.Activities.ActivityList.Main as ActivityList
import Utils.Activities.Filters.Main as Filters


view : Activities -> Html Msg
view (Activities model) =
    div
        []
        [ titleBar model.sourceID
        , Filters.view model.filters
            |> Html.map FiltersMsg
        , ActivityList.view model.activityList
            |> Html.map ActivityListMsg
        , model.newActivity
            |> Maybe.map (newActivityModal model.newTagSubmission model.newTag model.newActivitySubmission)
            |> Maybe.withDefault (text "")
        ]


titleBar sourceID =
    case sourceID of
        CompanyID _ ->
            div []
                [ h4 []
                    [ strong [] [ text "ACTIVITIES" ] ]
                , p []
                    [ small
                        [ class "font-13 text-muted" ]
                        [ text "Activities from this company's projects" ]
                    ]
                ]

        UserID _ ->
            div []
                [ h4 []
                    [ strong [] [ text "ACTIVITIES" ] ]
                , p []
                    [ small
                        [ class "font-13 text-muted" ]
                        [ text "Activities created by user" ]
                    ]
                ]

        _ ->
            h4
                [ class "clearfix" ]
                [ strong [] [ text "ACTIVITIES" ]
                , a
                    [ class "pull-right text-primary" ]
                    [ i
                        [ class "text-big fa fa-plus"
                        , style [ ( "cursor", "pointer" ) ]
                        , onClick CreateNewActivity
                        ]
                        []
                    ]
                ]



-- We will group activities by day and for each day we will
-- create an element stating the date


newActivityModal : WebData ActivityTag -> Maybe String -> WebData Activity -> EditableActivity -> Html Msg
newActivityModal tagSubmission newTag submission newActivity =
    let
        bodyContent =
            case submission of
                RemoteData.Success _ ->
                    text ""

                _ ->
                    newActivityForm tagSubmission newTag newActivity

        footerContent =
            case submission of
                RemoteData.Success _ ->
                    []

                _ ->
                    [ button [ class "btn btn-default waves-effect", onClick CancelNewActivity ]
                        [ text "Cancel" ]
                    , button [ class "btn btn-primary waves-effect waves-light", onClick NewActivitySubmit ]
                        [ text "Save changes" ]
                    ]
    in
        Ui.modal
            [ Ui.modalHeader "New Activity" CancelNewActivity
            , Ui.modalBody
                [ Ui.submissionInfo "Activity Created." submission
                , bodyContent
                ]
            , Ui.modalFooter
                footerContent
            ]


header : String -> List (Html Msg) -> Html Msg
header txt content =
    div [ class "form-group" ]
        ([ label [] [ text txt ] ]
            ++ content
        )


newActivityForm : WebData ActivityTag -> Maybe String -> EditableActivity -> Html Msg
newActivityForm newTagSubmission newTag newActivity =
    div []
        [ header "Date"
            [ DatePicker.view newActivity.date
                |> Html.map (ChangeDate >> NewActivity)
            ]
        , header "Description"
            [ textarea
                [ class "form-control"
                , style [ ( "min-height", "34px" ), ( "height", "auto" ) ]
                , rows 4
                , value newActivity.description
                , onInput (ChangeDescription >> NewActivity)
                ]
                []
            ]
        , header "Category"
            [ select
                [ class "form-control"
                , on "change" (Json.Decode.map (ChangeCategory >> NewActivity) targetValue)
                ]
                [ option [ selected (newActivity.category == PhoneCall) ] [ text "Phone Call" ]
                , option [ selected (newActivity.category == Meeting) ] [ text "Meeting" ]
                , option [ selected (newActivity.category == Email) ] [ text "Email" ]
                , option [ selected (newActivity.category == Other) ] [ text "Other" ]
                ]
            ]
        , header "Tags"
            [ newTagButton newTag
            , Ui.submissionInfo "Tag Created." newTagSubmission
            , case newTag of
                Nothing ->
                    SearchBox.view newActivity.tagSearch
                        |> Html.map (SearchTag >> NewActivity)

                Just tagTxt ->
                    div
                        []
                        [ input
                            [ type_ "text"
                            , class "form-control"
                            , value tagTxt
                            , placeholder "Type a name for the new tag"
                            , onInput NewTagChange
                            ]
                            []
                        , button
                            [ class "btn btn-default btn-sm m-l-10 m-t-10"
                            , onClick NewTagCancel
                            ]
                            [ text "Cancel" ]
                        , button
                            [ class "btn btn-primary btn-sm m-l-10 m-t-10"
                            , onClick NewTagSubmit
                            ]
                            [ text "Create" ]
                        ]
            , div
                [ class "m-t-10" ]
                -- [ tagList newActivity.tags ]
                (List.map removableTag newActivity.tags)
            ]
        , header "Contacts"
            [ SearchBox.view newActivity.contactSearch
                |> Html.map (SearchContact >> NewActivity)
            , div
                [ class "m-t-10" ]
                (List.map removableContact newActivity.contacts)
            ]
        , header "Project"
            [ small [ class "text-muted" ] [ text " (optional)" ]
            , SearchBox.view newActivity.project
                |> Html.map (SearchProject >> NewActivity)
            ]
        ]


newTagButton : Maybe String -> Html Msg
newTagButton tagTxt =
    case tagTxt of
        Nothing ->
            button
                [ class "btn btn-primary btn-xs m-l-10 m-b-5"
                , onClick NewTagCreate
                ]
                [ text "Create new" ]

        Just _ ->
            text ""


removableContact : ContactS -> Html Msg
removableContact contact =
    span [ class "m-r-5" ]
        [ a
            [ class "activity-item-label activity-item-label-contact"
            , href <| Routes.contactPage contact.id
            ]
            [ text contact.name ]
        , a
            [ class "activity-item-label activity-item-label-contact"
            , href "#"
            , title "Remove contact"
            , onClick (RemoveContact contact |> NewActivity)
            ]
            [ text "x" ]
        ]


removableTag : ActivityTag -> Html Msg
removableTag aTag =
    span [ class "m-r-5" ]
        [ a
            [ class "activity-item-label activity-item-label-tag" ]
            [ text aTag.name ]
        , a
            [ class "activity-item-label activity-item-label-tag"
            , href "#"
            , title "Remove Tag"
            , onClick (RemoveTag aTag |> NewActivity)
            ]
            [ text "x" ]
        ]
