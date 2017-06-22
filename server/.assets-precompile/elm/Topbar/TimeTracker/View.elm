module Topbar.TimeTracker.View exposing (root)

import Topbar.TimeTracker.Types exposing (..)
import Html exposing (Html, text, div, button, span, i, a, p, label, strong)
import Html.Attributes exposing (class, disabled, style, target, href)
import Html.Events exposing (onClick)
import Date exposing (Date)
import Date.Extra
import Time exposing (Time)
import RemoteData
import Utils.Ui as Ui
import Utils.SearchBox as SearchBox
import Utils.Time
import Utils.Routes as Routes
import Common.View
import NewDeliverable.Main as NewDeliverable


root : Model -> Html Msg
root model =
    div
        [ class "topbar-timetracker" ]
        [ case model.status of
            RemoteData.Success status ->
                counter model.currentTime status

            RemoteData.Failure err ->
                text err

            _ ->
                div
                    [ class "topbar-timetracker-counter" ]
                    [ i [ class "fa fa-spinner fa-pulse fa-fw" ] []
                    ]
        , model.creationModal
            |> Maybe.map newRecordingModal
            |> Maybe.withDefault (text "")
        , model.creationModal
            |> Maybe.andThen .newDeliverable
            |> Maybe.map (NewDeliverable.view >> Html.map (NewDeliverableMsg >> SubMsgModal))
            |> Maybe.withDefault (text "")
        ]


counter currentTime status =
    div []
        [ div
            [ class "topbar-timetracker-counter" ]
            [ text <| statusToCounter currentTime status
            , recordingStatusIcon status
            ]
        , toggleButton currentTime status
        ]


toggleButton : Time -> TrackingStatus -> Html Msg
toggleButton currentTime status =
    case status of
        Recording _ ->
            button
                [ class "btn btn-info btn-sm"
                , onClick <| CreateRecording status
                ]
                [ text "Stop" ]

        NotRecording ->
            button
                [ class "btn btn-default btn-sm"
                , onClick <| SendStatus (Recording (Date.fromTime currentTime))
                ]
                [ text "Track" ]


recordingStatusIcon status =
    span
        [ class <|
            case status of
                Recording _ ->
                    "text-danger"

                NotRecording ->
                    "text-info"
        ]
        [ i [ class "fa fa-circle font-13 m-l-5" ] []
        ]


newRecordingModal : CreationModal -> Html Msg
newRecordingModal modal =
    case modal.submission of
        RemoteData.Success timeRecording ->
            Ui.modal
                [ Ui.modalHeader "New Time Recording" CancelCreation
                , Ui.modalBody
                    [ Ui.submissionInfo "Time recording created successfully" modal.submission
                    , div
                        [ style [ ( "line-height", "normal" ) ] ]
                        [ timeRecordingView timeRecording ]
                    ]
                , Ui.modalFooter
                    [ button [ class "btn btn-default waves-effect", onClick CancelCreation ]
                        [ text "Done" ]
                    ]
                ]

        _ ->
            let
                mDeliverable =
                    SearchBox.getChosen modal.deliverable

                canBeSaved =
                    mDeliverable
                        |> Maybe.map (always True)
                        |> Maybe.withDefault False

                onSaveMsg =
                    mDeliverable
                        |> Maybe.map (Submit >> SubMsgModal)
                        |> Maybe.withDefault DoNothing
            in
                Ui.modal
                    [ Ui.modalHeader "New Time Recording" (CancelCreation)
                    , Ui.modalBody
                        [ Ui.submissionInfo "Time Recording created successfully" modal.submission
                        , title "Duration"
                            [ textField
                                [ Utils.Time.timeDifference modal.fromDate modal.toDate
                                    |> Utils.Time.intervalDescription
                                    |> text
                                ]
                            ]
                        , div
                            [ class "row" ]
                            [ div
                                [ class "col-md-6" ]
                                [ title "From"
                                    [ textField
                                        [ text <| dateDescription modal.fromDate ]
                                    ]
                                ]
                            , div [ class "col-md-6" ]
                                [ title "To"
                                    [ textField
                                        [ text <| dateDescription modal.toDate ]
                                    ]
                                ]
                            ]
                        , button
                            [ onClick <| SubMsgModal CreateNewDeliverable
                            , class "btn btn-xs btn-primary"
                            , style [ ( "float", "right" ) ]
                            ]
                            [ text "New Deliverable" ]
                        , title "Deliverable"
                            [ SearchBox.view modal.deliverable
                                |> Html.map (SearchBoxMsg >> SubMsgModal)
                            ]
                        , mDeliverable
                            |> Maybe.map deliverableDetails
                            |> Maybe.withDefault (text "")
                        ]
                    , Ui.modalFooter
                        [ button
                            [ class "btn btn-default waves-effect"
                            , onClick CancelCreation
                            ]
                            [ text "Cancel" ]
                        , button
                            [ class "btn btn-primary waves-effect waves-light"
                            , onClick onSaveMsg
                            , disabled <| not canBeSaved
                            ]
                            [ text "Create" ]
                        ]
                    ]


dateDescription date =
    Utils.Time.weekDayDayMonthYear date
        ++ " "
        ++ Utils.Time.timeOfDay date


deliverableDetails deliverable =
    div
        [ class "row" ]
        [ div
            [ class "col-md-6" ]
            [ title "Project"
                [ textField
                    [ a
                        [ href <| Routes.projectPage deliverable.project.id
                        , target "blank"
                        ]
                        [ text deliverable.project.name ]
                    ]
                ]
            ]
        , div [ class "col-md-6" ]
            [ title "Owner"
                [ textField
                    [ a
                        [ href <| Routes.userPage deliverable.owner.id
                        , target "blank"
                        ]
                        [ text deliverable.owner.email ]
                    ]
                ]
            ]
        ]


timeRecordingView : TimeRecording -> Html Msg
timeRecordingView timeRecording =
    div
        []
        [ div
            [ class "row" ]
            [ div
                [ class "col-md-6" ]
                [ title "Project"
                    [ textField
                        [ a
                            [ href <| Routes.projectPage timeRecording.project.id
                            , target "blank"
                            ]
                            [ text timeRecording.project.name ]
                        ]
                    ]
                ]
            , div [ class "col-md-6" ]
                [ title "Deliverable"
                    [ textField
                        [ a
                            [ href <| Routes.deliverablePage timeRecording.deliverable.id
                            , target "blank"
                            ]
                            [ text timeRecording.deliverable.title ]
                        ]
                    ]
                ]
            ]
        , Common.View.timeRecordingView timeRecording []
        ]


title : String -> List (Html Msg) -> Html Msg
title txt content =
    div [ class "form-group" ]
        ([ label [] [ text txt ] ]
            ++ content
        )


textField : List (Html Msg) -> Html Msg
textField content =
    p
        [ class "form-control"
        , style [ ( "min-height", "34px" ), ( "height", "auto" ) ]
        ]
        content


statusToCounter : Time -> TrackingStatus -> String
statusToCounter currentTime status =
    case status of
        NotRecording ->
            Utils.Time.intervalWatchNotation 0

        Recording date ->
            Utils.Time.intervalWatchNotation (currentTime - Date.toTime date)
