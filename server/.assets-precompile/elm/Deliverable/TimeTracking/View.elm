module Deliverable.TimeTracking.View exposing (view)

import Deliverable.TimeTracking.Types exposing (..)
import RemoteData exposing (WebData)
import Html exposing (Html, text, label, span, div, tr, a, p, i, h4, strong, button, input)
import Html.Attributes exposing (class, href, style, href, type_, value, disabled)
import Html.Events exposing (onClick, onInput)
import Utils.FlashMessages as FlashMessages
import Utils.Ui as Ui
import Utils.Routes as Routes
import Date.Extra
import Utils.Api as Api
import Utils.FlashMessages
import Date exposing (Date)
import Date.Extra
import Table exposing (defaultCustomizations)
import DatePicker
import TimePicker
import Time exposing (Time)
import List.Extra
import Utils.Time
    exposing
        ( dayMonthYear
        , timeOfDay
        , intervalDescription
        , timeDifference
        )
import Common.View exposing (timeRecordingView)


view : Model -> Html Msg
view model =
    div
        []
        [ titleBar
        , recordingList model.recordings
        , loadingStatus model.fetchRecordings
        , model.newRecording
            |> Maybe.map (newRecordingModal model.submission)
            |> Maybe.withDefault (text "")
        , model.deletionWarning
            |> Maybe.map (warningModal model.deletionStatus)
            |> Maybe.withDefault (text "")
        ]


titleBar =
    h4
        [ class "clearfix" ]
        [ strong [] [ text "TRACKED TIME" ]
        , a
            [ class "pull-right text-primary" ]
            [ i
                [ class "text-big fa fa-plus"
                , style [ ( "cursor", "pointer" ) ]
                , onClick (ToggleNewRecording True)
                ]
                []
            ]
        ]


recordingList : List TimeRecording -> Html Msg
recordingList recordingList =
    div []
        (recordingList
            |> List.Extra.groupWhile (\x y -> sameDay x.startDate y.startDate)
            |> List.map formatRecordingGroup
        )


formatRecordingGroup : List TimeRecording -> Html Msg
formatRecordingGroup recordingList =
    let
        date =
            List.head recordingList
                |> Maybe.map .startDate
                |> Maybe.map dateMark
                |> Maybe.withDefault (text "")
    in
        div []
            ([ date ]
                ++ (List.map recordingWithDeletion recordingList)
            )


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


recordingWithDeletion : TimeRecording -> Html Msg
recordingWithDeletion recording =
    timeRecordingView recording
        [ div [ class "clearfix" ]
            [ a
                [ class "fa fa-trash-o pull-right activity-delete text-muted m-t-10"
                , onClick (WarnDeletion <| Just recording)
                ]
                []
            ]
        ]


timeline recording =
    let
        oneDay =
            floor <| Time.hour * 24

        percentOfDay =
            recording.duration
                |> (\x -> (x * 100) // oneDay)
                |> max 1

        uncappedOffsetLeft =
            recording.startDate
                |> Date.Extra.floor Date.Extra.Day
                |> (\x -> Date.Extra.diff Date.Extra.Millisecond x recording.startDate)
                |> (\x -> (x * 100) // oneDay)

        offsetLeft =
            --  Here we are enforcing this: uncappedOffsetLeft + percentOfDay <= 100
            min
                (100 - percentOfDay)
                uncappedOffsetLeft
    in
        div
            [ class "timeline"
            , style
                [ ( "height", "3px" )
                , ( "position", "relative" )
                , ( "background-color", "#f0f0f0" )
                , ( "overflow", "hidden" )
                ]
            ]
            [ div
                [ class "time-recorded"
                , style
                    [ ( "position", "absolute" )
                    , ( "top", "0" )
                    , ( "height", "3px" )
                    , ( "background-color", "#f05a28" )
                    , ( "width", (toString percentOfDay) ++ "%" )
                    , ( "left", (toString offsetLeft) ++ "%" )
                    ]
                ]
                []
            ]


loadingStatus : WebData (List a) -> Html Msg
loadingStatus status =
    case status of
        RemoteData.Failure err ->
            Api.errorMessage err
                |> Utils.FlashMessages.failure

        RemoteData.Loading ->
            Ui.spinner

        RemoteData.NotAsked ->
            text ""

        RemoteData.Success aList ->
            if List.length aList > 0 then
                text ""
            else
                div
                    [ class "text-center text-muted" ]
                    [ text "No more activities to load" ]


userName : User -> String
userName user =
    user
        |> .contact
        |> Maybe.map .name
        |> Maybe.withDefault user.email


userLink : User -> Html Msg
userLink user =
    a
        [ href <| Routes.userPage user.id ]
        [ text <| userName user ]


title : String -> List (Html Msg) -> Html Msg
title txt content =
    div [ class "form-group" ]
        ([ label [] [ text txt ] ]
            ++ content
        )


dateField : Date -> Maybe TimePicker.Model -> (TimePicker.Msg -> Msg) -> (Bool -> Msg) -> Html Msg
dateField date mPicker pickerMsg togglePicker =
    div
        []
        [ p
            [ class "form-control"
            , style [ ( "min-height", "34px" ), ( "height", "auto" ) ]
            , onClick (togglePicker True)
            ]
            [ text <| timeOfDay date ]
        , case mPicker of
            Just picker ->
                timePickerPopup picker pickerMsg togglePicker

            Nothing ->
                text ""
        ]


timePickerPopup picker pickerMsg togglePicker =
    div
        [ style
            [ ( "position", "absolute" )
            , ( "margin", "auto" )
            , ( "left", "0" )
            , ( "right", "0" )
            , ( "z-index", "5" )
            ]
        ]
        [ i
            [ class "fa fa-times"
            , onClick <| togglePicker False
            , style
                [ ( "position", "absolute" )
                , ( "right", "25px" )
                , ( "top", "10px" )
                , ( "color", "white" )
                , ( "font-size", "18px" )
                , ( "cursor", "pointer" )
                ]
            ]
            []
        , TimePicker.view picker
            |> Html.map pickerMsg
        ]


newRecordingModal : WebData TimeRecording -> NewTimeRecording -> Html Msg
newRecordingModal submission newRecording =
    case submission of
        RemoteData.Success _ ->
            successModal "New Time Recording"
                (ToggleNewRecording False)
                [ Ui.submissionInfo "Time Recording created successfully" submission
                ]

        _ ->
            Ui.modal
                [ Ui.modalHeader "New Time Recording" (ToggleNewRecording False)
                , Ui.modalBody
                    [ Ui.submissionInfo "Time Recording created successfully" submission
                    , newRecordingForm newRecording
                    ]
                , Ui.modalFooter
                    [ button
                        [ class "btn btn-default waves-effect"
                        , onClick (ToggleNewRecording False)
                        ]
                        [ text "Cancel" ]
                    , button
                        [ class "btn btn-primary waves-effect waves-light"
                        , onClick Submit
                        ]
                        [ text "Create" ]
                    ]
                ]


newRecordingForm newRecording =
    div
        []
        [ div
            [ class "row" ]
            [ div
                [ class "col-md-12" ]
                [ title "Date"
                    [ DatePicker.view newRecording.startDate
                        |> Html.map (StartDate >> NewRecording)
                    ]
                ]
            ]
        , div
            [ class "row" ]
            [ div
                [ class "col-md-6" ]
                [ title "From Time"
                    [ dateField
                        newRecording.fromTime
                        newRecording.fromTimePicker
                        (FromTimePicker >> NewRecording)
                        (ToggleFromTimePicker >> NewRecording)
                    ]
                ]
            , div
                [ class "col-md-6" ]
                [ title "To Time"
                    [ dateField
                        newRecording.toTime
                        newRecording.toTimePicker
                        (ToTimePicker >> NewRecording)
                        (ToggleToTimePicker >> NewRecording)
                    ]
                ]
            ]
        , div
            [ class "row" ]
            [ div
                [ class "col-md-6" ]
                [ title "Duration:"
                    [ strong
                        []
                        [ input
                            [ class "form-control"
                            , disabled True
                            , value <| intervalDescription <| timeDifference newRecording.fromTime newRecording.toTime
                            ]
                            []
                        ]
                    ]
                ]
            ]
        ]


sameDay : Date -> Date -> Bool
sameDay x y =
    (Date.year x == Date.year y)
        && (Date.month x == Date.month y)
        && (Date.day x == Date.day y)


costDescription : Float -> String
costDescription cost =
    cost
        |> (*) 100
        |> floor
        |> toString
        |> String.padLeft 3 '0'
        |> (\x -> (String.dropRight 2 x) ++ "." ++ (String.right 2 x))
        |> (++) "£"


warningModal : WebData () -> TimeRecording -> Html Msg
warningModal submission recording =
    case submission of
        RemoteData.Success _ ->
            successModal "Time Recording deletion confirmation"
                (WarnDeletion Nothing)
                [ Ui.submissionInfo "Time recording deleted successfully" submission
                ]

        _ ->
            Ui.modal
                [ Ui.modalHeader "Time Recording deletion confirmation" (WarnDeletion Nothing)
                , Ui.modalBody
                    [ Ui.submissionInfo "Time recording deleted successfully" submission
                    , p [] [ text "Are you sure you want to permanently delete this time recording?" ]
                    , p [] [ text " It's cost will no longer be counted towards the deliverable or project." ]
                    , timeRecordingView recording []
                    ]
                , Ui.modalFooter
                    [ button [ class "btn btn-default waves-effect", onClick (WarnDeletion Nothing) ]
                        [ text "Cancel" ]
                    , button [ class "btn btn-danger waves-effect waves-light", onClick (Delete recording) ]
                        [ text "Delete" ]
                    ]
                ]


successModal title closeMsg content =
    Ui.modal
        [ Ui.modalHeader title closeMsg
        , Ui.modalBody content
        , Ui.modalFooter
            [ button [ class "btn btn-default waves-effect", onClick closeMsg ]
                [ text "Done" ]
            ]
        ]
