module Deliverable.TimeTracking.State exposing (init, update)

import Deliverable.TimeTracking.Types exposing (..)
import Deliverable.TimeTracking.Rest exposing (fetchTimeRecordings, createNewTimeRecording, deleteTimeRecording)
import RemoteData exposing (WebData)
import Table
import DatePicker
import Date exposing (Date)
import Date.Extra
import TimePicker
import Utils.Time as TimeUtils exposing (weekDayDayMonthYear)


init : Date -> ID -> ( Model, Cmd Msg )
init todayDate deliverableID =
    update Reload
        { todayDate = todayDate
        , deliverable = deliverableID
        , recordings = []
        , fetchRecordings = RemoteData.Loading
        , tableState = Table.initialSort "Date"
        , newRecording = Nothing
        , submission = RemoteData.NotAsked
        , deletionStatus = RemoteData.NotAsked
        , deletionWarning = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Reload ->
            ( model
            , fetchTimeRecordings model.deliverable
                |> Cmd.map FetchUpdate
            )

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        FetchUpdate status ->
            case status of
                RemoteData.Success newRecordings ->
                    { model
                        | fetchRecordings = status
                        , recordings = newRecordings
                    }
                        ! []

                _ ->
                    { model | fetchRecordings = status } ! []

        NewRecording subMsg ->
            case model.newRecording of
                Nothing ->
                    model ! []

                Just newRecording ->
                    recordingUpdate model.todayDate subMsg newRecording
                        |> Tuple.mapFirst (\v -> { model | newRecording = Just v })

        ToggleNewRecording create ->
            if create then
                let
                    newRec =
                        initNewTimeRecording model.todayDate model.deliverable
                in
                    { model | newRecording = Just newRec } ! []
            else
                { model
                    | newRecording = Nothing
                    , submission = RemoteData.NotAsked
                }
                    ! []

        Submit ->
            case model.newRecording of
                Nothing ->
                    model ! []

                Just newRecording ->
                    { model | submission = RemoteData.Loading }
                        ! [ createNewTimeRecording newRecording
                                |> Cmd.map SubmissionUpdate
                          ]

        SubmissionUpdate status ->
            case status of
                RemoteData.Success _ ->
                    update Reload { model | submission = status }

                _ ->
                    { model | submission = status } ! []

        WarnDeletion mRecording ->
            case mRecording of
                Just _ ->
                    { model | deletionWarning = mRecording } ! []

                Nothing ->
                    { model
                        | deletionWarning = mRecording
                        , deletionStatus = RemoteData.NotAsked
                    }
                        ! []

        Delete recording ->
            ( { model
                | deletionStatus = RemoteData.Loading
              }
            , deleteTimeRecording recording
                |> Cmd.map DeletionUpdate
            )

        DeletionUpdate status ->
            case status of
                RemoteData.Success _ ->
                    update Reload
                        { model
                            | deletionStatus = status
                        }

                _ ->
                    { model | deletionStatus = status } ! []


recordingUpdate : Date -> NewRecordingMsg -> NewTimeRecording -> ( NewTimeRecording, Cmd Msg )
recordingUpdate todayDate msg newRecording =
    case msg of
        StartDate dateMsg ->
            let
                ( newPicker, newPickerMsg, _ ) =
                    DatePicker.update dateMsg newRecording.startDate
            in
                ( { newRecording | startDate = newPicker }
                , Cmd.map (NewRecording << StartDate) newPickerMsg
                )

        FromTimePicker subMsg ->
            newRecording.fromTimePicker
                |> Maybe.map (TimePicker.update subMsg)
                |> Maybe.map
                    (\v ->
                        { newRecording
                            | fromTimePicker = Just v
                            , fromTime = TimePicker.selectedTime v
                        }
                    )
                |> Maybe.withDefault newRecording
                |> \v -> v ! []

        ToggleFromTimePicker toggleOn ->
            if toggleOn then
                let
                    picker =
                        TimePicker.init
                            { is24Hours = True
                            , mainColor = "#012639"
                            }
                            todayDate
                in
                    { newRecording | fromTimePicker = Just picker } ! []
            else
                { newRecording | fromTimePicker = Nothing } ! []

        ToTimePicker subMsg ->
            newRecording.toTimePicker
                |> Maybe.map (TimePicker.update subMsg)
                |> Maybe.map
                    (\v ->
                        { newRecording
                            | toTimePicker = Just v
                            , toTime = TimePicker.selectedTime v
                        }
                    )
                |> Maybe.withDefault newRecording
                |> \v -> v ! []

        ToggleToTimePicker toggleOn ->
            if toggleOn then
                let
                    picker =
                        TimePicker.init
                            { is24Hours = True
                            , mainColor = "#012639"
                            }
                            todayDate
                in
                    { newRecording | toTimePicker = Just picker } ! []
            else
                { newRecording | toTimePicker = Nothing } ! []


initNewTimeRecording todayDate deliverableID =
    let
        defaultSettings =
            DatePicker.defaultSettings

        ( startDatePicker, _ ) =
            DatePicker.init
                { defaultSettings
                    | pickedDate = Just todayDate
                    , inputClassList = [ ( "form-control", True ) ]
                    , dateFormatter = weekDayDayMonthYear
                }
    in
        { deliverable = deliverableID
        , startDate = startDatePicker
        , fromTime = todayDate
        , toTime = todayDate
        , fromTimePicker = Nothing
        , toTimePicker = Nothing
        }
