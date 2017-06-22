module Deliverable.State exposing (init, update)

import Deliverable.Types exposing (..)
import Deliverable.Rest
    exposing
        ( deliverableDecoder
        , stringToDelivStatus
        , deliverableContent
        , searchUsers
        , fetchDeliverable
        , fetchDeliverableCost
        , deleteDeliverable
        , saveDeliverable
        )
import Common.Types exposing (Deliverable, Cost, UserS)
import Deliverable.TimeTracking.Main as TimeTracking
import RemoteData
import Time exposing (Time)
import Date exposing (Date)
import Date.Extra
import Utils.Api as Api
import Utils.Http
import DatePicker
import Utils.SearchBox as SearchBox
import Utils.Time as TimeUtils exposing (weekDayDayMonthYear)
import List.Extra


init : { time : Time, deliverableID : String } -> ( Model, Cmd Msg )
init { time, deliverableID } =
    let
        todayDate =
            Date.fromTime time

        ( timeTracking, timeTrackingMsg ) =
            TimeTracking.init todayDate deliverableID
                |> Tuple.mapSecond (Cmd.map TimeTrackingMsg)

        model =
            { timeTracking = timeTracking
            , deliverable = RemoteData.Loading
            , cost = RemoteData.Loading
            , todayDate = todayDate
            , submission = RemoteData.NotAsked
            , deletion = RemoteData.NotAsked
            , editable = Nothing
            }
    in
        update (Reload deliverableID) model
            |> Tuple.mapSecond (\c -> Cmd.batch [ c, timeTrackingMsg ])


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeTrackingMsg subMsg ->
            TimeTracking.update subMsg model.timeTracking
                |> Tuple.mapFirst (\v -> { model | timeTracking = v })
                |> Tuple.mapSecond (Cmd.map TimeTrackingMsg)

        Reload deliverableID ->
            { model
                | deliverable = RemoteData.Loading
                , cost = RemoteData.Loading
                , editable = Nothing
            }
                ! [ fetchDeliverable deliverableID
                        |> Cmd.map FetchDeliverableUpdate
                  , fetchDeliverableCost deliverableID
                        |> Cmd.map FetchCostUpdate
                  ]

        FetchDeliverableUpdate status ->
            { model | deliverable = status } ! []

        FetchCostUpdate status ->
            { model | cost = status } ! []

        SetEditing enable ->
            case ( enable, model.deliverable ) of
                ( True, RemoteData.Success deliverableType ) ->
                    case toEditable deliverableType of
                        Nothing ->
                            model ! []

                        Just ( editable, cmd ) ->
                            { model | editable = Just editable }
                                ! [ cmd ]

                _ ->
                    { model | editable = Nothing }
                        ! []

        SubmissionUpdate status ->
            case status of
                RemoteData.Success _ ->
                    { model
                        | submission = status
                        , editable = Nothing
                        , deliverable = status
                    }
                        ! []

                _ ->
                    { model | submission = status }
                        ! []

        Submit editable ->
            { model | submission = RemoteData.Loading }
                ! [ saveDeliverable editable
                        |> Cmd.map SubmissionUpdate
                  ]

        Delete editable ->
            { model
                | deletion = RemoteData.Loading
            }
                ! [ deleteDeliverable editable
                        |> Cmd.map DeletionUpdate
                  ]

        DeletionUpdate status ->
            {- TODO: Redirect to project page after a countdown -}
            { model | deletion = status } ! []

        Change fieldMsg ->
            case model.editable of
                Nothing ->
                    model ! []

                Just editable ->
                    updateField fieldMsg editable
                        |> Tuple.mapFirst (\e -> { model | editable = Just e })


updateField : FieldMsg -> EditableDeliverable -> ( EditableDeliverable, Cmd Msg )
updateField fieldMsg deliv =
    case fieldMsg of
        Title val ->
            { deliv | title = val } ! []

        Type val ->
            { deliv | type_ = val } ! []

        Milestone val ->
            { deliv | milestone = val } ! []

        Description val ->
            { deliv | description = val } ! []

        Status val ->
            case stringToDelivStatus val of
                Just status ->
                    { deliv | status = status } ! []

                Nothing ->
                    deliv ! []

        HourlyRate val ->
            { deliv | hourly_rate = val } ! []

        StartDate subMsg ->
            let
                ( newPicker, newPickerMsg, _ ) =
                    DatePicker.update subMsg deliv.startDate
            in
                ( { deliv | startDate = newPicker }
                , Cmd.map (StartDate >> Change) newPickerMsg
                )

        Deadline subMsg ->
            let
                ( newPicker, newPickerMsg, _ ) =
                    DatePicker.update subMsg deliv.deadline
            in
                ( { deliv | deadline = newPicker }
                , Cmd.map (Deadline >> Change) newPickerMsg
                )

        Owner searchBoxMsg ->
            SearchBox.update searchBoxMsg deliv.owner
                |> Tuple.mapFirst (\owner -> { deliv | owner = owner })
                |> Tuple.mapSecond (Cmd.map (Owner >> Change))

        Assignees subMsg ->
            updateAssignees subMsg deliv.assignees
                |> Tuple.mapFirst (\a -> { deliv | assignees = a })

        ToggleDeletionModal val ->
            { deliv | deletionModalShowing = val } ! []


updateAssignees : AssigneesMsg -> List (SearchBox.SearchBox UserS) -> ( List (SearchBox.SearchBox UserS), Cmd Msg )
updateAssignees msg assignees =
    case msg of
        AAdd ->
            (assignees ++ [ SearchBox.init searchBoxConfig Nothing ]) ! []

        ARemove idx ->
            List.Extra.removeAt idx assignees ! []

        ASearch idx searchMsg ->
            let
                updateInList a =
                    List.Extra.updateIfIndex
                        ((==) idx)
                        (always a)
                        assignees

                updateBox b =
                    SearchBox.update searchMsg b
                        |> Tuple.mapFirst updateInList
                        |> Tuple.mapSecond (Cmd.map (ASearch idx >> Assignees >> Change))
            in
                List.Extra.getAt idx assignees
                    |> Maybe.map updateBox
                    |> Maybe.withDefault ( assignees, Cmd.none )


searchBoxConfig =
    { renderItem = .email
    , renderError = Utils.Http.errorMessage
    , onSearch = searchUsers
    , placeholder = "User email"
    }


toEditable : DeliverableEdit -> Maybe ( EditableDeliverable, Cmd Msg )
toEditable delivType =
    case delivType of
        NonEditable _ ->
            Nothing

        Editable deliv ->
            let
                defaultSettings =
                    DatePicker.defaultSettings

                ( startDatePicker, startDatePickerMsg ) =
                    DatePicker.init
                        { defaultSettings
                            | pickedDate = Just deliv.startDate
                            , inputClassList = [ ( "form-control", True ) ]
                            , dateFormatter = weekDayDayMonthYear
                        }

                ( deadlinePicker, deadlinePickerMsg ) =
                    DatePicker.init
                        { defaultSettings
                            | pickedDate = deliv.deadline
                            , inputClassList = [ ( "form-control", True ) ]
                            , dateFormatter = weekDayDayMonthYear
                        }

                owner =
                    SearchBox.init searchBoxConfig (Just deliv.owner)

                assignees =
                    deliv.assignees
                        |> List.map Just
                        |> List.map (SearchBox.init searchBoxConfig)

                editable =
                    { id = deliv.id
                    , title = deliv.title
                    , type_ = deliv.type_
                    , milestone = deliv.milestone
                    , description = deliv.description
                    , status = deliv.status
                    , hourly_rate = deliv.hourly_rate
                    , startDate = startDatePicker
                    , deadline = deadlinePicker
                    , owner = owner
                    , assignees = assignees
                    , project = deliv.project
                    , created_by = deliv.created_by
                    , deletionModalShowing = False
                    }

                cmd =
                    Cmd.batch
                        [ Cmd.map (StartDate >> Change) startDatePickerMsg
                        , Cmd.map (Deadline >> Change) deadlinePickerMsg
                        ]
            in
                Just ( editable, cmd )
