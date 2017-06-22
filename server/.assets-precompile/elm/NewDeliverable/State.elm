module NewDeliverable.State exposing (init, update)

import NewDeliverable.Types exposing (..)
import NewDeliverable.Rest exposing (createDeliverable, searchUsers, searchProjects)
import Common.Types exposing (UserS, DeliverableStatus(Cancelled, Completed, Active), DeliverableType(..))
import Table
import Utils.SearchBox as SearchBox
import DatePicker
import Utils.Http
import Utils.Routes as Routes
import RemoteData
import Utils.Time exposing (weekDayDayMonthYear)
import Task


init : Config msg -> Maybe Project -> ( Model msg, Cmd msg )
init config mProject =
    let
        ( newDeliverable, cmd ) =
            emptyNewDeliverable mProject
    in
        { newDeliverable = newDeliverable
        , submission = RemoteData.NotAsked
        , config = config
        }
            ! [ cmd
                    |> Cmd.map config.onUpdate
              ]


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update msg model =
    case msg of
        Change fieldMsg ->
            updateNewDeliverable fieldMsg model.newDeliverable
                |> Tuple.mapFirst (\e -> { model | newDeliverable = e })
                |> Tuple.mapSecond (Cmd.map model.config.onUpdate)

        Cancel ->
            model ! [ toCmd model.config.onCancel ]

        Create new ->
            ( { model | submission = RemoteData.Loading }
            , createDeliverable new
                |> Cmd.map CreationUpdate
                |> Cmd.map model.config.onUpdate
            )

        CreationUpdate status ->
            case status of
                RemoteData.Success deliverable ->
                    ( { model | submission = status }, toCmd (model.config.onCreated deliverable) )

                _ ->
                    ( { model | submission = status }, Cmd.none )


updateNewDeliverable : FieldMsg -> NewDeliverable -> ( NewDeliverable, Cmd Msg )
updateNewDeliverable msg d =
    case msg of
        Title val ->
            ( { d | title = val }, Cmd.none )

        Milestone val ->
            ( { d | milestone = val }, Cmd.none )

        DType val ->
            ( { d | type_ = val }, Cmd.none )

        StartDate pickerMsg ->
            let
                ( newPicker, newPickerMsg, _ ) =
                    DatePicker.update pickerMsg d.startDate
            in
                ( { d | startDate = newPicker }
                , Cmd.map (Change << StartDate) newPickerMsg
                )

        Deadline pickerMsg ->
            let
                ( newPicker, newPickerMsg, _ ) =
                    DatePicker.update pickerMsg d.deadline
            in
                ( { d | deadline = newPicker }
                , Cmd.map (Change << Deadline) newPickerMsg
                )

        Owner searchMsg ->
            SearchBox.update searchMsg d.owner
                |> Tuple.mapSecond (Cmd.map (Owner >> Change))
                |> Tuple.mapFirst (\c -> { d | owner = c })

        DProject searchMsg ->
            SearchBox.update searchMsg d.project
                |> Tuple.mapSecond (Cmd.map (DProject >> Change))
                |> Tuple.mapFirst (\c -> { d | project = c })


searchBoxConfigUser : SearchBox.Config UserS
searchBoxConfigUser =
    { renderItem = .email
    , renderError = Utils.Http.errorMessage
    , onSearch = searchUsers
    , placeholder = "User email"
    }


searchBoxConfigProject : SearchBox.Config Project
searchBoxConfigProject =
    { renderItem = .name
    , renderError = Utils.Http.errorMessage
    , onSearch = searchProjects
    , placeholder = "Project name"
    }


emptyNewDeliverable : Maybe Project -> ( NewDeliverable, Cmd Msg )
emptyNewDeliverable mProject =
    let
        defaultSettings =
            DatePicker.defaultSettings

        ( startDatePicker, startDatePickerMsg ) =
            DatePicker.init
                { defaultSettings
                    | pickedDate = Nothing
                    , inputClassList = [ ( "form-control", True ) ]
                    , dateFormatter = weekDayDayMonthYear
                }

        ( deadlinePicker, deadlinePickerMsg ) =
            DatePicker.init
                { defaultSettings
                    | pickedDate = Nothing
                    , inputClassList = [ ( "form-control", True ) ]
                    , dateFormatter = weekDayDayMonthYear
                }
    in
        { project = SearchBox.init searchBoxConfigProject mProject
        , title = ""
        , milestone = ""
        , type_ = DOther
        , startDate = startDatePicker
        , deadline = deadlinePicker
        , owner = SearchBox.init searchBoxConfigUser Nothing
        }
            ! [ Cmd.map (StartDate >> Change) startDatePickerMsg
              , Cmd.map (Deadline >> Change) deadlinePickerMsg
              ]


toCmd a =
    Task.perform (always a) (Task.succeed ())
