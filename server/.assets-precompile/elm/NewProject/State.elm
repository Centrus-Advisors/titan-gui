port module NewProject.State exposing (init, update, subscriptions)

import NewProject.Types exposing (..)
import NewProject.Rest exposing (searchUsers, searchCompanies, createProject)
import Common.Types exposing (ProjectType(Standard, Retainer), CompanyS, UserS)
import Utils.SearchBox as SearchBox
import RemoteData
import Date
import Time exposing (Time)
import Utils.Http
import Utils.Time exposing (weekDayDayMonthYear)
import DatePicker exposing (DatePicker)
import Utils.Delay exposing (delay)
import Utils.Routes as Routes
import Navigation


init : { time : Time } -> ( Model, Cmd Msg )
init { time } =
    { modal = Nothing
    , submission = RemoteData.NotAsked
    , todayDate = Date.fromTime time
    }
        ! []


port openModal : (String -> msg) -> Sub msg


subscriptions model =
    openModal (always OpenModal)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Submit ->
            case model.modal of
                Nothing ->
                    model ! []

                Just modal ->
                    { model | submission = RemoteData.Loading }
                        ! [ createProject modal
                                |> Cmd.map SubmissionUpdate
                          ]

        SubmissionUpdate status ->
            let
                newModel =
                    { model | submission = status }
            in
                case status of
                    RemoteData.Success project ->
                        newModel ! [ delay 1000 <| GoToProjectPage project ]

                    _ ->
                        newModel ! []

        GoToProjectPage project ->
            model ! [ Navigation.load <| Routes.projectPage project.id ]

        OpenModal ->
            let
                defaultSettings =
                    DatePicker.defaultSettings

                ( picker, pickerMsg ) =
                    DatePicker.init
                        { defaultSettings
                            | pickedDate = Nothing
                            , inputClassList = [ ( "form-control", True ) ]
                            , dateFormatter = weekDayDayMonthYear
                        }

                newModal =
                    { name = ""
                    , type_ = Standard
                    , startDate = picker
                    , endDate = picker
                    , company = SearchBox.init companySearchBoxConfig Nothing
                    , owner = SearchBox.init userSearchBoxConfig Nothing
                    , budget = ""
                    , description = ""
                    }
            in
                { model
                    | modal = Just newModal
                    , submission = RemoteData.NotAsked
                }
                    ! [ Cmd.map (PStartDate >> Change) pickerMsg
                      , Cmd.map (PEndDate >> Change) pickerMsg
                      ]

        CloseModal ->
            { model | modal = Nothing } ! []

        Change subMsg ->
            case model.modal of
                Nothing ->
                    model ! []

                Just modal ->
                    updateNewProject subMsg modal
                        |> Tuple.mapFirst (\m -> { model | modal = Just m })


updateNewProject : ChangeMsg -> NewProject -> ( NewProject, Cmd Msg )
updateNewProject msg project =
    case msg of
        PName val ->
            { project | name = val } ! []

        PType val ->
            case val of
                "Standard" ->
                    { project | type_ = Standard } ! []

                _ ->
                    { project | type_ = Retainer } ! []

        PStartDate subMsg ->
            DatePicker.update subMsg project.startDate
                |> (\( a, b, c ) -> ( a, b ))
                |> Tuple.mapFirst (\d -> { project | startDate = d })
                |> Tuple.mapSecond (Cmd.map (PStartDate >> Change))

        PEndDate subMsg ->
            DatePicker.update subMsg project.endDate
                |> (\( a, b, c ) -> ( a, b ))
                |> Tuple.mapFirst (\d -> { project | endDate = d })
                |> Tuple.mapSecond (Cmd.map (PEndDate >> Change))

        PCompany subMsg ->
            SearchBox.update subMsg project.company
                |> Tuple.mapFirst (\s -> { project | company = s })
                |> Tuple.mapSecond (Cmd.map (PCompany >> Change))

        POwner subMsg ->
            SearchBox.update subMsg project.owner
                |> Tuple.mapFirst (\s -> { project | owner = s })
                |> Tuple.mapSecond (Cmd.map (POwner >> Change))

        PBudget val ->
            { project | budget = val } ! []

        PDescription val ->
            { project | description = val } ! []


userSearchBoxConfig : SearchBox.Config UserS
userSearchBoxConfig =
    { renderItem = .email
    , renderError = Utils.Http.errorMessage
    , onSearch = searchUsers
    , placeholder = "User email"
    }


companySearchBoxConfig : SearchBox.Config CompanyS
companySearchBoxConfig =
    { renderItem = .name
    , renderError = Utils.Http.errorMessage
    , onSearch = searchCompanies
    , placeholder = "Company name"
    }
