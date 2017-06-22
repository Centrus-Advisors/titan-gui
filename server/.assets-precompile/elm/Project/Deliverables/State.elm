module Project.Deliverables.State exposing (init, update)

import Project.Deliverables.Types exposing (..)
import Project.Deliverables.Rest exposing (fetchDeliverables)
import Common.Types exposing (ProjectS)
import Table
import NewDeliverable.Main as NewDeliverable
import Task
import RemoteData


init : String -> ( Model, Cmd Msg )
init projectId =
    { deliverables = RemoteData.NotAsked
    , tableState = Table.initialSort "Deadline"
    , new = Nothing
    }
        ! [ toCmd <| FetchDeliverables projectId ]


update : Maybe ProjectS -> Msg -> Model -> ( Model, Cmd Msg )
update mProject msg model =
    case msg of
        FetchDeliverables projectId ->
            { model | deliverables = RemoteData.Loading }
                ! [ fetchDeliverables projectId
                        |> Cmd.map FetchDeliverablesUpdate
                  ]

        FetchDeliverablesUpdate status ->
            { model | deliverables = status } ! []

        SetTableState newState ->
            ( { model | tableState = newState }
            , Cmd.none
            )

        ShowNewDeliverable yes ->
            if yes then
                NewDeliverable.init newDeliverableConfig mProject
                    |> Tuple.mapFirst (\d -> { model | new = Just d })
            else
                ( { model | new = Nothing }, Cmd.none )

        NewDeliverableUpdate subMsg ->
            case model.new of
                Nothing ->
                    model ! []

                Just newDeliverable ->
                    NewDeliverable.update subMsg newDeliverable
                        |> Tuple.mapFirst (\d -> { model | new = Just d })


newDeliverableConfig : NewDeliverable.Config Msg
newDeliverableConfig =
    { onUpdate = NewDeliverableUpdate
    , onCancel = ShowNewDeliverable False
    , onCreated = (\d -> FetchDeliverables d.project.id)
    }


toCmd a =
    Task.perform (always a) (Task.succeed ())
