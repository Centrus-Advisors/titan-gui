module Project.State exposing (init, update, subscriptions)

import Project.Types exposing (..)
import Project.Details.Details as Details
import Common.Types exposing (Project, ProjectS, ProjectContact)
import RemoteData
import Utils.Http
import Utils.Routes as Routes
import Task
import Project.Common.Rest exposing (tupleProjectDecoder)
import Project.Deliverables.Deliverables as Deliverables
import Json.Decode
import Task
import Time exposing (Time)
import Date exposing (Date)
import Utils.Activities.Main as Activities


init : { time : Time, projectID : String } -> ( Model, Cmd Msg )
init { time, projectID } =
    let
        ( activities, activitiesMsg ) =
            Activities.initFromProject time projectID

        ( details, detailsMsg ) =
            Details.init projectID

        ( deliverables, deliverablesMsg ) =
            Deliverables.init projectID
    in
        ( { projectID = projectID
          , details = details
          , deliverables = deliverables
          , activities = activities
          , tab = DetailsTab
          , todayDate = Date.fromTime time
          }
        , Cmd.batch
            [ deliverablesMsg |> Cmd.map DeliverablesMsg
            , activitiesMsg |> Cmd.map ActivitiesMsg
            , detailsMsg |> Cmd.map DetailsMsg
            ]
        )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeTab tabName ->
            ( { model | tab = tabName }
            , Cmd.none
            )

        DetailsMsg dMsg ->
            Details.update dMsg model.details
                |> Tuple.mapFirst (\d -> { model | details = d })
                |> Tuple.mapSecond (Cmd.map DetailsMsg)

        DeliverablesMsg dMsg ->
            let
                mProject =
                    model.details
                        |> Details.getProject
                        |> Maybe.map toShallowProject
            in
                Deliverables.update mProject dMsg model.deliverables
                    |> Tuple.mapFirst (\d -> { model | deliverables = d })
                    |> Tuple.mapSecond (Cmd.map DeliverablesMsg)

        ActivitiesMsg dMsg ->
            Activities.update dMsg model.activities
                |> Tuple.mapFirst (\a -> { model | activities = a })
                |> Tuple.mapSecond (Cmd.map ActivitiesMsg)


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


toShallowProject : Project -> ProjectS
toShallowProject fullP =
    { fullP
        | owner = fullP.owner.id
        , created_by = fullP.created_by.id
        , contacts = List.map toShallowProjectContact fullP.contacts
        , company = fullP.company.id
    }


toShallowProjectContact : ProjectContact { a | id : String } -> ProjectContact String
toShallowProjectContact c =
    { c | contact = c.contact.id }
