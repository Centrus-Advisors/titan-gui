module Topbar.TimeTracker.State exposing (init, update, subscriptions)

import Topbar.TimeTracker.Types exposing (..)
import Common.Types exposing (Deliverable)
import Topbar.TimeTracker.Rest
    exposing
        ( decodeTrackingStatus
        , encodeTrackingStatus
        , searchDeliverables
        , createTimeRecording
        )
import Topbar.TimeTracker.SocketIO as SocketIO
import Date exposing (Date)
import Time exposing (Time)
import RemoteData exposing (RemoteData, WebData)
import Utils.Http
import Utils.SearchBox as SearchBox
import NewDeliverable.Main as NewDeliverable


init : Time -> ( Model, Cmd Msg )
init currentTime =
    { status = RemoteData.Loading
    , currentTime = currentTime
    , creationModal = Nothing
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            model ! []

        ReceivedStatus status ->
            { model | status = status } ! []

        SendStatus status ->
            model ! [ sendStatus status ]

        CurrentTime time ->
            { model | currentTime = time } ! []

        CreateRecording tracking ->
            { model
                | creationModal = toCreationModal model.currentTime tracking
                , status = RemoteData.succeed NotRecording
            }
                ! [ sendStatus NotRecording ]

        CancelCreation ->
            { model
                | creationModal = Nothing
            }
                ! [ sendStatus NotRecording ]

        SubMsgModal subMsg ->
            case model.creationModal of
                Nothing ->
                    model ! []

                Just modal ->
                    updateModal subMsg modal
                        |> Tuple.mapFirst (\m -> { model | creationModal = Just m })


updateModal : ModalMsg -> CreationModal -> ( CreationModal, Cmd Msg )
updateModal msg modal =
    case msg of
        SearchBoxMsg subMsg ->
            SearchBox.update subMsg modal.deliverable
                |> Tuple.mapFirst (\d -> { modal | deliverable = d })
                |> Tuple.mapSecond (Cmd.map (SearchBoxMsg >> SubMsgModal))

        Submit deliverable ->
            { modal | submission = RemoteData.Loading }
                ! [ createTimeRecording deliverable modal.fromDate modal.toDate
                        |> Cmd.map (SubmissionUpdate >> SubMsgModal)
                  ]

        SubmissionUpdate status ->
            { modal | submission = status } ! []

        NewDeliverableMsg subMsg ->
            modal.newDeliverable
                |> Maybe.map (NewDeliverable.update subMsg)
                |> Maybe.map (Tuple.mapFirst (\d -> { modal | newDeliverable = Just d }))
                |> Maybe.withDefault ( modal, Cmd.none )

        CreateNewDeliverable ->
            NewDeliverable.init newDeliverableConfig Nothing
                |> Tuple.mapFirst (\d -> { modal | newDeliverable = Just d })

        NewDeliverableCreated deliverable ->
            { modal
                | newDeliverable = Nothing
                , deliverable = SearchBox.init deliverableSearchConfig (Just deliverable)
            }
                ! []

        NewDeliverableCancel ->
            { modal | newDeliverable = Nothing } ! []


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveStatus ReceivedStatus
        , Time.every Time.second CurrentTime
        ]


receiveStatus : (RemoteData String TrackingStatus -> Msg) -> Sub Msg
receiveStatus msg =
    SocketIO.receive (decodeTrackingStatus >> RemoteData.fromResult >> msg)


sendStatus : TrackingStatus -> Cmd Msg
sendStatus status =
    SocketIO.send (encodeTrackingStatus status)


toCreationModal : Time -> TrackingStatus -> Maybe CreationModal
toCreationModal currentTime status =
    case status of
        NotRecording ->
            Nothing

        Recording date ->
            Just
                { deliverable = SearchBox.init deliverableSearchConfig Nothing
                , fromDate = date
                , toDate = Date.fromTime currentTime
                , submission = RemoteData.NotAsked
                , newDeliverable = Nothing
                }


deliverableSearchConfig : SearchBox.Config Deliverable
deliverableSearchConfig =
    { renderItem = (\d -> d.title ++ " - " ++ d.project.name)
    , renderError = Utils.Http.errorMessage
    , onSearch = searchDeliverables
    , placeholder = "Search deliverable title"
    }


newDeliverableConfig : NewDeliverable.Config Msg
newDeliverableConfig =
    { onUpdate = NewDeliverableMsg >> SubMsgModal
    , onCancel = NewDeliverableCancel |> SubMsgModal
    , onCreated = NewDeliverableCreated >> SubMsgModal
    }
