module Topbar.State exposing (init, update, subscriptions)

import Topbar.Types exposing (..)
import Topbar.TimeTracker.Main as TimeTracker
import Time exposing (Time)


init : Time -> ( Model, Cmd Msg )
init currentTime =
    let
        ( tracker, trackerMsg ) =
            TimeTracker.init currentTime
    in
        { timeTracker = tracker
        }
            ! [ trackerMsg
                    |> Cmd.map TimeTrackerMsg
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeTrackerMsg subMsg ->
            let
                ( tracker, trackerMsg ) =
                    TimeTracker.update subMsg model.timeTracker
            in
                { model | timeTracker = tracker }
                    ! [ trackerMsg
                            |> Cmd.map TimeTrackerMsg
                      ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ TimeTracker.subscriptions model.timeTracker
            |> Sub.map TimeTrackerMsg
        ]
