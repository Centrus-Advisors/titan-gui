module Main exposing (..)

import Project.View exposing (root)
import Project.Types exposing (Model, Msg)
import Project.State exposing (init, update, subscriptions)
import Html


-- main : Program Never Model Msg


main =
    Html.programWithFlags
        { init = init
        , view = root
        , update = update
        , subscriptions = subscriptions
        }
