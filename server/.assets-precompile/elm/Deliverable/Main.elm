module Main exposing (..)

import Deliverable.View exposing (root)
import Deliverable.Types exposing (Model, Msg)
import Deliverable.State exposing (init, update)
import Html


-- main : Program Never Model Msg


main =
    Html.programWithFlags
        { init = init
        , view = root
        , update = update
        , subscriptions = \_ -> Sub.none
        }
