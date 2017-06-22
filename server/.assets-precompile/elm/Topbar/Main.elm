module Topbar.Main exposing (..)

import Topbar.View exposing (root)
import Topbar.Types exposing (Model, Msg)
import Topbar.State exposing (init, update, subscriptions)
import Html


-- main : Program Never Model Msg


main =
    Html.programWithFlags
        { init = init
        , view = root
        , update = update
        , subscriptions = subscriptions
        }
