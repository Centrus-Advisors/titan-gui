module NewProject exposing (..)

import NewProject.View exposing (root)
import NewProject.Types exposing (Model, Msg)
import NewProject.State exposing (init, update, subscriptions)
import Html


-- main : Program Never Model Msg


main =
    Html.programWithFlags
        { init = init
        , view = root
        , update = update
        , subscriptions = subscriptions
        }
