module Main exposing (..)

import NewUser.View exposing (root)
import NewUser.Types exposing (Model, Msg)
import NewUser.State exposing (init, update, subscriptions)
import Html


-- main : Program Never Model Msg


main =
    Html.programWithFlags
        { init = init
        , view = root
        , update = update
        , subscriptions = subscriptions
        }
