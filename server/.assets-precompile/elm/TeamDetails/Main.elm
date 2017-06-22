module Main exposing (..)

import TeamDetails.View exposing (view)
import TeamDetails.Types exposing (Model, Msg)
import TeamDetails.State exposing (init, update)
import Html


-- main : Program Never Model Msg


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
