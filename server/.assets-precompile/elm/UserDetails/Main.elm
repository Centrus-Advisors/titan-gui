module Main exposing (..)

import UserDetails.View exposing (view)
import UserDetails.Types exposing (Model, Msg)
import UserDetails.State exposing (init, update)
import Html


-- main : Program Never Model Msg


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
