module Main exposing (..)

import ContactDetails.View exposing (view)
import ContactDetails.Types exposing (Model, Msg)
import ContactDetails.State exposing (init, update)
import Html


-- main : Program Never Model Msg


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
