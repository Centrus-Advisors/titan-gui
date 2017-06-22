module InputForm exposing (..)

import InputForm.View exposing (root)
import InputForm.Types exposing (Model, Msg)
import InputForm.State exposing (init, update, subscriptions)
import Html


-- main : Program Never Model Msg


main =
    Html.programWithFlags
        { init = init
        , view = root
        , update = update
        , subscriptions = subscriptions
        }
