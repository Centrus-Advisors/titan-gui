module DownloadForm exposing (..)

import DownloadForm.View exposing (root)
import DownloadForm.Types exposing (Model, Msg)
import DownloadForm.State exposing (init, update, subscriptions)
import Html


-- main : Program Never Model Msg


main =
    Html.programWithFlags
        { init = init
        , view = root
        , update = update
        , subscriptions = subscriptions
        }
