module Main exposing (..)

import CompanyDetails.View exposing (view)
import CompanyDetails.Types exposing (Model, Msg, Company)
import CompanyDetails.State exposing (init, update)
import Html


-- main : Program Never Model Msg


main =
    Html.programWithFlags
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
