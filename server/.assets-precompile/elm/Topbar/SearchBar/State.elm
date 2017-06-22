module Topbar.State exposing (init, update)

import Topbar.Types exposing (..)
import Common.Types exposing (Topbar, Cost)


init : () -> ( Model, Cmd Msg )
init _ =
    {} ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []
