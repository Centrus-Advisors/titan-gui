port module InputForm.State exposing (init, update, subscriptions)

import InputForm.Types exposing (..)
import RemoteData
import Time exposing (Time)


init : { todayDate : Time } -> ( Model, Cmd Msg )
init _ =
    {} ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


subscriptions model =
    Sub.none
