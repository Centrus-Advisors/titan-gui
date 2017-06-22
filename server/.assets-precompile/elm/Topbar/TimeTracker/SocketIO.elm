port module Topbar.TimeTracker.SocketIO exposing (..)

import Json.Decode


-- Sending stuff out


port send : Json.Decode.Value -> Cmd msg



-- Receiving stuff


port receive : (Json.Decode.Value -> msg) -> Sub msg
