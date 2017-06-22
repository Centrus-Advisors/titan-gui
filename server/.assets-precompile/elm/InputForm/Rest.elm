module InputForm.Rest exposing (..)

import InputForm.Types exposing (..)
import Json.Encode
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Decode exposing (Decoder, string, nullable, list)
import Http
import RemoteData exposing (WebData)


doSomething =
    2
