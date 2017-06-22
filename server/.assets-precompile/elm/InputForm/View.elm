module InputForm.View exposing (root)

import InputForm.Types exposing (..)
import Html exposing (Html, div, a, text, button, small, label, p, input, h4, select, option)
import Html.Attributes exposing (disabled, class, checked, type_, name, placeholder, href, style, value, selected)
import Html.Events exposing (onClick, onInput, on, targetValue)
import RemoteData


root : Model -> Html Msg
root model =
    div [] [ text "ELM DATA INPUT" ]
