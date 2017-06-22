module Utils.FlashMessages exposing (..)

import Html exposing (Html, div, i, text, button, br)
import Html.Attributes exposing (class, attribute, type_, property)
import Regex


success : String -> Html msg
success message =
    div
        [ class "alert alert-success" ]
        (jsonToHtml message)


failure : String -> Html msg
failure message =
    div
        [ class "alert alert-danger" ]
        (jsonToHtml message)


loading : Html msg
loading =
    div [ class "alert alert-info alert-dismissable text-center" ]
        [ i [ class "fa fa-circle-o-notch fa-spin" ] [] ]


jsonToHtml : String -> List (Html msg)
jsonToHtml txt =
    Regex.split Regex.All (Regex.regex "\n") txt
        |> List.map text
        |> List.intersperse (br [] [])
