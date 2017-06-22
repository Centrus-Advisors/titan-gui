module Utils.Activities.Filters.View exposing (view)

import Utils.Activities.Filters.Types exposing (..)
import Html exposing (Html, div, a, select, option, text, small, p, input)
import Html.Attributes exposing (class, name, type_, placeholder, style)
import Html.Events exposing (on, onInput, targetValue)
import DatePicker
import Utils.SearchBox as SearchBox
import Json.Decode


onChange : (String -> value) -> Html.Attribute value
onChange msg =
    on "change" (Json.Decode.map msg targetValue)


view : Filters -> Html Msg
view (Filters model) =
    div
        [ class "row", style [ ( "margin-bottom", "15px" ) ] ]
        [ div
            [ class "col-sm-6" ]
            [ input
                [ class "form-control"
                , name "search"
                , placeholder "Search"
                , type_ "text"
                , onInput (TextUpdate)
                ]
                []
            ]
        , div
            [ class "col-sm-6" ]
            [ select
                [ class "form-control"
                , onChange SelectFilter
                ]
                [ option [] [ text "No Filter" ]
                , option [] [ text "Date" ]
                , option [] [ text "Tag" ]
                , option [] [ text "Contact" ]
                ]
            ]
        , case model.filter of
            NoFilter ->
                text ""

            TagFilter v ->
                div
                    [ class "col-sm-12 m-t-15" ]
                    [ SearchBox.view v
                        |> Html.map (TagUpdate)
                    ]

            ContactFilter v ->
                div
                    [ class "col-sm-12 m-t-15" ]
                    [ SearchBox.view v
                        |> Html.map (ContactUpdate)
                    ]

            DateFilter datePicker ->
                div
                    [ class "col-sm-12 m-t-15" ]
                    [ DatePicker.view datePicker
                        |> Html.map (DateUpdate)
                    , p
                        []
                        [ small
                            [ class "text-muted m-l-10" ]
                            [ text "Choose a date up to which to show all activities." ]
                        ]
                    ]
        ]
