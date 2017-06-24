module DownloadForm.View exposing (root)

import DownloadForm.Types exposing (..)
import Html exposing (Html, div, a, text, button, i, small, label, p, input, h4, select, option, textarea, table, tr, td)
import Html.Attributes exposing (attribute, disabled, class, checked, type_, name, placeholder, href, style, value, selected, rows)
import Html.Events exposing (onClick, onInput, on, targetValue)
import DatePicker
import Date.Extra


root : Model -> Html Msg
root model =
    div [ style [ ( "padding-bottom", "200px" ) ], class "row" ]
        [ div [ class "col-md-6" ]
            [ div
                [ class "card-box" ]
                [ h4
                    []
                    [ text "Pick a date" ]
                , table []
                    [ tr []
                        [ td [] [ text "From " ]
                        , td []
                            [ renderPicker model.fromDate
                                |> Html.map FromDate
                            ]
                        ]
                    , tr []
                        [ td [] [ text "To " ]
                        , td []
                            [ renderPicker model.toDate
                                |> Html.map ToDate
                            ]
                        ]
                    ]
                , a
                    [ class "btn btn-inverse waves-effect waves-light m-b-5 m-l-10"
                    , href "data-input-api"
                    , attribute "download" <|
                        "Trades - "
                            ++ (getDateString "yyyy-MM-dd" model.fromDate)
                            ++ " to "
                            ++ ((getDateString "yyyy-MM-dd" model.toDate))
                    ]
                    [ text <| " Download CSV "
                    , i [ class "fa fa-download" ] []
                    ]
                ]
            ]
        , div [ class "col-md-6" ]
            [ div
                [ class "card-box " ]
                [ h4
                    []
                    [ text "Complete database" ]
                , a
                    [ class "btn btn-inverse waves-effect waves-light m-b-5 m-l-10"
                    , href "data-input-api"
                    , attribute "download" "All Trades"
                    ]
                    [ text <| " Download CSV "
                    , i [ class "fa fa-download" ] []
                    ]
                ]
            ]
        ]


renderPicker picker =
    div
        [ class "m-b-10"
        , style
            [ ( "max-width", "10em" )
            , ( "display", "inline-block" )
            ]
        ]
        [ DatePicker.view picker
        ]


getDateString format picker =
    DatePicker.getDate picker
        |> Maybe.map (Date.Extra.toFormattedString format)
        |> Maybe.withDefault ""
