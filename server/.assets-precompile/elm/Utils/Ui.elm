module Utils.Ui exposing (..)

import Html exposing (Html, div, label, p, text, i, li, a, input, textarea, button, h4, br)
import Html.Attributes exposing (class, style, type_, value, rows, placeholder)
import Html.Events exposing (onInput, onBlur, onClick)
import RemoteData exposing (RemoteData)
import Http
import Utils.Http
import Utils.Api
import Utils.FlashMessages


--------------
-- SEARCH BOX
--------------


type alias SearchBoxConfig a msg =
    { onSelectOne : Maybe a -> msg
    , onBlur : msg
    , onSearch : String -> msg
    }


type alias SuggestionSearch a =
    { suggestions : RemoteData Http.Error (List a)
    , text : String
    }


searchBox :
    SearchBoxConfig { a | name : String } msg
    -> Maybe { a | name : String }
    -> SuggestionSearch { a | name : String }
    -> Html msg
searchBox config maybeChosen itemSearch =
    let
        itemClickable suggestion =
            li
                [ onClick <| config.onSelectOne <| Just suggestion ]
                [ a [] [ text suggestion.name ] ]

        itemText t =
            li [] [ a [] [ t ] ]

        suggestions =
            case itemSearch.suggestions of
                RemoteData.Success values ->
                    if List.length values > 0 then
                        List.map itemClickable values
                    else
                        [ itemText <| text "No result found" ]

                RemoteData.Failure err ->
                    [ itemText <| text <| Utils.Http.errorMessage err ]

                RemoteData.Loading ->
                    [ li
                        [ class "text-center" ]
                        [ i [ class "fa fa-circle-o-notch fa-spin" ] [] ]
                    ]

                RemoteData.NotAsked ->
                    []

        inputValue =
            case itemSearch.suggestions of
                RemoteData.NotAsked ->
                    maybeChosen |> Maybe.map .name |> Maybe.withDefault ""

                _ ->
                    itemSearch.text

        dropdown =
            if itemSearch.suggestions == RemoteData.NotAsked then
                div [] []
            else
                div
                    [ class "dropdown-menu"
                    , style [ ( "display", "block" ) ]
                    ]
                    suggestions
    in
        div [ style [ ( "position", "relative" ) ] ]
            [ input
                [ type_ "text"
                , class "form-control"
                , onInput config.onSearch
                , onBlur config.onBlur
                , value inputValue
                ]
                []
            , dropdown
            ]



--------------
-- EDITABLE LIST
--------------


type alias EditableListConfig msg =
    { onAddItem : msg
    , onRemoveItem : Int -> msg
    }


editableList : EditableListConfig msg -> String -> List (Html msg) -> Html msg
editableList config newItemLabel listItems =
    div [] <|
        (List.indexedMap (editableListItem config) listItems)
            ++ [ button
                    [ class "btn btn-primary btn-sm"
                    , type_ "button"
                    , onClick config.onAddItem
                    ]
                    [ text newItemLabel ]
               ]


editableListItem : EditableListConfig msg -> Int -> Html msg -> Html msg
editableListItem config idx rowContent =
    div [ class "row" ]
        [ div [ class "col-sm-11" ]
            [ rowContent ]
        , div [ class "col-sm-1" ]
            [ i
                [ class "text-primary fa fa-minus-square fa-lg m-t-10"
                , style [ ( "cursor", "pointer" ) ]
                , onClick (config.onRemoveItem idx)
                ]
                []
            ]
        ]



--------------
-- REMOTE DATA INFO
--------------


submissionInfo : String -> RemoteData Http.Error a -> Html msg
submissionInfo successMesssage submission =
    case submission of
        RemoteData.NotAsked ->
            div [] []

        RemoteData.Loading ->
            Utils.FlashMessages.loading

        RemoteData.Success c ->
            Utils.FlashMessages.success successMesssage

        RemoteData.Failure err ->
            Utils.FlashMessages.failure (Utils.Http.errorMessage err)


showWhenLoaded showFunc fetch =
    case fetch of
        RemoteData.Success val ->
            showFunc val

        RemoteData.Failure err ->
            Utils.Api.errorMessage err
                |> Utils.FlashMessages.failure

        RemoteData.Loading ->
            spinner

        _ ->
            text ""



--------------
-- MODAL
--------------


modalHeader : String -> msg -> Html msg
modalHeader titleText closeModalMsg =
    div
        [ class "modal-header" ]
        [ button
            [ class "close", onClick closeModalMsg ]
            [ text "×" ]
        , h4
            [ class "modal-title" ]
            [ text titleText ]
        ]


modalBody : List (Html msg) -> Html msg
modalBody content =
    div
        [ class "modal-body" ]
        content


modalFooter : List (Html msg) -> Html msg
modalFooter content =
    div [ class "modal-footer" ]
        content


modal : List (Html msg) -> Html msg
modal content =
    div
        [ class "elm-modal" ]
        [ div
            [ class "modal-dialog" ]
            [ div
                [ class "modal-content" ]
                [ div [] content
                ]
            ]
        ]


spinner =
    div [ class "spinningCircle" ] []


inlineSpinner =
    i [ class "fa fa-spinner fa-pulse fa-fw" ] []



--------------
-- Form fields
--------------


inputField : String -> String -> (String -> msg) -> Html msg
inputField placeholderTxt contentTxt onInputMsg =
    input
        [ class "form-control"
        , style [ ( "min-height", "34px" ), ( "height", "auto" ) ]
        , type_ "text"
        , value contentTxt
        , placeholder placeholderTxt
        , onInput onInputMsg
        ]
        []


textAreaField : String -> String -> (String -> msg) -> Html msg
textAreaField placeholderTxt contentTxt onInputMsg =
    textarea
        [ class "form-control"
        , style [ ( "min-height", "34px" ), ( "height", "auto" ) ]
        , placeholder placeholderTxt
        , rows 4
        , value contentTxt
        , onInput onInputMsg
        ]
        []


formTitle : String -> List (Html msg) -> Html msg
formTitle txt content =
    div [ class "form-group" ]
        ([ label [] [ text txt ] ]
            ++ content
        )


textField : String -> Html msg
textField contentTxt =
    formField (jsonStringToHtml contentTxt)


formField : List (Html msg) -> Html msg
formField content =
    p
        [ class "form-control"
        , style
            [ ( "min-height", "34px" )
            , ( "height", "auto" )
            ]
        ]
        content


jsonStringToHtml : String -> List (Html msg)
jsonStringToHtml content =
    content
        |> String.split "\n"
        |> List.map text
        |> List.intersperse (br [] [])
