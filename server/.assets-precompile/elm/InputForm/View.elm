module InputForm.View exposing (root)

import InputForm.Types exposing (..)
import Html exposing (Html, div, a, text, button, small, label, p, input, h4, select, option, textarea)
import Html.Attributes exposing (disabled, class, checked, type_, name, placeholder, href, style, value, selected, rows)
import Html.Events exposing (onClick, onInput, on, targetValue)
import RemoteData
import DatePicker


root : Model -> Html Msg
root model =
    renderForm model.records


renderForm : List ( String, String, DBType ) -> Html Msg
renderForm form =
    div [ class "row" ] <|
        List.map renderFormItem form


renderFormItem : ( String, String, DBType ) -> Html Msg
renderFormItem ( dbName, dbTitle, dbType ) =
    div [ class "col-md-6" ]
        [ formTitle dbTitle
            [ formField dbType ]
        ]


formField : DBType -> Html Msg
formField v =
    case v of
        DBString nullable maxLen mContent ->
            inputField "Type here" (Maybe.withDefault "" mContent) (always DoNothing)

        DBTimeStamp nullable datePicker ->
            DatePicker.view datePicker
                |> Html.map (always DoNothing)

        DBDate nullable datePicker ->
            DatePicker.view datePicker
                |> Html.map (always DoNothing)

        DBNumber nullable mInt ->
            inputField "Type a number here" (mInt |> Maybe.map toString |> Maybe.withDefault "") (always DoNothing)

        DBFloat nullable mFloat ->
            inputField "Type a number here" (mFloat |> Maybe.map toString |> Maybe.withDefault "") (always DoNothing)



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
