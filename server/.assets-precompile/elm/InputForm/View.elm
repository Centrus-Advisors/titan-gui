module InputForm.View exposing (root)

import InputForm.Types exposing (..)
import Html exposing (Html, div, a, text, button, small, label, p, input, h4, select, option, textarea, span)
import Html.Attributes exposing (disabled, class, checked, type_, name, placeholder, href, style, value, selected, rows)
import Html.Events exposing (onClick, onInput, on, targetValue)
import RemoteData
import DatePicker


root : Model -> Html Msg
root model =
    let
        toPairs newElement ( pairs, newPair ) =
            if List.isEmpty newPair then
                ( pairs, [ newElement ] )
            else
                ( (newElement :: newPair) :: pairs, [] )
    in
        List.indexedMap (renderFormItem model.validate) model.records
            |> List.foldr toPairs ( [], [] )
            |> Tuple.first
            |> List.map (div [ class "row" ])
            |> div []


renderFormItem : Bool -> Int -> ( String, String, DBType ) -> Html Msg
renderFormItem validate idx ( dbName, dbTitle, dbType ) =
    div [ class "col-md-6" ]
        [ formTitle dbTitle
            [ formField idx dbTitle dbType
            , if validate then
                case validateType dbType of
                    Ok _ ->
                        text ""

                    Err err ->
                        span [ class "text-danger" ] [ text err ]
              else
                text ""
            ]
        ]


formField : Int -> String -> DBType -> Html Msg
formField idx placeholder v =
    case v of
        DBString nullable maxLen val ->
            inputField placeholder val (ChangeRecord idx)

        DBTimeStamp nullable datePicker ->
            DatePicker.view datePicker
                |> Html.map (ChangeDate idx)

        DBDate nullable datePicker ->
            div [ style [ ( "cursor", "pointer" ) ] ]
                [ DatePicker.view datePicker
                    |> Html.map (ChangeDate idx)
                ]

        DBNumber nullable val ->
            inputField "Type a number here" val (ChangeRecord idx)

        DBFloat nullable val ->
            inputField "Type a number here" val (ChangeRecord idx)



--------------
-- Form fields
--------------


validateType : DBType -> Result String ()
validateType v =
    case v of
        DBString nullable maxLen val ->
            ifNotNull nullable
                val
                (\txt ->
                    if String.length txt > maxLen then
                        Err <| "This field exceeds the maximum amount of " ++ (toString maxLen) ++ " characters"
                    else
                        Ok ()
                )

        DBTimeStamp nullable datePicker ->
            emptyPicker nullable datePicker

        DBDate nullable datePicker ->
            emptyPicker nullable datePicker

        DBNumber nullable val ->
            if nullable && String.isEmpty val then
                Ok ()
            else
                case String.toInt val of
                    Ok _ ->
                        Ok ()

                    Err _ ->
                        Err <| "Could not convert \"" ++ val ++ "\" to integer. Please insert a valid number"

        DBFloat nullable val ->
            if nullable && String.isEmpty val then
                Ok ()
            else
                case String.toFloat val of
                    Ok _ ->
                        Ok ()

                    Err _ ->
                        Err <| "Could not convert \"" ++ val ++ "\" to float. Please insert a valid number"


ifNotNull canBeNull val f =
    if String.isEmpty val && not canBeNull then
        Err "This field cannot be empty"
    else
        f val


emptyPicker nullable datePicker =
    case DatePicker.getDate datePicker of
        Just aDate ->
            Ok ()

        Nothing ->
            if nullable then
                Ok ()
            else
                Err "This field cannot be empty. Please choose a date"



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
