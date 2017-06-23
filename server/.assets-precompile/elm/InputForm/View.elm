module InputForm.View exposing (root)

import InputForm.Types exposing (..)
import InputForm.State exposing (entireFormIsValid, validateType)
import Html exposing (Html, div, a, text, button, small, label, p, input, h4, select, option, textarea, span)
import Html.Attributes exposing (disabled, class, checked, type_, name, placeholder, href, style, value, selected, rows)
import Html.Events exposing (onClick, onInput, on, targetValue)
import RemoteData exposing (RemoteData)
import DatePicker
import InputForm.FlashMessages as FlashMessages
import Http


root : Model -> Html Msg
root model =
    div []
        [ div [ class "clearfix" ]
            [ button
                [ class "btn btn-info pull-right"
                , onClick Submit
                ]
                [ text "Submit" ]
            , h4 [] [ text "Data Input" ]
            ]
        , div
            [ class "widget-simple-chart card-box" ]
            [ submissionInfo "Data saved successfully." model.submission
            , validationWarning model.validate model.records
            , renderForm model.validate model.records
            ]
        ]


submissionInfo : String -> RemoteData Http.Error a -> Html msg
submissionInfo successMesssage submission =
    case submission of
        RemoteData.NotAsked ->
            div [] []

        RemoteData.Loading ->
            FlashMessages.loading

        RemoteData.Success c ->
            FlashMessages.success successMesssage

        RemoteData.Failure err ->
            FlashMessages.failure (httpErrorMessage err)


httpErrorMessage : Http.Error -> String
httpErrorMessage error =
    case error of
        Http.BadUrl wrongUrl ->
            "Invalid url: " ++ wrongUrl

        Http.Timeout ->
            "The server didn't respond on time. Please try again"

        Http.NetworkError ->
            "Unable to connect to server"

        Http.BadPayload errMessage { status } ->
            "Unable to parse server response: " ++ errMessage

        Http.BadStatus { status, body } ->
            "Server returned " ++ (toString status.code) ++ ". " ++ status.message


validationWarning validate form =
    if validate && not (entireFormIsValid form) then
        FlashMessages.failure "There are errors in the form. Please correct them before submission"
    else
        text ""


renderForm validate records =
    let
        toPairs newElement ( pairs, newPair ) =
            if List.isEmpty newPair then
                ( pairs, [ newElement ] )
            else
                ( (newElement :: newPair) :: pairs, [] )
    in
        List.indexedMap (renderFormItem validate) records
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

        DBTimeStamp nullable val ->
            inputField placeholder val (ChangeRecord idx)

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
