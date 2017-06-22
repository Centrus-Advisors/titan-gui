module CompanyDetails.View exposing (..)

import CompanyDetails.Types exposing (..)
import Html exposing (Html, div, text, label, input, h4, p, i, a, li, button, textarea, select, option)
import Html.Attributes exposing (class, type_, value, style, checked, name, property, disabled, id, for, placeholder, href, selected)
import Html.Events exposing (onClick, onInput, onBlur)
import RemoteData exposing (RemoteData(NotAsked, Loading, Success, Failure))
import Utils.FlashMessages as FlashMessages
import Http
import Utils.Http
import Utils.Ui as Ui
import Regex
import Json.Encode
import Utils.Activities.Main as Activities
import Utils.FileStorage.Main as FileStorage


view : Model -> Html Msg
view model =
    Ui.showWhenLoaded (subView model) model.company


subView : Model -> Company -> Html Msg
subView model company =
    let
        buttons =
            case model.editableCompany of
                Just aCompany ->
                    div [ class "pull-right text-primary" ]
                        [ button
                            [ class "btn btn-default waves-effect m-r-10", type_ "button", onClick CancelEditing ]
                            [ text "Cancel" ]
                        , button
                            [ class "btn btn-primary waves-effect waves-light"
                            , type_ "button"
                            , onClick <| SaveEditing aCompany
                            ]
                            [ text "Save" ]
                        ]

                Nothing ->
                    a
                        [ class "pull-right text-primary" ]
                        [ i
                            [ class "text-big fa fa-pencil"
                            , style [ ( "cursor", "pointer" ) ]
                            , onClick <| EnableEditing company
                            ]
                            []
                        ]

        content =
            case model.editableCompany of
                Just editable ->
                    editableForm editable

                Nothing ->
                    readOnlyForm model.fileStorage company
    in
        div [ class "row" ]
            [ div
                [ class "col-md-6" ]
                [ h4 [ class "clearfix" ]
                    [ text "Details"
                    , buttons
                    ]
                , submissionInfo model.submission
                , content
                ]
            , div
                [ class "col-md-6" ]
                [ Activities.view model.activities
                    |> Html.map ActivitiesMsg
                ]
            ]


submissionInfo : RemoteData Http.Error Company -> Html Msg
submissionInfo submission =
    case submission of
        NotAsked ->
            div [] []

        Loading ->
            FlashMessages.loading

        Success c ->
            FlashMessages.success "Changes saved."

        Failure err ->
            FlashMessages.failure (Utils.Http.errorMessage err)


readOnlyForm : FileStorage.FileStorage -> Company -> Html Msg
readOnlyForm fileStorage company =
    div []
        [ FileStorage.view fileStorage company.requiredFiles
            |> Html.map FileStorageMsg
        , div [ class "form-group" ]
            [ label [] [ text "Name" ]
            , p [ class "form-control" ]
                [ text company.name ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Sector" ]
            , p [ class "form-control" ]
                [ text company.sector ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Classification" ]
            , p [ class "form-control" ]
                [ text company.classification ]
            ]
        , div
            [ class "row" ]
            [ div
                [ class "col-sm-6" ]
                [ div [ class "form-group" ]
                    [ label [] [ text "Investor Classification" ]
                    , p [ class "form-control" ]
                        [ text company.investorClassification ]
                    ]
                ]
            , div
                [ class "col-sm-6" ]
                [ div [ class "form-group" ]
                    [ label [] [ text "Investor Type" ]
                    , p [ class "form-control" ]
                        [ text company.investorType ]
                    ]
                ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Email" ]
            , p [ class "form-control" ]
                [ text company.email ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "website" ]
            , p [ class "form-control" ]
                [ text company.website ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Phones" ]
            , div [] (List.map readOnlyPhone company.phones)
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Related Contacts" ]
            , div [] (List.map readOnlyRelatedContact company.relatedContacts)
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Location" ]
            , p [ class "form-control" ]
                [ text company.location ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Address" ]
            , p
                [ class "form-control"
                , property "innerHTML" <| Json.Encode.string <| (stringToHtml company.address) ++ "&zwnj;"
                , style [ ( "height", "auto" ) ]
                ]
                []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Postcode" ]
            , p [ class "form-control" ]
                [ text company.postcode ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Notes" ]
            , p
                [ class "form-control"
                , style [ ( "height", "auto" ), ( "min-height", "34px" ) ]
                ]
                [ text company.notes ]
            ]
        , div [ class "row" ]
            [ div [ class "col-md-8" ] []
            , div [ class "form-group col-md-4" ]
                [ div [ class "checkbox checkbox-danger" ]
                    [ input
                        [ id "company-archived-checkbox"
                        , type_ "checkbox"
                        , checked company.archived
                        , disabled True
                        ]
                        []
                    , label
                        [ for "company-archived-checkbox" ]
                        [ text "Archived" ]
                    ]
                ]
            ]
        ]


editableForm : EditableCompany -> Html Msg
editableForm company =
    div []
        [ div [ class "form-group" ]
            [ label [] [ text "Required Files" ]
            , Ui.editableList
                requiredFilesListConfig
                "Add File +"
                (List.indexedMap
                    editableRequiredFileRow
                    company.requiredFiles
                )
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Name" ]
            , input [ class "form-control", type_ "text", value company.name, onInput (\v -> CompanyName v |> UpdateField) ]
                []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Sector" ]
            , input [ class "form-control", type_ "text", value company.sector, onInput (CompanySector >> UpdateField) ]
                []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Classification" ]
            , input [ class "form-control", type_ "text", value company.classification, onInput (CompanyClassification >> UpdateField) ]
                []
            ]
        , div
            [ class "row" ]
            [ div
                [ class "col-sm-6" ]
                [ div [ class "form-group" ]
                    [ label [] [ text "Investor Classification" ]
                    , select
                        [ class "form-control", onInput (CompanyInvestorClassification >> UpdateField) ]
                        (List.map (\v -> toOption v company.investorClassification) investorClassifications)
                    ]
                ]
            , div
                [ class "col-sm-6" ]
                [ div [ class "form-group" ]
                    [ label [] [ text "Investor Type" ]
                    , select
                        [ class "form-control", onInput (CompanyInvestorType >> UpdateField) ]
                        (List.map (\v -> toOption v company.investorType) investorTypes)
                    ]
                ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Email" ]
            , input [ class "form-control", type_ "email", value company.email, onInput (CompanyEmail >> UpdateField) ]
                []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Website" ]
            , input [ class "form-control", type_ "text", value company.website, onInput (CompanyWebsite >> UpdateField) ]
                []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Phones" ]
            , div [] (List.indexedMap editablePhone company.phones)
            , button
                [ class "btn btn-primary btn-sm"
                , type_ "button"
                , onClick (UpdateField CompanyAddPhone)
                ]
                [ text "New phone +" ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Related Contacts" ]
            , div [] (List.indexedMap editableRelatedContacts company.relatedContacts)
            , button
                [ class "btn btn-primary btn-sm"
                , type_ "button"
                , onClick (RContact 0 AddContact |> UpdateField)
                ]
                [ text "Add contact +" ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Location" ]
            , input [ class "form-control", type_ "text", value company.location, onInput (\v -> CompanyLocation v |> UpdateField) ]
                []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Address" ]
            , textarea [ class "form-control", value company.address, onInput (CompanyAddress >> UpdateField) ]
                []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Postcode" ]
            , input [ class "form-control", type_ "text", value company.postcode, onInput (CompanyPostcode >> UpdateField) ]
                []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Notes" ]
            , textarea [ class "form-control", value company.notes, onInput (CompanyNotes >> UpdateField) ]
                []
            ]
        , div [ class "row" ]
            [ div [ class "col-md-8" ] []
            , div [ class "form-group col-md-4" ]
                [ div [ class "checkbox checkbox-danger" ]
                    [ input
                        [ id "company-archived-checkbox"
                        , type_ "checkbox"
                        , checked company.archived
                        ]
                        []
                    , label
                        [ for "company-archived-checkbox"
                        , onClick (CompanyArchived (not company.archived) |> UpdateField)
                        ]
                        [ text "Archived" ]
                    ]
                ]
            ]
        ]


toOption txt val =
    option [ selected (val == txt) ] [ text txt ]


investorClassifications =
    [ "TBC"
    , "N/A"
    , "Multi"
    , "Equity"
    , "Debt"
    ]


investorTypes =
    [ "TBC"
    , "N/A"
    , "Asset Manager"
    , "Bank"
    , "Derivatives"
    , "Family Office"
    , "Insurance"
    , "Private Equity"
    , "Pension Fund"
    , "Other"
    ]


editableRequiredFileRow : Int -> String -> Html Msg
editableRequiredFileRow idx fileName =
    div
        [ class "m-b-10" ]
        [ input
            [ class "form-control"
            , style [ ( "min-height", "34px" ), ( "height", "auto" ) ]
            , type_ "text"
            , value fileName
            , placeholder "Type a file name"
            , onInput (FName idx >> RequiredFiles >> UpdateField)
            ]
            []
        ]


editablePhone : Int -> Phone -> Html Msg
editablePhone idx phone =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ input
                [ class "form-control m-b-10"
                , type_ "text"
                , value phone.description
                , placeholder "Description"
                , onInput (\desc -> CompanyUpdatePhone idx desc phone.number |> UpdateField)
                ]
                []
            ]
        , div [ class "col-sm-5" ]
            [ input
                [ class "form-control"
                , type_ "phone"
                , value phone.number
                , placeholder "Phone number"
                , onInput (\num -> CompanyUpdatePhone idx phone.description num |> UpdateField)
                ]
                []
            ]
        , div [ class "col-sm-1" ]
            [ i
                [ class "text-primary fa fa-minus-square fa-lg m-t-10"
                , style [ ( "cursor", "pointer" ) ]
                , onClick (CompanyRemovePhone idx |> UpdateField)
                ]
                []
            ]
        ]


readOnlyPhone : Phone -> Html Msg
readOnlyPhone phone =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ p [ class "form-control" ] [ text phone.description ]
            ]
        , div [ class "col-sm-6" ]
            [ p [ class "form-control" ] [ text phone.number ]
            ]
        ]


readOnlyRelatedContact : CompanyContact -> Html Msg
readOnlyRelatedContact related =
    div [ class "row" ]
        [ div [ class "col-sm-6" ]
            [ p [ class "form-control" ] [ text related.relation ]
            ]
        , div [ class "col-sm-6" ]
            [ case related.contact of
                Just contact ->
                    a
                        [ class "form-control"
                        , href <| "/contacts/contact-details/" ++ contact.id
                        ]
                        [ text contact.name ]

                Nothing ->
                    p [ class "form-control" ] []
            ]
        ]


editableRelatedContacts : Int -> EditableCompanyContact -> Html Msg
editableRelatedContacts idx related =
    div [ class "row" ]
        [ div [ class "col-sm-5" ]
            [ input
                [ class "form-control"
                , type_ "text"
                , placeholder "Relation to the company"
                , value related.relation
                , onInput (\txt -> UpdateField <| RContact idx <| SetRelation txt)
                ]
                []
            ]
        , div [ class "col-sm-6" ]
            [ contactSearchBox idx related.contact related.search ]
        , div [ class "col-sm-1" ]
            [ i
                [ class "text-primary fa fa-minus-square fa-lg m-t-10"
                , style [ ( "cursor", "pointer" ) ]
                , onClick (UpdateField <| RContact idx RemoveContact)
                ]
                []
            ]
        ]


contactSearchBox : Int -> Maybe Contact -> SuggestionSearch Contact -> Html Msg
contactSearchBox idx contact search =
    let
        itemClickable suggestion =
            li [ onClick <| UpdateField <| RContact idx <| ChooseContact <| Just suggestion ]
                [ a []
                    [ text suggestion.name ]
                ]

        itemText t =
            li [] [ a [] [ t ] ]

        suggestions =
            case search.suggestions of
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
            case search.suggestions of
                RemoteData.NotAsked ->
                    contact |> Maybe.map .name |> Maybe.withDefault ""

                _ ->
                    search.text

        dropdown =
            if search.suggestions == RemoteData.NotAsked then
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
                , class "form-control m-b-10"
                , placeholder "Contact name"
                , onInput (\v -> UpdateField <| RContact idx <| Search v)
                , onBlur (UpdateField <| RContact idx BlurSearchBox)
                , value inputValue
                ]
                []
            , dropdown
            ]


requiredFilesListConfig =
    { onAddItem = FAdd |> RequiredFiles |> UpdateField
    , onRemoveItem = FRemove >> RequiredFiles >> UpdateField
    }


stringToHtml aString =
    Regex.replace Regex.All (Regex.regex "\n") (\_ -> "</br>") aString
