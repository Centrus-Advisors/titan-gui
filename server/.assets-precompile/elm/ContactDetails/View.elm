module ContactDetails.View exposing (..)

import ContactDetails.Types exposing (..)
import Common.Types exposing (Phone, Contact, CompanyS)
import Html exposing (Html, div, text, label, input, h4, p, form, i, a, li, button, textarea)
import Html.Attributes exposing (class, type_, value, style, checked, name, property, disabled, href, placeholder)
import Html.Events exposing (onClick, onInput, onBlur)
import RemoteData exposing (WebData, RemoteData(NotAsked, Loading, Success, Failure))
import Utils.FlashMessages as FlashMessages
import Http
import Utils.Http
import Utils.Ui as Ui
import Regex
import Json.Encode
import Utils.Api
import Utils.Activities.Main as Activities
import Utils.FileStorage.Main as FileStorage


view : Model -> Html Msg
view model =
    div [ class "row" ]
        [ div
            [ class "col-md-6" ]
            [ details model.fileStorage model.loadedModel ]
        , div
            [ class "col-md-6" ]
            [ Activities.view model.activities
                |> Html.map ActivitiesMsg
            ]
        ]


details : FileStorage.FileStorage -> WebData LoadedModel -> Html Msg
details fileStorage loadedModel =
    case loadedModel of
        RemoteData.Failure err ->
            h4 [] [ text <| Utils.Api.errorMessage err ]

        RemoteData.NotAsked ->
            h4 [] [ text "An unknown error occurred. Unable to fetch user data" ]

        RemoteData.Loading ->
            div [ class "spinningCircle" ] []

        RemoteData.Success loadedModel ->
            loadedView fileStorage loadedModel


loadedView : FileStorage.FileStorage -> LoadedModel -> Html Msg
loadedView fileStorage model =
    let
        isEditing =
            model.editableContact
                |> Maybe.map (\_ -> True)
                |> Maybe.withDefault False

        buttons =
            if isEditing then
                div [ class "pull-right text-primary" ]
                    [ button
                        [ class "btn btn-default waves-effect m-r-10", type_ "button", onClick CancelEditing ]
                        [ text "Cancel" ]
                    , button
                        [ class "btn btn-primary waves-effect waves-light", type_ "button", onClick SaveEditing ]
                        [ text "Save" ]
                    ]
            else
                a
                    [ class "pull-right text-primary" ]
                    [ i [ class "text-big fa fa-pencil", style [ ( "cursor", "pointer" ) ], onClick EnableEditing ] []
                    ]

        content =
            case model.editableContact of
                Just aContact ->
                    editableForm aContact model.companySearch

                Nothing ->
                    readOnlyForm fileStorage model.contact
    in
        div []
            [ h4 [ class "clearfix" ]
                [ text "Details"
                , buttons
                ]
            , submissionInfo model.submission
            , content
            ]


submissionInfo : RemoteData Http.Error Contact -> Html Msg
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


readOnlyForm : FileStorage.FileStorage -> Contact -> Html Msg
readOnlyForm fileStorage contact =
    form []
        [ FileStorage.view fileStorage contact.requiredFiles
            |> Html.map FileStorageMsg
        , div [ class "form-group" ]
            [ label [] [ text "Name" ]
            , p [ class "form-control" ]
                [ text contact.name ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Email" ]
            , p [ class "form-control" ]
                [ text contact.email ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Company" ]
            , case contact.company of
                Just company ->
                    a [ class "form-control", href <| "/contacts/company-details/" ++ company.id ]
                        [ text company.name ]

                Nothing ->
                    p [ class "form-control" ]
                        [ text "" ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Position" ]
            , p [ class "form-control" ]
                [ text contact.position ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Phones" ]
            , div [] (List.map readOnlyPhone contact.phones)
            ]
        , div [ class "row" ]
            [ div [ class "col-sm-8" ]
                [ div [ class "form-group" ]
                    [ label [] [ text "Location" ]
                    , p [ class "form-control" ]
                        [ text contact.location ]
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Address" ]
                    , p [ class "form-control", style [ ( "min-height", "34px" ), ( "height", "auto" ) ] ]
                        [ text contact.address ]
                    ]
                ]
            , div [ class "col-sm-4" ]
                [ div [ class "form-group" ]
                    [ label [] [ text "Gender" ]
                    , div [ class "radio" ]
                        [ input [ type_ "radio", disabled True, checked (contact.gender == "male"), style [ ( "pointer-events", "none" ) ] ] []
                        , label [] [ text "Male" ]
                        ]
                    , div [ class "radio" ]
                        [ input [ type_ "radio", disabled True, checked (contact.gender == "female"), style [ ( "pointer-events", "none" ) ] ] []
                        , label [] [ text "Female" ]
                        ]
                    ]
                ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Notes" ]
            , p
                [ class "form-control"
                , property "innerHTML" <| Json.Encode.string <| (stringToHtml contact.notes) ++ "&zwnj;"
                , style [ ( "min-height", "34px" ), ( "height", "auto" ) ]
                ]
                []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Contact Owner" ]
            , p [ class "form-control" ]
                [ text contact.created_by.email ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Date Created" ]
            , p [ class "form-control" ]
                [ text contact.createdAt ]
            ]
        ]


listConfig =
    { onAddItem = UpdateField ContactAddPhone
    , onRemoveItem = \idx -> UpdateField (ContactRemovePhone idx)
    }


editableForm : Contact -> SuggestionSearch CompanyS -> Html Msg
editableForm contact companySearch =
    form []
        [ div [ class "form-group" ]
            [ label [] [ text "Required Files" ]
            , Ui.editableList
                requiredFilesListConfig
                "Add File +"
                (List.indexedMap
                    editableRequiredFileRow
                    contact.requiredFiles
                )
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Name" ]
            , input [ class "form-control", type_ "text", value contact.name, onInput (\v -> ContactName v |> UpdateField) ]
                []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Email" ]
            , input [ class "form-control", type_ "email", value contact.email, onInput (\v -> ContactEmail v |> UpdateField) ]
                []
            ]
        , companySearchBox contact.company companySearch
        , div [ class "form-group" ]
            [ label [] [ text "Position" ]
            , input [ class "form-control", type_ "text", value contact.position, onInput (\v -> ContactPosition v |> UpdateField) ]
                []
            ]
        , div [ class "form-group" ]
            [ label []
                [ text "Phones" ]
            , Ui.editableList listConfig "New phone +" (List.indexedMap editablePhone contact.phones)
            ]
        , div [ class "row" ]
            [ div [ class "col-sm-8" ]
                [ div [ class "form-group" ]
                    [ label [] [ text "Address" ]
                    , input [ class "form-control", type_ "text", value contact.address, onInput (\v -> ContactAddress v |> UpdateField) ]
                        []
                    ]
                , div [ class "form-group" ]
                    [ label [] [ text "Location" ]
                    , input [ class "form-control", type_ "text", value contact.location, onInput (\v -> ContactLocation v |> UpdateField) ]
                        []
                    ]
                ]
            , div [ class "col-sm-4" ]
                [ div [ class "form-group" ]
                    [ label [] [ text "Gender" ]
                    , div [ class "radio", onClick (ContactGender "male" |> UpdateField) ]
                        [ input [ type_ "radio", name "gender", checked (contact.gender == "male") ] []
                        , label [] [ text "Male" ]
                        ]
                    , div [ class "radio", onClick (ContactGender "female" |> UpdateField) ]
                        [ input [ type_ "radio", name "gender", checked (contact.gender == "female") ] []
                        , label [] [ text "Female" ]
                        ]
                    ]
                ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Notes" ]
            , textarea [ class "form-control", value contact.notes, onInput (\v -> ContactNotes v |> UpdateField) ]
                []
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Contact Owner" ]
            , p [ class "form-control", type_ "text" ]
                [ text contact.created_by.email ]
            ]
        , div [ class "form-group" ]
            [ label [] [ text "Date Created" ]
            , p [ class "form-control", type_ "text" ]
                [ text contact.createdAt ]
            ]
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
                , onInput (\desc -> ContactUpdatePhone idx desc phone.number |> UpdateField)
                ]
                []
            ]
        , div [ class "col-sm-6" ]
            [ input
                [ class "form-control"
                , type_ "phone"
                , placeholder "Phone number"
                , value phone.number
                , onInput (\num -> ContactUpdatePhone idx phone.description num |> UpdateField)
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


stringToHtml aString =
    Regex.replace Regex.All (Regex.regex "\n") (\_ -> "</br>") aString


searchBoxConfig : Ui.SearchBoxConfig CompanyS Msg
searchBoxConfig =
    { onSelectOne = ChooseCompany
    , onBlur = SearchCompanyBoxBlur
    , onSearch = SearchCompany
    }


companySearchBox : Maybe CompanyS -> SuggestionSearch CompanyS -> Html Msg
companySearchBox company companySearch =
    div [ class "form-group" ]
        [ label [] [ text "Company" ]
        , Ui.searchBox searchBoxConfig company companySearch
        ]


requiredFilesListConfig =
    { onAddItem = FAdd |> RequiredFiles |> UpdateField
    , onRemoveItem = FRemove >> RequiredFiles >> UpdateField
    }
