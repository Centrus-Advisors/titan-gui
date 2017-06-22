module NewUser.View exposing (root)

import NewUser.Types exposing (..)
import Common.Types exposing (ContactS, Permission(ADMINISTRATOR, STANDARD, PRIVILEGED))
import Html exposing (Html, div, a, text, button, small, label, p, input, h4, select, option)
import Html.Attributes exposing (disabled, class, checked, type_, name, placeholder, href, style, value, selected)
import Html.Events exposing (onClick, onInput, on, targetValue)
import Utils.Ui as Ui
import Utils.SearchBox as SearchBox
import Json.Decode
import Utils.Routes as Routes
import RemoteData


root : Model -> Html Msg
root model =
    case model.screen of
        NoScreen ->
            text ""

        ScreenUserCreation subModel ->
            modal model <| userCreation subModel

        ScreenContactCreation subModel ->
            modal model <| contactCreation subModel

        ScreenContactFind subModel ->
            modal model <| contactFind subModel


modal model body =
    Ui.modal
        [ Ui.modalHeader "New User" (SetModalOpen False)
        , Ui.modalBody
            [ body ]
        , modalFooter model
        ]


modalFooter model =
    let
        validEmails u =
            u.email1 == u.email2 && String.length u.email1 > 3

        validPass u =
            u.pass1 == u.pass2 && String.length u.pass1 > 3

        createDisabled =
            case model.screen of
                ScreenUserCreation subModel ->
                    not (validEmails subModel.user && validPass subModel.user)

                _ ->
                    True

        userAlreadyCreated =
            case model.screen of
                ScreenUserCreation subModel ->
                    case subModel.submission of
                        RemoteData.Success _ ->
                            True

                        _ ->
                            False

                _ ->
                    False
    in
        if userAlreadyCreated then
            Ui.modalFooter
                [ button
                    [ class "btn btn-default waves-effect"
                    , onClick (SetModalOpen False)
                    ]
                    [ text "Close" ]
                ]
        else
            Ui.modalFooter
                [ button
                    [ class "btn btn-default waves-effect"
                    , onClick (SetModalOpen False)
                    ]
                    [ text "Cancel" ]
                , button
                    [ class "btn btn-primary waves-effect waves-light"
                    , disabled createDisabled
                    , onClick (SubmitUser |> UserCreation)
                    ]
                    [ text "Create User" ]
                ]


header : String -> List (Html Msg) -> Html Msg
header txt content =
    div [ class "form-group" ]
        ([ label [] [ text txt ] ]
            ++ content
        )


inputField : String -> String -> (String -> Msg) -> Html Msg
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


userCreation : UserCreationModel -> Html Msg
userCreation { user, submission } =
    div
        []
        [ Ui.submissionInfo "User created" submission
        , div
            [ class "row" ]
            [ div
                [ class "col-md-6" ]
                [ header "Permission level"
                    [ select
                        [ class "form-control"
                        , on "change" (Json.Decode.map (ChangePermission >> UserCreation) targetValue)
                        ]
                        [ option [ selected (user.permission == ADMINISTRATOR) ] [ text "ADMINISTRATOR" ]
                        , option [ selected (user.permission == PRIVILEGED) ] [ text "PRIVILEGED" ]
                        , option [ selected (user.permission == STANDARD) ] [ text "STANDARD" ]
                        ]
                    ]
                ]
            , div
                [ class "col-md-6" ]
                [ header "Position"
                    [ SearchBox.view user.position
                        |> Html.map (ChangePosition >> UserCreation)
                    ]
                ]
            ]
        , header "Email address"
            [ inputField "Type the new user's email address" user.email1 (ChangeEmail1 >> UserCreation)
            , div [ class "m-t-15" ] []
            , inputField "Re-Type the email address" user.email2 (ChangeEmail2 >> UserCreation)
            , if user.email1 /= user.email2 then
                p [] [ small [ class "text-danger" ] [ text "emails must match" ] ]
              else
                text ""
            ]
        , div
            [ class "row" ]
            [ div
                [ class "col-md-6" ]
                [ header "Set a password"
                    [ input
                        [ class "form-control"
                        , style [ ( "min-height", "34px" ), ( "height", "auto" ) ]
                        , type_ "password"
                        , value user.pass1
                        , placeholder "Type a password"
                        , onInput (ChangePassword1 >> UserCreation)
                        ]
                        []
                    , input
                        [ class "form-control m-t-15"
                        , style [ ( "min-height", "34px" ), ( "height", "auto" ) ]
                        , type_ "password"
                        , value user.pass2
                        , placeholder "Re-Type a password"
                        , onInput (ChangePassword2 >> UserCreation)
                        ]
                        []
                    , if user.pass1 /= user.pass2 then
                        p [] [ small [ class "text-danger" ] [ text "passwords must match" ] ]
                      else
                        text ""
                    ]
                ]
            , div
                [ class "col-md-6" ]
                [ header "Status"
                    [ div [ class "radio radio-success" ]
                        [ input
                            [ type_ "radio"
                            , name "status"
                            , checked (user.active)
                            , onClick (ChangeActive True |> UserCreation)
                            ]
                            []
                        , label [] [ text "Active" ]
                        ]
                    , div [ class "radio" ]
                        [ input
                            [ type_ "radio"
                            , name "status"
                            , checked (not user.active)
                            , onClick (ChangeActive False |> UserCreation)
                            ]
                            []
                        , label [] [ text "Inactive" ]
                        ]
                    ]
                ]
            ]
        , header "Related contact"
            [ p
                [ class "form-control text-muted" ]
                [ a
                    [ href <| Routes.contactPage user.contact.id ]
                    [ text user.contact.name ]
                ]
            ]
        ]


contactExplanation =
    p
        []
        [ small
            [ class "text-muted" ]
            [ text "Users are linked to contact records. To create a new user you need to link it to an existing contact or create a new contact for the user" ]
        ]


contactCreation : ContactCreationModel -> Html Msg
contactCreation subModel =
    div []
        [ button
            [ class "btn btn-primary btn-xs pull-right"
            , onClick GoToContactFind
            ]
            [ text "Find an existing contact" ]
        , h4 [] [ text "New Contact" ]
        , Ui.submissionInfo "Contact created" subModel.submission
        , header "Name"
            [ inputField "New user's name" subModel.contact.name (ChangeContactName >> ContactCreation)
            ]
        , header "Email"
            [ inputField "New user's email" subModel.contact.email (ChangeContactEmail >> ContactCreation)
            ]
        , button
            [ class "btn btn-primary"
            , onClick (SubmitContact |> ContactCreation)
            ]
            [ text "Create Contact" ]
        , contactExplanation
        ]


contactFind : ContactFindModel -> Html Msg
contactFind subModel =
    div []
        [ button
            [ class "btn btn-primary btn-xs pull-right"
            , onClick GoToContactCreation
            ]
            [ text "Create new contact" ]
        , header "Related Contact"
            [ SearchBox.view subModel
                |> Html.map (ChangeSearch >> ContactFind)
            ]
        , case SearchBox.getChosen subModel of
            Nothing ->
                button
                    [ class "btn btn-primary"
                    , disabled True
                    ]
                    [ text "Select" ]

            Just contact ->
                button
                    [ class "btn btn-primary"
                    , onClick (SelectContact contact)
                    ]
                    [ text "Select" ]
        , contactExplanation
        ]
