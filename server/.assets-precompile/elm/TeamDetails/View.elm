module TeamDetails.View exposing (view)

import TeamDetails.Types exposing (..)
import Html exposing (Html, text, p, div, label, a, h4, button, i, strong, input, span, tr)
import Html.Attributes exposing (class, style, href, type_, value, placeholder)
import Html.Events exposing (onClick, onInput)
import Table exposing (defaultCustomizations)
import Utils.Ui as Ui
import Utils.Routes as Routes
import Utils.SearchBox as SearchBox
import RemoteData
import Date.Extra
import Date exposing (Date)


view : Model -> Html Msg
view model =
    div
        []
        [ pageHeader model.canBeEdited model.editable
        , Ui.submissionInfo "Team deleted permanently" model.deleteTeamSubmission
        , Ui.submissionInfo "Changes saved" model.submission
        , case ( model.canBeEdited, model.editable ) of
            ( True, Just editable ) ->
                editableTeam editable

            _ ->
                div []
                    [ Ui.showWhenLoaded teamDetails model.team
                    , Ui.showWhenLoaded membersDetails model.members
                    ]
        ]


pageHeader canBeEdited editable =
    let
        isEditing =
            editable
                |> Maybe.map (\_ -> True)
                |> Maybe.withDefault False
    in
        h4
            [ class "clearfix" ]
            [ strong [] [ text "DETAILS" ]
            , if canBeEdited then
                toggleEditingButtons isEditing
              else
                text ""
            ]


toggleEditingButtons : Bool -> Html Msg
toggleEditingButtons isEditing =
    if isEditing then
        div [ class "pull-right text-primary" ]
            [ button
                [ class "btn btn-default waves-effect m-r-10"
                , type_ "button"
                , onClick (SetEditing False)
                ]
                [ text "Cancel" ]
            , button
                [ class "btn btn-primary waves-effect waves-light"
                , type_ "button"
                , onClick SaveEditing
                ]
                [ text "Save" ]
            ]
    else
        a
            [ class "pull-right text-primary" ]
            [ i
                [ class "text-big fa fa-pencil"
                , style [ ( "cursor", "pointer" ) ]
                , onClick (SetEditing True)
                ]
                []
            ]


header : String -> List (Html Msg) -> Html Msg
header txt content =
    div [ class "form-group" ]
        ([ label [] [ text txt ] ]
            ++ content
        )


textField : String -> Html msg
textField contentTxt =
    p
        [ class "form-control"
        , style [ ( "min-height", "34px" ), ( "height", "auto" ) ]
        ]
        [ text contentTxt
        ]


teamDetails : Team -> Html Msg
teamDetails team =
    div
        []
        [ header "Name"
            [ textField team.name
            ]
        , header "Team Leader"
            [ case team.leader of
                Nothing ->
                    textField ""

                Just aUser ->
                    memberDetails aUser
            ]
        ]


membersDetails : List User -> Html Msg
membersDetails members =
    div
        []
        [ header "Members" <| List.map memberDetails members
        ]


memberDetails : User -> Html Msg
memberDetails user =
    p
        [ class "form-control" ]
        [ case user.contact of
            Nothing ->
                text user.email

            Just contact ->
                a
                    [ href <| Routes.contactPage contact.id ]
                    [ text contact.name ]
        ]


editableTeam editable =
    div
        []
        [ header "Name"
            [ inputField "Team name" editable.name (ChangeName >> TeamEditing)
            ]
        , header "Leader"
            [ SearchBox.view editable.leader
                |> Html.map (SearchLeader >> TeamEditing)
            ]
        , header "Members"
            [ Ui.showWhenLoaded removableMembersList editable.members ]
        , header "Add member"
            [ div
                [ class "row" ]
                [ div
                    [ class "col-sm-10" ]
                    [ SearchBox.view editable.newMember
                        |> Html.map (SearchNewMember >> TeamEditing)
                    ]
                , div
                    [ class "col-sm-2 text-right" ]
                    [ button
                        [ class "btn btn-primary"
                        , onClick (ChooseNewMember |> TeamEditing)
                        ]
                        [ text "Add Member" ]
                    ]
                ]
            ]
        , div
            [ class "text-right" ]
            [ button
                [ class "btn btn-danger"
                , onClick (ToggleDeletionModal |> TeamEditing)
                ]
                [ text "Delete" ]
            ]
        , if editable.confirmDeleteModalShowing then
            confirmDeletionModal
          else
            text ""
        ]


confirmDeletionModal =
    Ui.modal
        [ Ui.modalHeader "Confirm Team Deletion" (ToggleDeletionModal |> TeamEditing)
        , Ui.modalBody
            [ p [] [ text "Are you sure you want to delete this team permanently?" ]
            , p [] [ text "Members of this team will become unassigned to any team." ]
            , p [] [ strong [] [ text "This cannot be undone." ] ]
            ]
        , Ui.modalFooter
            [ button [ class "btn btn-default waves-effect", onClick (ToggleDeletionModal |> TeamEditing) ]
                [ text "Cancel" ]
            , button [ class "btn btn-danger waves-effect waves-light", onClick (DeleteTeam) ]
                [ text "Delete Team Permanently" ]
            ]
        ]


removableMembersList members =
    div [] <| List.map removableMember members


removableMember user =
    div
        [ class "row" ]
        [ div
            [ class "col-sm-10" ]
            [ memberDetails user ]
        , div
            [ class "col-sm-2 text-right" ]
            [ button
                [ class "btn btn-primary"
                , onClick (RemoveMember user |> TeamEditing)
                ]
                [ text "Remove" ]
            ]
        ]


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
