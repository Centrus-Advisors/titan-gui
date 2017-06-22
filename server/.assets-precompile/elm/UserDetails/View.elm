module UserDetails.View exposing (..)

import UserDetails.Types exposing (Msg(..), Model)
import Common.Types exposing (User, ContactS, TeamS, Position)
import Html exposing (Html, div, text, label, input, h4, p, form, i, a, li, button, textarea, strong)
import Html.Attributes exposing (class, type_, value, style, checked, name, property, disabled, href, placeholder)
import RemoteData exposing (WebData, RemoteData(NotAsked, Loading, Success, Failure))
import Utils.Ui as Ui
import Utils.Routes as Routes
import Utils.Activities.Main as Activities


view : Model -> Html Msg
view model =
    case model of
        Nothing ->
            div
                []
                [ text "Error parsing User JSON" ]

        Just content ->
            div [ class "row" ]
                [ div
                    [ class "col-md-6" ]
                    [ details content.user ]
                , div
                    [ class "col-md-6" ]
                    [ Activities.view content.activities
                        |> Html.map ActivitiesMsg
                    ]
                ]


details : User -> Html Msg
details user =
    div
        []
        [ h4
            [ class "clearfix" ]
            [ strong [] [ text "DETAILS" ]
            ]
        , header "Email"
            [ textField [ text user.email ]
            ]
        , header "Position"
            [ textField [ text <| Maybe.withDefault "" <| Maybe.map .name user.position ]
            ]
        , header "Team"
            [ teamField user.team
            ]
        , header "Contact"
            [ contactField user.contact
            ]
        ]


header : String -> List (Html Msg) -> Html Msg
header txt content =
    div [ class "form-group" ]
        ([ label [] [ text txt ] ]
            ++ content
        )


textField : List (Html msg) -> Html msg
textField content =
    p
        [ class "form-control"
        , style [ ( "min-height", "34px" ), ( "height", "auto" ) ]
        ]
        content


teamField : Maybe TeamS -> Html msg
teamField mTeam =
    case mTeam of
        Nothing ->
            textField [ text "" ]

        Just team ->
            textField
                [ a
                    [ href <| Routes.teamPage team.id ]
                    [ text team.name ]
                ]


contactField : Maybe ContactS -> Html msg
contactField mContact =
    case mContact of
        Nothing ->
            textField [ text "" ]

        Just contact ->
            textField
                [ a
                    [ href <| Routes.contactPage contact.id ]
                    [ text contact.name ]
                ]
