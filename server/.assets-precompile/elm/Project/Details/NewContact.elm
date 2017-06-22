module Project.Details.NewContact exposing (Model, Msg, empty, update, view)

import Common.Types exposing (CompanyS, ContactS)
import Common.Decoders
import RemoteData exposing (WebData)
import Utils.SearchBox as SearchBox
import Utils.Http
import Http
import Utils.Api as Api
import Utils.Ui as Ui
import Utils.Routes as Routes
import Json.Encode
import Task
import Html exposing (Html, text, div, button, input, label)
import Html.Attributes exposing (class, style, type_, placeholder, value)
import Html.Events exposing (onInput, onClick)


--------------------------------------------------
------- Types
--------------------------------------------------


type Msg
    = Name String
    | Email String
    | CCompany (SearchBox.Msg CompanyS)
    | Create
    | CreateUpdate (WebData ContactS)
    | Cancel


type alias Model =
    { name : String
    , email : String
    , company : SearchBox.SearchBox CompanyS
    , submission : WebData ContactS
    }


type alias Config msg =
    { onUpdate : Msg -> msg
    , onCreated : ContactS -> msg
    , onCancel : msg
    }



--------------------------------------------------
------- Update
--------------------------------------------------


empty : Model
empty =
    { name = ""
    , email = ""
    , company = SearchBox.init companySearchBoxConfig Nothing
    , submission = RemoteData.NotAsked
    }


companySearchBoxConfig : SearchBox.Config CompanyS
companySearchBoxConfig =
    { renderItem = .name
    , renderError = Utils.Http.errorMessage
    , onSearch = searchCompanies
    , placeholder = "Company name"
    }


update : Config msg -> Msg -> Model -> ( Model, Cmd msg )
update config msg model =
    case msg of
        Name val ->
            { model | name = val } ! []

        Email val ->
            { model | email = val } ! []

        CCompany subMsg ->
            SearchBox.update subMsg model.company
                |> Tuple.mapSecond (Cmd.map (CCompany >> config.onUpdate))
                |> Tuple.mapFirst (\c -> { model | company = c })

        Create ->
            model
                ! [ createContact model
                        |> Cmd.map CreateUpdate
                        |> Cmd.map config.onUpdate
                  ]

        CreateUpdate status ->
            case status of
                RemoteData.Success contact ->
                    { model | submission = status }
                        ! [ config.onCreated contact
                                |> toCmd
                          ]

                _ ->
                    { model | submission = status } ! []

        Cancel ->
            model ! [ toCmd config.onCancel ]


toCmd a =
    Task.perform (always a) (Task.succeed ())



--------------------------------------------------
------- Rest
--------------------------------------------------


createContact : Model -> Cmd (WebData ContactS)
createContact model =
    let
        content =
            { fields = []
            , embed = []
            }

        body =
            encodeNewContact model

        endpoint =
            Routes.contactApi ""

        decoder =
            Common.Decoders.contactS
    in
        Api.create decoder content endpoint body
            |> Utils.Http.attemptWithRemoteData


encodeNewContact : Model -> Http.Body
encodeNewContact model =
    let
        fields =
            [ ( "name", Json.Encode.string model.name )
            , ( "email", Json.Encode.string model.email )
            , ( "company", encodeSearchBox model.company )
            ]
    in
        Utils.Http.toJsonBody fields


encodeSearchBox a =
    SearchBox.getChosen a
        |> Maybe.map .id
        |> Maybe.map Json.Encode.string
        |> Maybe.withDefault Json.Encode.null


searchCompanies : String -> Cmd (WebData (List CompanyS))
searchCompanies searchString =
    let
        config =
            { search = searchString
            , search_fields = [ "name" ]
            , fields = []
            , embed = []
            , sort = "name"
            , max_results = 5
            , page = 0
            , filters = []
            }
    in
        Api.search Common.Decoders.companyS config (Routes.companyApi "")
            |> Utils.Http.attemptWithRemoteData



--------------------------------------------------
------- View
--------------------------------------------------


view : Model -> Html Msg
view model =
    Ui.modal
        [ Ui.modalHeader "New Contact" Cancel
        , Ui.modalBody
            [ Ui.submissionInfo "Contact created" model.submission
            , div
                [ class "row" ]
                [ div
                    [ class "col-md-6" ]
                    [ title "Name"
                        [ inputField "Contact Name" model.name Name
                        ]
                    ]
                , div
                    [ class "col-md-6" ]
                    [ title "Email"
                        [ inputField "Email address" model.email Email
                        ]
                    ]
                ]
            , title "Company"
                [ SearchBox.view model.company
                    |> Html.map CCompany
                ]
            ]
        , Ui.modalFooter
            [ button
                [ class "btn btn-default waves-effect"
                , onClick Cancel
                ]
                [ text "Cancel" ]
            , button
                [ class "btn btn-primary waves-effect waves-light"
                , onClick Create
                ]
                [ text "Create Contact" ]
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


title : String -> List (Html Msg) -> Html Msg
title txt content =
    div [ class "form-group" ]
        ([ label [] [ text txt ] ]
            ++ content
        )
