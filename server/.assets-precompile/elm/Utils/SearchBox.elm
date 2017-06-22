module Utils.SearchBox
    exposing
        ( Config
        , SearchBox
        , Msg
        , init
        , update
        , getChosen
        , view
        )

import Http
import RemoteData exposing (WebData)
import Process
import Task
import Html exposing (Html, div, li, a, i, input, text)
import Html.Attributes exposing (style, class, type_, value, placeholder)
import Html.Events exposing (onInput, onBlur, onClick)


------------------------------------------------------------------
-- Types
------------------------------------------------------------------


type alias Config a =
    { renderItem : a -> String
    , renderError : Http.Error -> String
    , onSearch : String -> Cmd (WebData (List a))
    , placeholder : String
    }


type alias Model a =
    { config : Config a
    , results : WebData (List a)
    , searchText : Maybe String
    , chosen : Maybe a
    }


type SearchBox a
    = SearchBox (Model a)


type Msg a
    = Search String
    | SearchUpdate (WebData (List a))
    | Select (Maybe a)
    | Blur



------------------------------------------------------------------
-- State
------------------------------------------------------------------


init : Config a -> Maybe a -> SearchBox a
init config chosen =
    SearchBox <|
        { config = config
        , results = RemoteData.NotAsked
        , searchText = Nothing
        , chosen = chosen
        }


update : Msg a -> SearchBox a -> ( SearchBox a, Cmd (Msg a) )
update msg (SearchBox model) =
    updateUnboxed msg model
        |> Tuple.mapFirst SearchBox


updateUnboxed : Msg a -> Model a -> ( Model a, Cmd (Msg a) )
updateUnboxed msg model =
    case msg of
        Search txt ->
            ( { model
                | searchText = Just txt
                , results = RemoteData.Loading
              }
            , model.config.onSearch txt
                |> Cmd.map SearchUpdate
            )

        SearchUpdate status ->
            ( { model | results = status }, Cmd.none )

        Select mItem ->
            ( { model
                | chosen = mItem
                , searchText = Nothing
              }
            , Cmd.none
            )

        Blur ->
            model
                ! [ delay 200 (SearchUpdate RemoteData.NotAsked)
                  ]


delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


getChosen : SearchBox a -> Maybe a
getChosen (SearchBox model) =
    model.chosen



------------------------------------------------------------------
-- VIEW
------------------------------------------------------------------


view : SearchBox a -> Html (Msg a)
view (SearchBox model) =
    let
        clickableItem suggestion =
            li
                [ onClick <| Select (Just suggestion) ]
                [ a [] [ text <| model.config.renderItem suggestion ] ]

        textItem t =
            li [] [ a [] [ text t ] ]

        itemList =
            case model.results of
                RemoteData.Success values ->
                    if List.length values > 0 then
                        List.map clickableItem values
                    else
                        [ textItem "No result found" ]

                RemoteData.Failure err ->
                    [ textItem <| model.config.renderError err ]

                RemoteData.Loading ->
                    [ li
                        [ class "text-center" ]
                        [ i [ class "fa fa-circle-o-notch fa-spin" ] [] ]
                    ]

                RemoteData.NotAsked ->
                    []

        inputValue =
            case model.results of
                RemoteData.NotAsked ->
                    model.chosen
                        |> Maybe.map model.config.renderItem
                        |> Maybe.withDefault ""

                _ ->
                    case model.searchText of
                        Just txt ->
                            txt

                        Nothing ->
                            model.chosen
                                |> Maybe.map model.config.renderItem
                                |> Maybe.withDefault ""

        dropdown =
            if model.results == RemoteData.NotAsked then
                div [] []
            else
                div
                    [ class "dropdown-menu"
                    , style [ ( "display", "block" ) ]
                    ]
                    itemList
    in
        div [ style [ ( "position", "relative" ) ] ]
            [ input
                [ type_ "text"
                , class "form-control"
                , onInput Search
                , onBlur Blur
                , placeholder model.config.placeholder
                , value inputValue
                ]
                []
            , dropdown
            ]
