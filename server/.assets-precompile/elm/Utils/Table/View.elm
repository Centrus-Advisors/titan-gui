module Utils.Table.View exposing (view)

import Utils.Table.Types exposing (..)
import Html exposing (Html, div, text, table, tbody, thead, td, tr, th, i, button, ul, li, a, select, option, input, span)
import Html.Attributes exposing (class, style, type_, disabled, href, selected, value, colspan)
import Html.Events exposing (onClick, on, targetValue, onInput)
import Json.Decode
import RemoteData exposing (WebData)
import Utils.Api as Api
import Utils.Ui as Ui
import List.Extra
import Utils.FlashMessages as FlashMessages


view : Table doc -> Html (Msg doc)
view (Table model) =
    case model.hiddenFilters of
        Err error ->
            FlashMessages.failure ("Error parsing filters: " ++ error)

        Ok _ ->
            div []
                [ widgetHeader model
                , widgetBody model
                , widgetBottom model
                ]


widgetHeader : Model doc -> Html (Msg doc)
widgetHeader model =
    div []
        [ span [ class "pull-left" ] [ selectMaxItems model.maxItems ]
        , renderFilters model.config.filters model.filter
        ]


widgetBottom : Model doc -> Html (Msg doc)
widgetBottom model =
    let
        oneOf =
            (\l -> l |> List.map RemoteData.toMaybe |> List.filterMap identity |> List.head)
    in
        case oneOf [ model.data, model.lastData ] of
            Just searchData ->
                pagination model.maxItems model.page searchData.totalDocuments

            Nothing ->
                text ""


widgetBody : Model doc -> Html (Msg doc)
widgetBody model =
    case model.data of
        RemoteData.Success searchData ->
            tableView model searchData

        RemoteData.Failure err ->
            Ui.submissionInfo "" model.data

        RemoteData.Loading ->
            case model.lastData of
                RemoteData.Success searchData ->
                    tableView model searchData

                _ ->
                    renderTable model (loadingBody model.maxItems model.config.columns)

        _ ->
            text "Data not requested"


tableView model searchData =
    let
        emptyBody =
            tbody
                []
                [ td
                    [ colspan (List.length model.config.columns)
                    , class "text-muted bg-muted"
                    , style [ ( "padding", ".5em" ) ]
                    ]
                    [ text "No matching records found" ]
                ]

        rows =
            List.map (renderRow model.config.columns) searchData.results

        tBody =
            if (List.length rows) > 0 then
                tbody [] rows
            else
                emptyBody
    in
        renderTable model tBody


renderTable : Model doc -> Html (Msg doc) -> Html (Msg doc)
renderTable { sortOrder, sortColumn, config } tBody =
    table
        [ class "table table-striped table-bordered dt-responsive nowrap dataTable no-footer dtr-inline m-t-5"
        , style [ ( "table-layout", "fixed" ) ]
        ]
        [ thead []
            [ tr [] (List.map (headerCell sortOrder sortColumn) config.columns)
            ]
        , tBody
        ]


headerCell : SortOrder -> Maybe (ColumnConfig doc) -> ColumnConfig doc -> Html (Msg doc)
headerCell sortOrder sortColumn colInfo =
    let
        iconClass =
            if sortColumn == Just colInfo then
                case sortOrder of
                    Asc ->
                        iconAscending

                    Desc ->
                        iconDescending
            else
                iconUnsorted
    in
        th
            [ onClick <| SortBy colInfo
            , style [ ( "position", "relative " ) ]
            ]
            [ text colInfo.title
            , i
                [ class iconClass
                , style
                    [ ( "position", "absolute" )
                    , ( "top", "11px" )
                    , ( "right", "8px" )
                    , ( "display", "block" )
                    , ( "opacity", "0.3" )
                    , ( "font-weight", "bold" )
                    ]
                ]
                []
            ]


renderRow : List (ColumnConfig doc) -> doc -> Html (Msg doc)
renderRow columns doc =
    tr [] <| List.map (renderCell doc) columns


renderCell : doc -> ColumnConfig doc -> Html (Msg doc)
renderCell doc col =
    td [] [ col.renderCell doc ]


iconAscending =
    "glyphicon glyphicon-sort-by-attributes"


iconDescending =
    "glyphicon glyphicon-sort-by-attributes-alt"


iconUnsorted =
    "glyphicon glyphicon-sort"


pagination : Int -> Int -> Int -> Html (Msg doc)
pagination maxItems currentPage totalDocuments =
    let
        pageLink =
            (\n ->
                li
                    [ if currentPage == n then
                        class "active"
                      else
                        class ""
                    , onClick (SetPage n)
                    ]
                    [ a [ href "#" ] [ text <| toString <| n + 1 ] ]
            )

        lastPage =
            (totalDocuments - 1) // maxItems

        pageNumbers =
            [ currentPage - 1, currentPage, currentPage + 1 ]
                |> List.filter (\v -> 0 <= v && v <= lastPage)

        firstPageLink =
            pageLink 0

        previous =
            li
                [ onClick (SetPage (currentPage - 1))
                , disabled (currentPage == 0)
                ]
                [ a [ href "#" ] [ text "Previous" ] ]

        previousElipsis =
            li
                [ disabled True ]
                [ a [ href "#" ] [ text "..." ] ]

        next =
            li
                [ onClick (SetPage (currentPage + 1))
                , disabled (currentPage == lastPage)
                ]
                [ a [ href "#" ] [ text "Next" ] ]

        nextElipsis =
            li
                [ disabled True ]
                [ a [ href "#" ] [ text "..." ] ]

        lastPageLink =
            pageLink lastPage

        buttons =
            [ previous ]
                ++ (if List.member 0 pageNumbers then
                        []
                    else
                        [ firstPageLink
                        , previousElipsis
                        ]
                   )
                ++ (List.map pageLink pageNumbers)
                ++ (if List.member lastPage pageNumbers then
                        []
                    else
                        [ nextElipsis, lastPageLink ]
                   )
                ++ [ next ]
    in
        div [ class "text-right" ]
            [ ul [ class "pagination" ] buttons ]


selectMaxItems maxItems =
    let
        values =
            [ 10, 25, 50, 100 ]
    in
        div []
            [ text "Show"
            , select
                [ class "form-control m-l-5 m-r-5"
                , style [ ( "display", "inline-block" ), ( "width", "auto" ) ]
                , on "change" (Json.Decode.map (String.toInt >> Result.withDefault maxItems >> SetMaxItems) targetValue)
                ]
                (List.map (\v -> option [ selected <| maxItems == v ] [ text <| toString v ]) values)
            , text "items"
            ]


loadingBody maxItems columns =
    let
        row =
            (\rowNo a ->
                tr [] <|
                    List.indexedMap (\idx a -> td [] [ placeholderText (idx + rowNo) ]) columns
            )
    in
        tbody [] (List.repeat maxItems () |> List.indexedMap row)


placeholderText idx =
    let
        w =
            if (idx % 5) == 0 then
                10
            else if (idx % 4) == 0 then
                7
            else if (idx % 3) == 0 then
                9
            else if (idx % 2) == 0 then
                8
            else
                6
    in
        div
            [ style
                [ ( "animation", "elmTablePulse .5s infinite alternate" )
                , ( "height", "1.5em" )
                , ( "width", (toString w) ++ "0%" )
                ]
            ]
            []


renderFilters : List FilterConfig -> Maybe FilterConfig -> Html (Msg doc)
renderFilters filters mChosenFilter =
    let
        mChosen =
            case mChosenFilter of
                Just v ->
                    Just v

                Nothing ->
                    List.head filters
    in
        case mChosen of
            Nothing ->
                text ""

            Just chosenFilter ->
                div [ class "text-right" ]
                    [ filterOptions filters chosenFilter
                    , filterInput chosenFilter
                    ]


filterOptions : List FilterConfig -> FilterConfig -> Html (Msg doc)
filterOptions filters chosenFilter =
    div [ style [ ( "display", "inline" ) ] ] <|
        if List.length filters < 2 then
            [ text "Search" ]
        else
            let
                selectFilter =
                    (\name ->
                        filters
                            |> List.Extra.find (\f -> filterName f == name)
                            |> Maybe.withDefault chosenFilter
                            |> SetFilter
                    )
            in
                [ text "Search by"
                , select
                    [ class "form-control m-l-10"
                    , style
                        [ ( "display", "inline-block" )
                        , ( "width", "auto" )
                        , ( "position", "relative" )
                        , ( "top", "2px" )
                        ]
                    , on "change" (Json.Decode.map selectFilter targetValue)
                    ]
                    (List.map (renderFilterOption chosenFilter) filters)
                ]


filterName : FilterConfig -> String
filterName filter =
    case filter of
        TextFilter f ->
            f.name

        DateFilter f ->
            f.name


renderFilterOption chosenFilter filter =
    option
        [ if filterName chosenFilter == filterName filter then
            selected True
          else
            selected False
        , value (filterName filter)
        ]
        [ text (filterName filter) ]


filterInput chosenFilter =
    div
        [ class "m-l-10"
        , style [ ( "width", "auto" ), ( "display", "inline-block" ) ]
        ]
        [ case chosenFilter of
            TextFilter filter ->
                Ui.inputField "Type to Search" filter.value (\v -> SetFilter (TextFilter { filter | value = v }))

            DateFilter a ->
                text "Not implemented"
        ]
