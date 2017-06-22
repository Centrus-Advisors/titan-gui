port module Utils.Table.State exposing (init, update, textFilter, subscriptions)

import Utils.Table.Types exposing (..)
import RemoteData
import Utils.Api as Api
import List.Extra
import Json.Decode


textFilter : String -> (String -> List ( String, String )) -> FilterConfig
textFilter name applyFilter =
    TextFilter
        { name = name
        , applyFilter = applyFilter
        , value = ""
        }


init : Config doc -> Json.Decode.Value -> ( Table doc, Cmd (Msg doc) )
init config jValue =
    let
        model =
            { page = 0
            , maxItems = 25
            , sortOrder = Asc
            , sortColumn = List.Extra.find (\c -> c.fieldName == config.defaultSortColumn) config.columns
            , searchText = ""
            , searchColumn = List.head config.columns
            , hiddenFilters = (decodeTupleList jValue)
            , lastRequestID = 0
            , config = config
            , data = RemoteData.Loading
            , lastData = RemoteData.Loading
            , filter = List.head config.filters
            }
    in
        fetchData model
            |> Tuple.mapFirst Table



-- SUBSCRIPTION


port setHiddenFilters : (Json.Decode.Value -> msg) -> Sub msg


subscriptions model =
    setHiddenFilters (decodeTupleList >> SetHiddenFilters)


decodeTupleList : Json.Decode.Value -> Result String (List ( String, String ))
decodeTupleList jvalue =
    jvalue
        |> Json.Decode.decodeValue (Json.Decode.keyValuePairs filterValueDecoder)


filterValueDecoder : Json.Decode.Decoder String
filterValueDecoder =
    Json.Decode.oneOf
        [ Json.Decode.string
        , Json.Decode.list Json.Decode.string |> Json.Decode.map (List.intersperse "," >> List.foldl (++) "")
        ]



-- UPDATE


update : Msg doc -> Table doc -> ( Table doc, Cmd (Msg doc) )
update msg (Table model) =
    subUpdate msg model
        |> Tuple.mapFirst Table


subUpdate : Msg doc -> Model doc -> ( Model doc, Cmd (Msg doc) )
subUpdate msg model =
    case msg of
        SetMaxItems v ->
            fetchData { model | maxItems = v }

        SetPage newPage ->
            if newPage == model.page then
                model ! []
            else
                fetchData { model | page = max 0 newPage }

        DataFetch requestID status ->
            if requestID /= model.lastRequestID then
                model ! []
            else
                { model | data = status } ! []

        SortBy colInfo ->
            if model.sortColumn == Just colInfo then
                fetchData
                    { model
                        | sortOrder = ivertOrder model.sortOrder
                        , page = 0
                    }
            else
                fetchData
                    { model
                        | sortColumn = Just colInfo
                        , sortOrder = Asc
                        , page = 0
                    }

        SetFilter filterConfig ->
            if model.filter == Just filterConfig then
                model ! []
            else
                fetchData
                    { model
                        | page = 0
                        , sortOrder = Asc
                        , filter = Just filterConfig
                    }

        SetHiddenFilters hiddenFilters ->
            if model.hiddenFilters == hiddenFilters then
                model ! []
            else
                fetchData
                    { model | hiddenFilters = hiddenFilters }


fetchData : Model doc -> ( Model doc, Cmd (Msg doc) )
fetchData model =
    let
        newModel =
            { model
                | lastRequestID = model.lastRequestID + 1
                , data = RemoteData.Loading
                , lastData = model.data
            }
    in
        newModel
            ! [ toApiSearch newModel
                    |> model.config.search
                    |> Cmd.map (DataFetch newModel.lastRequestID)
              ]


toApiSearch : Model doc -> Api.SearchObj
toApiSearch model =
    let
        search_fields =
            model.searchColumn
                |> Maybe.map .fieldName
                |> Maybe.map (\v -> [ v ])
                |> Maybe.withDefault []

        sortField =
            model.sortColumn
                |> Maybe.map .fieldName
                |> Maybe.withDefault ""

        sort =
            case model.sortOrder of
                Asc ->
                    sortField

                Desc ->
                    "-" ++ sortField

        filters =
            (Result.withDefault [] model.hiddenFilters)
                ++ case model.filter of
                    Nothing ->
                        []

                    Just filterConfig ->
                        case filterConfig of
                            TextFilter f ->
                                f.applyFilter f.value

                            DateFilter f ->
                                f.applyFilter f.value
    in
        { search = model.searchText
        , search_fields = search_fields
        , fields = []
        , embed = []
        , sort = sort
        , max_results = model.maxItems
        , page = model.page
        , filters = filters
        }


ivertOrder : SortOrder -> SortOrder
ivertOrder order =
    case order of
        Asc ->
            Desc

        Desc ->
            Asc
