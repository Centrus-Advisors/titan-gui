module Utils.Activities.Filters.State exposing (init, update, getPartialSearch)

import Utils.Activities.Filters.Types exposing (..)
import Utils.Activities.CommonTypes exposing (..)
import Common.Types exposing (ContactS)
import DatePicker
import Date exposing (Date)
import Utils.SearchBox as SearchBox
import Utils.Activities.Rest
    exposing
        ( fetchContact
        , fetchProject
        , searchContacts
        , searchProjects
        , searchActivityTag
        , createActivity
        , createActivityTag
        )
import Utils.Http
import Utils.Time exposing (weekDayDayMonthYear)
import Date.Extra


init : SourceID -> Date -> Filters
init sourceID todayDate =
    Filters
        { sourceID = sourceID
        , todayDate = todayDate
        , textSearch = ""
        , filter = NoFilter
        }


update : Msg -> Filters -> ( Filters, Cmd Msg )
update msg (Filters model) =
    subUpdate msg model
        |> Tuple.mapFirst Filters


subUpdate : Msg -> Model -> ( Model, Cmd Msg )
subUpdate msg model =
    case msg of
        TextUpdate txt ->
            { model | textSearch = txt } ! []

        DateUpdate subMsg ->
            case model.filter of
                DateFilter picker ->
                    let
                        ( newPicker, pickerMsg, _ ) =
                            DatePicker.update subMsg picker
                    in
                        ( { model | filter = DateFilter newPicker }
                        , pickerMsg
                            |> Cmd.map DateUpdate
                        )

                _ ->
                    model ! []

        TagUpdate subMsg ->
            case model.filter of
                TagFilter searchBox ->
                    let
                        ( newSearchBox, boxCmd ) =
                            SearchBox.update subMsg searchBox
                                |> Tuple.mapSecond (Cmd.map TagUpdate)
                    in
                        { model | filter = TagFilter newSearchBox } ! [ boxCmd ]

                _ ->
                    model ! []

        ContactUpdate subMsg ->
            case model.filter of
                ContactFilter searchBox ->
                    let
                        ( newSearchBox, boxCmd ) =
                            SearchBox.update subMsg searchBox
                                |> Tuple.mapSecond (Cmd.map ContactUpdate)
                    in
                        { model | filter = ContactFilter newSearchBox } ! [ boxCmd ]

                _ ->
                    model ! []

        SelectFilter filterName ->
            let
                newFilter =
                    if filterName == "Date" then
                        Just model.todayDate
                            |> datePickerSettings
                            |> DatePicker.init
                            |> Tuple.first
                            |> DateFilter
                    else if filterName == "Tag" then
                        SearchBox.init tagSearchConfig Nothing
                            |> TagFilter
                    else if filterName == "Contact" then
                        SearchBox.init contactSearchConfig Nothing
                            |> ContactFilter
                    else
                        NoFilter
            in
                { model | filter = newFilter } ! []


getPartialSearch : Filters -> Maybe PartialSearch
getPartialSearch (Filters model) =
    let
        sourceFilter =
            case model.sourceID of
                ProjectID id ->
                    [ ( "project", id ) ]

                ContactID id ->
                    [ ( "contacts", id ) ]

                CompanyID id ->
                    [ ( "company", id ) ]

                UserID id ->
                    [ ( "created_by", id ) ]

                NoSource ->
                    []

        additionalFilter =
            case model.filter of
                NoFilter ->
                    []

                DateFilter picker ->
                    DatePicker.getDate picker
                        |> Maybe.map (\d -> [ ( "date", (++) "<" <| toString <| Date.toTime d ) ])
                        |> Maybe.withDefault []

                TagFilter searchBox ->
                    SearchBox.getChosen searchBox
                        |> Maybe.map (\t -> [ ( "tags", t.id ) ])
                        |> Maybe.withDefault []

                ContactFilter searchBox ->
                    SearchBox.getChosen searchBox
                        |> Maybe.map (\c -> [ ( "contacts", c.id ) ])
                        |> Maybe.withDefault []
    in
        case model.sourceID of
            NoSource ->
                Nothing

            _ ->
                Just
                    { search = model.textSearch
                    , search_fields = [ "description", "category" ]
                    , filters = sourceFilter ++ additionalFilter
                    }


datePickerSettings : Maybe Date -> DatePicker.Settings
datePickerSettings pickedDate =
    let
        defaultSettings =
            DatePicker.defaultSettings
    in
        { defaultSettings
            | pickedDate = pickedDate
            , inputClassList = [ ( "form-control", True ) ]
            , dateFormatter = weekDayDayMonthYear
        }


contactSearchConfig : SearchBox.Config ContactS
contactSearchConfig =
    { renderItem = .name
    , renderError = Utils.Http.errorMessage
    , onSearch = searchContacts
    , placeholder = "Contact name"
    }


tagSearchConfig : SearchBox.Config ActivityTag
tagSearchConfig =
    { renderItem = .name
    , renderError = Utils.Http.errorMessage
    , onSearch = searchActivityTag
    , placeholder = "Tag name"
    }
