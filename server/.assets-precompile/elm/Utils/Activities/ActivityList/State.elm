module Utils.Activities.ActivityList.State exposing (init, update)

import Utils.Activities.ActivityList.Rest exposing (fetchActivities, deleteActivity)
import Utils.Activities.ActivityList.Types exposing (..)
import Utils.Activities.CommonTypes exposing (..)
import RemoteData exposing (WebData)
import Utils.Api as Api
import Utils.SearchBox as SearchBox
import DatePicker
import Date


init : Maybe PartialSearch -> ( ActivityList, Cmd Msg )
init mPartialSearch =
    case mPartialSearch of
        Nothing ->
            let
                activityList =
                    ActivityList
                        { deletionWarning = Nothing
                        , partialSearch = Nothing
                        , activities = []
                        , fetchStatus = RemoteData.Success []
                        , deletionStatus = RemoteData.NotAsked
                        }
            in
                activityList ! []

        Just partialSearch ->
            let
                activityList =
                    ActivityList
                        { deletionWarning = Nothing
                        , partialSearch = Just partialSearch
                        , activities = []
                        , fetchStatus = RemoteData.Loading
                        , deletionStatus = RemoteData.NotAsked
                        }
            in
                update (Fetch FreshLoad) activityList


update : Msg -> ActivityList -> ( ActivityList, Cmd Msg )
update msg (ActivityList model) =
    subUpdate msg model
        |> Tuple.mapFirst ActivityList


subUpdate : Msg -> Model -> ( Model, Cmd Msg )
subUpdate msg model =
    case msg of
        Fetch fetchType ->
            case model.partialSearch of
                Nothing ->
                    model ! []

                Just partialSearch ->
                    let
                        searchConfig =
                            case fetchType of
                                FreshLoad ->
                                    toSearch False model.activities partialSearch

                                LoadingMore ->
                                    toSearch True model.activities partialSearch
                    in
                        ( { model
                            | fetchStatus = RemoteData.Loading
                          }
                        , fetchActivities searchConfig
                            |> Cmd.map (FetchUpdate fetchType)
                        )

        FetchUpdate fetchType status ->
            case status of
                RemoteData.Success activities ->
                    { model
                      {- We leave the status here so that we can have a
                         know when tere are no more activities to load and
                         show an appropriate message in the view
                      -}
                        | fetchStatus = status
                        , activities =
                            case fetchType of
                                FreshLoad ->
                                    activities

                                LoadingMore ->
                                    model.activities ++ activities
                    }
                        ! []

                _ ->
                    { model | fetchStatus = status } ! []

        Delete activity ->
            ( { model
                | deletionStatus = RemoteData.Loading
                , deletionWarning = Nothing
              }
            , deleteActivity activity.id
                |> Cmd.map DeletionUpdate
            )

        DeletionUpdate status ->
            case status of
                RemoteData.Success _ ->
                    subUpdate
                        (Fetch FreshLoad)
                        { model | deletionStatus = status }

                _ ->
                    { model | deletionStatus = status } ! []

        WarnDeletion mActivity ->
            { model | deletionWarning = mActivity } ! []


max_results =
    20


pageCalculator maxElements elementCount =
    if rem elementCount maxElements > 0 then
        (elementCount // maxElements) + 1
    else
        (elementCount // maxElements)


toSearch : Bool -> List Activity -> PartialSearch -> Api.SearchObj
toSearch loadMore activities partialSearch =
    let
        defaultSearchObj =
            Api.defaultSearchObj

        page =
            if loadMore then
                pageCalculator max_results (List.length activities)
            else
                0
    in
        { defaultSearchObj
            | search = partialSearch.search
            , search_fields = partialSearch.search_fields
            , filters = partialSearch.filters
            , fields = []
            , embed = [ "contacts", "project", "created_by.contact", "tags" ]
            , sort = "-date"
            , max_results = max_results
            , page = page
        }
