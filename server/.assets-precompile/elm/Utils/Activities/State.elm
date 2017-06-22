module Utils.Activities.State
    exposing
        ( initFromProject
        , initFromCompany
        , initFromContact
        , initFromUser
        , initEmpty
        , update
        )

import Utils.Activities.CommonTypes exposing (..)
import Utils.Activities.Types exposing (..)
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
import Common.Types exposing (ProjectS, ContactS)
import Utils.Api as Api exposing (SearchObj, defaultSearchObj)
import RemoteData exposing (RemoteData)
import Utils.SearchBox as SearchBox
import Utils.Http
import Date exposing (Date)
import Date.Extra
import Time exposing (Time)
import List.Extra
import Utils.Activities.ActivityList.Main as ActivityList
import Utils.Activities.Filters.Main as Filters
import DatePicker
import Utils.Time exposing (weekDayDayMonthYear)


-- INIT


initFromProject : Time -> String -> ( Activities, Cmd Msg )
initFromProject todayTime id =
    init todayTime <| ProjectID id


initFromCompany : Time -> String -> ( Activities, Cmd Msg )
initFromCompany todayTime id =
    init todayTime <| CompanyID id


initFromContact : Time -> String -> ( Activities, Cmd Msg )
initFromContact todayTime id =
    init todayTime <| ContactID id


initFromUser : Time -> String -> ( Activities, Cmd Msg )
initFromUser todayTime id =
    init todayTime <| UserID id


initEmpty : Time -> ( Activities, Cmd Msg )
initEmpty todayTime =
    init todayTime NoSource


init : Time -> SourceID -> ( Activities, Cmd Msg )
init todayTime sourceID =
    let
        filters =
            Filters.init sourceID (Date.fromTime todayTime)

        partialSearch =
            Filters.getPartialSearch filters

        ( activityList, activityListMsg ) =
            ActivityList.init partialSearch

        model =
            { activityList = activityList
            , filters = filters
            , sourceID = sourceID
            , sourceDocument = RemoteData.Loading
            , todayDate = Date.fromTime todayTime
            , newActivity = Nothing
            , newActivitySubmission = RemoteData.NotAsked
            , newTag = Nothing
            , newTagSubmission = RemoteData.NotAsked
            }
    in
        Activities model
            ! [ activityListMsg
                    |> Cmd.map ActivityListMsg
              , fetchSourceDocument sourceID
              ]



-- UPDATE


update : Msg -> Activities -> ( Activities, Cmd Msg )
update msg (Activities model) =
    subUpdate msg model
        |> Tuple.mapFirst Activities


subUpdate : Msg -> Model -> ( Model, Cmd Msg )
subUpdate msg model =
    case msg of
        ActivityListMsg subMsg ->
            ActivityList.update subMsg model.activityList
                |> Tuple.mapFirst (\l -> { model | activityList = l })
                |> Tuple.mapSecond (Cmd.map ActivityListMsg)

        FiltersMsg subMsg ->
            let
                ( filters, filtersCmd ) =
                    Filters.update subMsg model.filters

                newPartialSearch =
                    Filters.getPartialSearch filters

                oldPartialSearch =
                    Filters.getPartialSearch model.filters

                newModel =
                    { model | filters = filters }
            in
                if newPartialSearch /= oldPartialSearch then
                    setSearch newModel
                        |> Tuple.mapSecond (\cmd -> Cmd.batch [ cmd, Cmd.map FiltersMsg filtersCmd ])
                else
                    ( { model | filters = filters }, Cmd.map FiltersMsg filtersCmd )

        CancelNewActivity ->
            { model | newActivity = Nothing } ! []

        CreateNewActivity ->
            case model.sourceDocument of
                RemoteData.Success sourceDocument ->
                    ( { model
                        | newActivity = Just <| emptyNewActivity model.todayDate sourceDocument
                        , newActivitySubmission = RemoteData.NotAsked
                      }
                    , Cmd.none
                    )

                RemoteData.Loading ->
                    model ! []

                _ ->
                    model ! [ fetchSourceDocument model.sourceID ]

        NewActivity subMsg ->
            case model.newActivity of
                Nothing ->
                    model ! []

                Just newActivity ->
                    updateNewActivity subMsg newActivity
                        |> Tuple.mapFirst (\n -> { model | newActivity = Just n })
                        |> Tuple.mapSecond (Cmd.map NewActivity)

        NewActivitySubmit ->
            ( { model | newActivitySubmission = RemoteData.Loading }
            , model.newActivity
                |> Maybe.map createActivity
                |> Maybe.withDefault Cmd.none
                |> Cmd.map NewActivitySubmissionUpdate
            )

        NewActivitySubmissionUpdate status ->
            case status of
                RemoteData.Success _ ->
                    let
                        newModel =
                            { model | newActivitySubmission = status }
                    in
                        setSearch newModel

                _ ->
                    { model | newActivitySubmission = status } ! []

        NewTagCreate ->
            { model | newTag = Just "" } ! []

        NewTagCancel ->
            { model
                | newTag = Nothing
                , newTagSubmission = RemoteData.NotAsked
            }
                ! []

        NewTagSubmit ->
            ( { model | newTagSubmission = RemoteData.Loading }
            , model.newTag
                |> Maybe.map createActivityTag
                |> Maybe.withDefault Cmd.none
                |> Cmd.map NewTagSubmissionUpdate
            )

        NewTagChange txt ->
            { model | newTag = Just txt } ! []

        NewTagSubmissionUpdate status ->
            case status of
                RemoteData.Success aTag ->
                    case model.newActivity of
                        Nothing ->
                            { model
                                | newTag = Nothing
                                , newTagSubmission = RemoteData.NotAsked
                            }
                                ! []

                        Just activity ->
                            let
                                newActivity =
                                    { activity | tags = [ aTag ] ++ activity.tags }
                            in
                                ( { model
                                    | newTag = Nothing
                                    , newTagSubmission = RemoteData.NotAsked
                                    , newActivity = Just newActivity
                                  }
                                , Cmd.none
                                )

                _ ->
                    { model | newTagSubmission = status } ! []

        SourceDocumentUpdate status ->
            { model | sourceDocument = status } ! []


updateNewActivity : NewActivityMsg -> EditableActivity -> ( EditableActivity, Cmd NewActivityMsg )
updateNewActivity msg activity =
    case msg of
        ChangeDate subMsg ->
            DatePicker.update subMsg activity.date
                |> toTwoTuple
                |> Tuple.mapFirst (\newPicker -> { activity | date = newPicker })
                |> Tuple.mapSecond (Cmd.map ChangeDate)

        ChangeDescription txt ->
            ( { activity | description = txt }, Cmd.none )

        ChangeCategory txt ->
            if txt == "Phone Call" then
                ( { activity | category = PhoneCall }, Cmd.none )
            else if txt == "Meeting" then
                ( { activity | category = Meeting }, Cmd.none )
            else if txt == "Email" then
                ( { activity | category = Email }, Cmd.none )
            else
                ( { activity | category = Other }, Cmd.none )

        SearchTag subMsg ->
            let
                ( tagSearch, cmd ) =
                    SearchBox.update subMsg activity.tagSearch
                        |> Tuple.mapSecond (Cmd.map SearchTag)
            in
                case SearchBox.getChosen tagSearch of
                    Nothing ->
                        ( { activity | tagSearch = tagSearch }
                        , cmd
                        )

                    Just aTag ->
                        ( { activity
                            | tagSearch = SearchBox.init tagSearchConfig Nothing
                            , tags =
                                activity.tags
                                    |> (++) [ aTag ]
                                    |> List.Extra.uniqueBy .id
                          }
                        , cmd
                        )

        SearchContact subMsg ->
            let
                ( contactSearch, cmd ) =
                    SearchBox.update subMsg activity.contactSearch
                        |> Tuple.mapSecond (Cmd.map SearchContact)
            in
                case SearchBox.getChosen contactSearch of
                    Nothing ->
                        ( { activity | contactSearch = contactSearch }
                        , cmd
                        )

                    Just aContact ->
                        ( { activity
                            | contactSearch = SearchBox.init contactSearchConfig Nothing
                            , contacts =
                                activity.contacts
                                    |> (++) [ aContact ]
                                    |> List.Extra.uniqueBy .id
                          }
                        , cmd
                        )

        SearchProject subMsg ->
            SearchBox.update subMsg activity.project
                |> Tuple.mapSecond (Cmd.map SearchProject)
                |> Tuple.mapFirst (\s -> { activity | project = s })

        RemoveContact contact ->
            let
                contacts =
                    List.Extra.filterNot (\{ name } -> name == contact.name) activity.contacts
            in
                { activity | contacts = contacts } ! []

        RemoveTag tag ->
            let
                tags =
                    List.Extra.filterNot (\{ name } -> name == tag.name) activity.tags
            in
                { activity | tags = tags } ! []


setSearch : Model -> ( Model, Cmd Msg )
setSearch model =
    ActivityList.init (Filters.getPartialSearch model.filters)
        |> Tuple.mapFirst (\l -> { model | activityList = l })
        |> Tuple.mapSecond (Cmd.map ActivityListMsg)


emptyNewActivity : Date -> SourceDocument -> EditableActivity
emptyNewActivity todayDate sourceDocument =
    let
        ( picker, _ ) =
            Just todayDate
                |> datePickerSettings
                |> DatePicker.init

        contacts =
            case sourceDocument of
                ContactDoc contact ->
                    [ contact ]

                _ ->
                    []

        project =
            case sourceDocument of
                ProjectDoc proj ->
                    Just proj

                _ ->
                    Nothing
    in
        { sourceDocument = sourceDocument
        , date = picker
        , description = ""
        , category = Other
        , tags = []
        , tagSearch = SearchBox.init tagSearchConfig Nothing
        , contacts = contacts
        , contactSearch = SearchBox.init contactSearchConfig Nothing
        , project = SearchBox.init projectSearchConfig project
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


projectSearchConfig : SearchBox.Config ProjectS
projectSearchConfig =
    { renderItem = .name
    , renderError = Utils.Http.errorMessage
    , onSearch = searchProjects
    , placeholder = "Project name"
    }


tagSearchConfig : SearchBox.Config ActivityTag
tagSearchConfig =
    { renderItem = .name
    , renderError = Utils.Http.errorMessage
    , onSearch = searchActivityTag
    , placeholder = "Tag name"
    }


chosenChanged : SearchBox.SearchBox a -> SearchBox.SearchBox a -> Bool
chosenChanged s1 s2 =
    SearchBox.getChosen s1 /= SearchBox.getChosen s2


dateChanged : DatePicker.DatePicker -> DatePicker.DatePicker -> Bool
dateChanged d1 d2 =
    DatePicker.getDate d1 /= DatePicker.getDate d2


toTwoTuple ( a, b, c ) =
    ( a, b )


fetchSourceDocument : SourceID -> Cmd Msg
fetchSourceDocument sourceID =
    case sourceID of
        -- You can't create activities against companies, and you only
        -- need the source document for creating a new activity.
        CompanyID _ ->
            Cmd.none

        UserID _ ->
            Cmd.none

        ProjectID id ->
            fetchProject id
                |> Cmd.map (RemoteData.map ProjectDoc)
                |> Cmd.map SourceDocumentUpdate

        ContactID id ->
            fetchContact id
                |> Cmd.map (RemoteData.map ContactDoc)
                |> Cmd.map SourceDocumentUpdate

        NoSource ->
            Cmd.none
