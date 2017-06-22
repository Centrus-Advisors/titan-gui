module TeamDetails.State exposing (init, update)

import TeamDetails.Types exposing (..)
import TeamDetails.Headers
import TeamDetails.Rest
    exposing
        ( fetchTeam
        , fetchTeamMembers
        , saveTeam
        , searchTeamUser
        , searchAnyUser
        , makeNewMember
        , removeMember
        , deleteTeam
        )
import RemoteData exposing (WebData)
import Utils.SearchBox as SearchBox
import Utils.Http
import Task


init : { teamId : String } -> ( Model, Cmd Msg )
init { teamId } =
    update (Reload teamId)
        { team = RemoteData.Loading
        , members = RemoteData.Loading
        , editable = Nothing
        , submission = RemoteData.NotAsked
        , deleteTeamSubmission = RemoteData.NotAsked
        , canBeEdited = False
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetEditing on ->
            case ( on, model.team, model.members ) of
                ( True, RemoteData.Success team, RemoteData.Success members ) ->
                    let
                        editable =
                            { id = team.id
                            , name = team.name
                            , leader = SearchBox.init (teamUserSearchConfig team.id) team.leader
                            , members = RemoteData.Success members
                            , newMember = SearchBox.init (anyUserSearchConfig team.id) Nothing
                            , newMemberSubmission = RemoteData.NotAsked
                            , removeMemberSubmission = RemoteData.NotAsked
                            , confirmDeleteModalShowing = False
                            }
                    in
                        { model
                            | editable = Just editable
                        }
                            ! []

                _ ->
                    { model
                        | editable = Nothing
                        , submission = RemoteData.NotAsked
                    }
                        ! []

        SaveEditing ->
            { model | submission = RemoteData.Loading }
                ! [ model.editable
                        |> Maybe.map saveTeam
                        |> Maybe.withDefault Cmd.none
                        |> Cmd.map TeamSaveUpdate
                  ]

        TeamSaveUpdate status ->
            case status of
                RemoteData.Success newTeam ->
                    { model
                        | submission = status
                        , team = status
                        , editable = Nothing
                    }
                        ! [ TeamDetails.Headers.set newTeam
                          ]

                _ ->
                    { model | submission = status }
                        ! []

        TeamEditing subMsg ->
            case model.editable of
                Nothing ->
                    model ! []

                Just editable ->
                    teamEditingUpdate subMsg editable
                        |> Tuple.mapFirst (\e -> { model | editable = Just e })

        TeamFetchUpdate status ->
            case status of
                RemoteData.Success ( team, canBeEdited ) ->
                    { model
                        | team = RemoteData.Success team
                        , canBeEdited = canBeEdited
                    }
                        ! [ TeamDetails.Headers.set team ]

                RemoteData.Loading ->
                    { model | team = RemoteData.Loading } ! []

                RemoteData.NotAsked ->
                    { model | team = RemoteData.NotAsked } ! []

                RemoteData.Failure err ->
                    { model | team = RemoteData.Failure err } ! []

        TeamMembersFetchUpdate status ->
            { model
                | members = status
                , editable = Maybe.map (\e -> { e | members = status }) model.editable
            }
                ! []

        Reload teamId ->
            model
                ! [ fetchTeam teamId
                        |> Cmd.map TeamFetchUpdate
                  , fetchTeamMembers teamId
                        |> Cmd.map TeamMembersFetchUpdate
                  ]

        DeleteTeam ->
            case model.editable of
                Just editable ->
                    { model
                        | deleteTeamSubmission = RemoteData.Loading
                        , editable = Nothing
                    }
                        ! [ deleteTeam editable
                                |> Cmd.map DeleteTeamUpdate
                          ]

                Nothing ->
                    model ! []

        DeleteTeamUpdate status ->
            case status of
                RemoteData.Success _ ->
                    { model | deleteTeamSubmission = status }
                        ! [ case model.team of
                                RemoteData.Success team ->
                                    toCmd <| Reload team.id

                                _ ->
                                    Cmd.none
                          ]

                _ ->
                    { model | deleteTeamSubmission = status } ! []


teamEditingUpdate : TeamEditingMsg -> EditableTeam -> ( EditableTeam, Cmd Msg )
teamEditingUpdate msg editable =
    case msg of
        ChangeName val ->
            { editable | name = val } ! []

        SearchLeader subMsg ->
            SearchBox.update subMsg editable.leader
                |> Tuple.mapFirst (\b -> { editable | leader = b })
                |> Tuple.mapSecond (Cmd.map (SearchLeader >> TeamEditing))

        SearchNewMember subMsg ->
            SearchBox.update subMsg editable.newMember
                |> Tuple.mapFirst (\b -> { editable | newMember = b })
                |> Tuple.mapSecond (Cmd.map (SearchNewMember >> TeamEditing))

        ChooseNewMember ->
            case SearchBox.getChosen editable.newMember of
                Just aUser ->
                    ( { editable | newMemberSubmission = RemoteData.Loading }
                    , makeNewMember editable.id aUser.id
                        |> Cmd.map (NewMemberUpdate >> TeamEditing)
                    )

                Nothing ->
                    editable ! []

        NewMemberUpdate status ->
            case status of
                RemoteData.Success _ ->
                    { editable
                        | newMember = SearchBox.init (anyUserSearchConfig editable.id) Nothing
                    }
                        ! [ toCmd <| Reload editable.id ]

                _ ->
                    { editable | newMemberSubmission = status } ! []

        RemoveMember aUser ->
            ( { editable | removeMemberSubmission = RemoteData.Loading }
            , removeMember aUser
                |> Cmd.map (RemoveMemberUpdate >> TeamEditing)
            )

        RemoveMemberUpdate status ->
            case status of
                RemoteData.Success _ ->
                    editable ! [ toCmd <| Reload editable.id ]

                _ ->
                    { editable | removeMemberSubmission = status } ! []

        TeamMembersReloadUpdate status ->
            { editable | members = status } ! []

        ToggleDeletionModal ->
            { editable | confirmDeleteModalShowing = not editable.confirmDeleteModalShowing } ! []


teamUserSearchConfig : String -> SearchBox.Config User
teamUserSearchConfig teamId =
    { renderItem = .email
    , renderError = Utils.Http.errorMessage
    , onSearch = searchTeamUser teamId
    , placeholder = "User email"
    }


anyUserSearchConfig : String -> SearchBox.Config User
anyUserSearchConfig teamId =
    { renderItem = .email
    , renderError = Utils.Http.errorMessage
    , onSearch = searchAnyUser
    , placeholder = "User email"
    }


toCmd msg =
    Task.perform (\_ -> msg) <| Task.succeed ()
