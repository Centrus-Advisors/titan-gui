module TeamDetails.Types exposing (..)

import RemoteData exposing (WebData)
import Utils.SearchBox as SearchBox
import Date exposing (Date)
import Table
import Common.Types
    exposing
        ( UserStructure
        , ContactS
        , TeamStructure
        )


type Msg
    = SetEditing Bool
    | SaveEditing
    | TeamEditing TeamEditingMsg
    | TeamSaveUpdate (WebData Team)
    | TeamFetchUpdate (WebData ( Team, Bool ))
    | TeamMembersFetchUpdate (WebData (List User))
    | Reload String
    | DeleteTeam
    | DeleteTeamUpdate (WebData ())


type TeamEditingMsg
    = ChangeName String
    | SearchLeader (SearchBox.Msg User)
    | SearchNewMember (SearchBox.Msg User)
    | ChooseNewMember
    | NewMemberUpdate (WebData ())
    | RemoveMember User
    | RemoveMemberUpdate (WebData ())
    | TeamMembersReloadUpdate (WebData (List User))
    | ToggleDeletionModal


type alias Model =
    { team : WebData Team
    , members : WebData (List User)
    , editable : Maybe EditableTeam
    , submission : WebData Team
    , deleteTeamSubmission : WebData ()
    , canBeEdited : Bool
    }


type alias Team =
    TeamStructure User


type alias User =
    UserStructure String ContactS String


type alias Contact =
    ContactS


type alias EditableTeam =
    { id : String
    , name : String
    , leader : SearchBox.SearchBox User
    , members : WebData (List User)
    , newMember : SearchBox.SearchBox User
    , newMemberSubmission : WebData ()
    , removeMemberSubmission : WebData ()
    , confirmDeleteModalShowing : Bool
    }
