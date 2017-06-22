module NewUser.Types exposing (..)

import Utils.SearchBox as SearchBox
import RemoteData exposing (WebData)
import Common.Types exposing (ContactS, Position, Permission(..))


type alias Model =
    { screen : Screen
    }


type Screen
    = ScreenUserCreation UserCreationModel
    | ScreenContactCreation ContactCreationModel
    | ScreenContactFind (SearchBox.SearchBox ContactS)
    | NoScreen


type alias UserCreationModel =
    { user : EditableUser
    , submission : WebData ()
    }


type UserCreationMsg
    = SubmitUser
    | SubmitUserUpdate (WebData ())
    | ChangePermission String
    | ChangePosition (SearchBox.Msg Position)
    | ChangeEmail1 String
    | ChangeEmail2 String
    | ChangePassword1 String
    | ChangePassword2 String
    | ChangeActive Bool


type alias ContactCreationModel =
    { contact : EditableContact
    , submission : WebData ContactS
    }


type alias ContactFindModel =
    SearchBox.SearchBox ContactS


type ContactCreationMsg
    = SubmitContact
    | SubmitContactUpdate (WebData ContactS)
    | ChangeContactName String
    | ChangeContactEmail String


type ContactFindMsg
    = ChooseFound
    | ChangeSearch (SearchBox.Msg ContactS)


type Msg
    = UserCreation UserCreationMsg
    | ContactCreation ContactCreationMsg
    | ContactFind ContactFindMsg
    | SelectContact ContactS
    | GoToContactCreation
    | GoToContactFind
    | SetModalOpen Bool



-- We require it to have a full contact so that it can
-- only be created once a contact for it is chosen


type alias EditableUser =
    { contact : ContactS
    , permission : Permission
    , position : SearchBox.SearchBox Position
    , email1 : String
    , email2 : String
    , pass1 : String
    , pass2 : String
    , active : Bool
    }


type alias EditableContact =
    { name : String
    , email : String
    }
