module Utils.Activities.ActivityList.Types exposing (..)

import RemoteData exposing (WebData)
import Utils.Activities.CommonTypes exposing (..)


type Msg
    = Fetch FetchType
    | FetchUpdate FetchType (WebData (List Activity))
    | Delete Activity
    | DeletionUpdate (WebData ())
    | WarnDeletion (Maybe Activity)


type FetchType
    = FreshLoad
    | LoadingMore


type alias Model =
    { deletionWarning : Maybe Activity
    , partialSearch : Maybe PartialSearch
    , activities : List Activity
    , fetchStatus : WebData (List Activity)
    , deletionStatus : WebData ()
    }


type ActivityList
    = ActivityList Model
