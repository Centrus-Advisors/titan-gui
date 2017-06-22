module UserDetails.Types exposing (..)

import RemoteData exposing (WebData)
import Http
import Utils.Activities.Main as Activities
import Common.Types exposing (User)


type alias Model =
    Maybe Content


type alias Content =
    { user : User
    , activities : Activities.Activities
    }


type Msg
    = ActivitiesMsg Activities.Msg
