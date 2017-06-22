module Deliverable.Types exposing (..)

import Common.Types
    exposing
        ( Deliverable
        , UserS
        , CompanyS
        , ProjectS
        , DeliverableStatus(Completed, Cancelled, Active)
        , Cost
        )
import Deliverable.TimeTracking.Main as TimeTracking
import RemoteData exposing (WebData)
import Utils.SearchBox as SearchBox


type alias Model =
    {}


type Msg
    = Noop
