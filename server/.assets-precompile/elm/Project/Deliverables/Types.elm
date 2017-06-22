module Project.Deliverables.Types exposing (..)

import Date exposing (Date)
import Table
import DatePicker
import RemoteData exposing (WebData)
import Utils.SearchBox as SearchBox
import NewDeliverable.Main as NewDeliverable
import Common.Types exposing (DeliverableStructure, UserS)


type alias Model =
    { new : Maybe (NewDeliverable.Model Msg)
    , deliverables : WebData (List Deliverable)
    , tableState : Table.State
    }


type Msg
    = FetchDeliverables String
    | FetchDeliverablesUpdate (WebData (List Deliverable))
    | SetTableState Table.State
    | ShowNewDeliverable Bool
    | NewDeliverableUpdate NewDeliverable.Msg


type alias Deliverable =
    DeliverableStructure String UserS
