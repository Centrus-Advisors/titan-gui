module Project.Deliverables.Deliverables
    exposing
        ( Model
        , Msg
        , Deliverable
        , view
        , init
        , update
        , fetchDeliverables
        )

import Project.Deliverables.Rest
import Project.Deliverables.Types
import Project.Deliverables.View
import Project.Deliverables.State


type alias Model =
    Project.Deliverables.Types.Model


type alias Msg =
    Project.Deliverables.Types.Msg


type alias Deliverable =
    Project.Deliverables.Types.Deliverable


init =
    Project.Deliverables.State.init


update =
    Project.Deliverables.State.update


fetchDeliverables =
    Project.Deliverables.Rest.fetchDeliverables


view =
    Project.Deliverables.View.root
