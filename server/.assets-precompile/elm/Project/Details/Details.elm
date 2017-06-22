module Project.Details.Details exposing (Model, Msg, init, update, view, getProject)

import Project.Details.State
import Project.Details.Types
import Project.Details.View


type alias Model =
    Project.Details.Types.Model


type alias Msg =
    Project.Details.Types.Msg


init =
    Project.Details.State.init


update =
    Project.Details.State.update


view =
    Project.Details.View.root


getProject =
    Project.Details.State.getProject
