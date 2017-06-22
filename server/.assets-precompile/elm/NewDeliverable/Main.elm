module NewDeliverable.Main
    exposing
        ( Model
        , Msg
        , Deliverable
        , view
        , init
        , update
        , Config
        )

{-
   This module is not aimed at being compiled on it's own, but
   at being used by other modules


   Generates a modal to create a deliverable
-}

import NewDeliverable.Types
import NewDeliverable.State
import NewDeliverable.View


type alias Model msg =
    NewDeliverable.Types.Model msg


type alias Msg =
    NewDeliverable.Types.Msg


type alias Deliverable =
    NewDeliverable.Types.Deliverable


type alias Config msg =
    NewDeliverable.Types.Config msg


init =
    NewDeliverable.State.init


update =
    NewDeliverable.State.update


view =
    NewDeliverable.View.root
