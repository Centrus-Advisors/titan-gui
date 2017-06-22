module Utils.Table.Main
    exposing
        ( Config
        , init
        , update
        , textFilter
        , view
        , subscriptions
        )

import Utils.Table.Types
import Utils.Table.State
import Html exposing (text)
import Utils.Table.View


type alias Config doc =
    Utils.Table.Types.Config doc


init =
    Utils.Table.State.init


update =
    Utils.Table.State.update


subscriptions =
    Utils.Table.State.subscriptions


textFilter =
    Utils.Table.State.textFilter


view =
    Utils.Table.View.view
