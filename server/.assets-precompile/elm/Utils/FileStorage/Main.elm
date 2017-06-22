module Utils.FileStorage.Main
    exposing
        ( FileStorage
        , Msg
        , initFromProject
        , initFromContact
        , initFromCompany
        , update
        , view
        , FileInfo
        , FolderInfo
        )

import Utils.FileStorage.Types
import Utils.FileStorage.State
import Utils.FileStorage.View
import Utils.FileStorage.Dropbox


type alias Msg =
    Utils.FileStorage.Types.Msg


type alias FileStorage =
    Utils.FileStorage.Types.FileStorage


type alias FileInfo =
    Utils.FileStorage.Dropbox.FileInfo


type alias FolderInfo =
    Utils.FileStorage.Dropbox.FolderInfo


initFromProject =
    Utils.FileStorage.State.initFromProject


initFromContact =
    Utils.FileStorage.State.initFromContact


initFromCompany =
    Utils.FileStorage.State.initFromCompany


update =
    Utils.FileStorage.State.update


view =
    Utils.FileStorage.View.view
