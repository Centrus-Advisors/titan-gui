module Utils.FileStorage.Types exposing (..)

import Utils.FileStorage.Dropbox exposing (RequestError)
import RemoteData exposing (RemoteData, WebData)
import Utils.FileStorage.Dropbox as Dropbox
    exposing
        ( FolderItem
        , FileInfo
        , DownloadLink
        , Credentials
        )
import Utils.FileReader as FileReader
import Task exposing (Task)


type FileStorage
    = FileStorage Model


type alias Model =
    { sourceID : SourceID
    , credentials : WebData (Maybe Credentials)
    , fileList : RemoteData RequestError (List FolderItem)
    , createSourceFolder : RemoteData RequestError FolderItem
    , fileUpload : RemoteData RequestError FolderItem
    }


type SourceID
    = ProjectID String
    | ContactID String
    | CompanyID String


type Msg
    = FetchCredentials
    | FetchCredentialsUpdate (WebData (Maybe Credentials))
    | FetchFileList Credentials
    | FetchFileListUpdate (RemoteData RequestError (List FolderItem))
    | CreateSourceFolder Credentials
    | CreateSourceFolderUpdate (RemoteData RequestError FolderItem)
    | FetchDownloadLink Credentials FileInfo
    | FetchDownloadLinkUpdate FileInfo (RemoteData RequestError DownloadLink)
    | Delete Credentials FileInfo
    | DeleteUpdate FileInfo (RemoteData RequestError ())
    | UploadFile Credentials (Maybe String) (List FileReader.NativeFile)
    | UploadFileUpdate (RemoteData RequestError FolderItem)
