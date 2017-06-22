module Utils.FileStorage.State
    exposing
        ( initFromProject
        , initFromContact
        , initFromCompany
        , update
        )

import Utils.FileStorage.Types exposing (..)
import Utils.FileStorage.Dropbox as Dropbox
    exposing
        ( FolderItem(Folder, File)
        , FolderInfo
        , FileInfo
        )
import RemoteData exposing (RemoteData, WebData)
import Navigation
import List.Extra
import Utils.FileReader as FileReader
import Task


-- INIT


initFromProject : String -> ( FileStorage, Cmd Msg )
initFromProject projectID =
    init (ProjectID projectID)


initFromContact : String -> ( FileStorage, Cmd Msg )
initFromContact contactID =
    init (ContactID contactID)


initFromCompany : String -> ( FileStorage, Cmd Msg )
initFromCompany companyID =
    init (CompanyID companyID)


init : SourceID -> ( FileStorage, Cmd Msg )
init sourceID =
    { sourceID = sourceID
    , credentials = RemoteData.NotAsked
    , fileList = RemoteData.NotAsked
    , createSourceFolder = RemoteData.NotAsked
    , fileUpload = RemoteData.NotAsked
    }
        |> FileStorage
        |> update FetchCredentials



-- UPDATE


update : Msg -> FileStorage -> ( FileStorage, Cmd Msg )
update msg (FileStorage model) =
    subUpdate msg model
        |> Tuple.mapFirst FileStorage


subUpdate : Msg -> Model -> ( Model, Cmd Msg )
subUpdate msg model =
    case msg of
        FetchCredentials ->
            { model | credentials = RemoteData.Loading }
                ! [ Dropbox.fetchCredentials
                        |> Cmd.map FetchCredentialsUpdate
                  ]

        FetchCredentialsUpdate status ->
            case status of
                RemoteData.Success (Just credentials) ->
                    subUpdate
                        (FetchFileList credentials)
                        { model | credentials = status }

                _ ->
                    { model | credentials = status } ! []

        FetchFileList credentials ->
            { model | fileList = RemoteData.Loading }
                ! [ Dropbox.fetchFolderContent credentials (getSourceFolder model.sourceID)
                        |> Cmd.map FetchFileListUpdate
                  ]

        FetchFileListUpdate status ->
            { model | fileList = status } ! []

        CreateSourceFolder credentials ->
            { model | createSourceFolder = RemoteData.Loading }
                ! [ Dropbox.createFolder credentials (getSourceFolder model.sourceID)
                        |> Cmd.map CreateSourceFolderUpdate
                  ]

        CreateSourceFolderUpdate status ->
            let
                newModel =
                    { model | createSourceFolder = status }
            in
                case status of
                    RemoteData.Success folderInfo ->
                        model.credentials
                            |> RemoteData.withDefault Nothing
                            |> Maybe.map (\c -> subUpdate (FetchFileList c) newModel)
                            |> Maybe.withDefault (newModel ! [])

                    _ ->
                        { model | createSourceFolder = status } ! []

        FetchDownloadLink credentials file ->
            case file.downloadLink of
                RemoteData.NotAsked ->
                    let
                        fileList =
                            updateFile
                                (\f -> { f | downloadLink = RemoteData.Loading })
                                model.fileList
                                file
                    in
                        { model | fileList = fileList }
                            ! [ Dropbox.fetchDownloadLink credentials file
                                    |> Cmd.map (FetchDownloadLinkUpdate file)
                              ]

                _ ->
                    subUpdate
                        (FetchDownloadLinkUpdate file file.downloadLink)
                        model

        FetchDownloadLinkUpdate file status ->
            let
                fileList =
                    updateFile
                        (\f -> { f | downloadLink = status })
                        model.fileList
                        file
            in
                case status of
                    RemoteData.Success link ->
                        { model | fileList = fileList } ! [ Navigation.load <| Dropbox.getLink link ]

                    _ ->
                        { model | fileList = fileList } ! []

        Delete credentials file ->
            let
                fileList =
                    updateFile
                        (\f -> { f | deletion = RemoteData.Loading })
                        model.fileList
                        file
            in
                { model | fileList = fileList }
                    ! [ Dropbox.deleteItem credentials (File file)
                            |> Cmd.map (DeleteUpdate file)
                      ]

        DeleteUpdate file status ->
            case ( status, model.credentials ) of
                ( RemoteData.Success _, RemoteData.Success (Just credentials) ) ->
                    subUpdate
                        (FetchFileList credentials)
                        { model | fileList = removeFile file model.fileList }

                _ ->
                    let
                        fileList =
                            updateFile
                                (\f -> { f | deletion = status })
                                model.fileList
                                file
                    in
                        { model | fileList = fileList } ! []

        UploadFile credentials mName nativeFiles ->
            case List.head nativeFiles of
                Just nativeFile ->
                    { model | fileUpload = RemoteData.Loading }
                        ! [ Dropbox.upload credentials (getSourceFolder model.sourceID) mName nativeFile
                                |> Cmd.map UploadFileUpdate
                          ]

                Nothing ->
                    model ! []

        UploadFileUpdate status ->
            case ( status, model.credentials ) of
                ( RemoteData.Success _, RemoteData.Success (Just credentials) ) ->
                    subUpdate
                        (FetchFileList credentials)
                        { model | fileUpload = status }

                _ ->
                    { model | fileUpload = status } ! []


updateFile func fileList file =
    RemoteData.map
        (List.Extra.replaceIf
            (\f ->
                case f of
                    File aFile ->
                        aFile.id == file.id

                    _ ->
                        False
            )
            (File <| func file)
        )
        fileList


removeFile : FileInfo -> RemoteData a (List FolderItem) -> RemoteData a (List FolderItem)
removeFile file itemList =
    let
        getId i =
            case i of
                File f ->
                    f.id

                Folder f ->
                    f.id
    in
        RemoteData.map
            (List.Extra.filterNot (getId >> (==) file.id))
            itemList


getSourceFolder : SourceID -> String
getSourceFolder sourceID =
    case sourceID of
        ProjectID id ->
            "/Projects/" ++ id

        ContactID id ->
            "/Contacts/" ++ id

        CompanyID id ->
            "/Companies/" ++ id
