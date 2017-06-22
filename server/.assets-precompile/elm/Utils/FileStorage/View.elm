module Utils.FileStorage.View exposing (view)

import Utils.FileStorage.Types exposing (..)
import Utils.FileStorage.Dropbox as Dropbox
    exposing
        ( FolderItem(File, Folder)
        , RequestError
            ( InvalidCredentials
            , PathNotFound
            , MalformedPath
            , MalformedCredentials
            , InvalidEndpoint
            , HttpError
            )
        , Credentials
        , FileInfo
        , FolderInfo
        )
import Html exposing (Html, text, div, h2, p, a, button, span, label, ul, li, input, i)
import Html.Attributes exposing (href, class, target, style, type_, title)
import Html.Events exposing (onClick, on)
import RemoteData exposing (RemoteData, WebData)
import Utils.Ui as Ui
import Utils.Routes as Routes
import Utils.FlashMessages
import Utils.Http
import Http
import Json.Decode
import List.Extra
import Utils.FileReader exposing (parseSelectedFiles)


--------------------------------------------------------------------------------
-- If dropbox is not authenticated, or if the project
-- doesn't have a folder yet, this will show buttons to take the
-- next steps. Otherwise it will show nothing.
--------------------------------------------------------------------------------


view : FileStorage -> List String -> Html Msg
view (FileStorage model) requiredFileNames =
    case model.credentials of
        RemoteData.Success Nothing ->
            authenticationView

        RemoteData.Success (Just credentials) ->
            fileListing requiredFileNames credentials model

        _ ->
            model.credentials
                |> toDropboxRequest
                |> dropboxShowWhenLoaded (always <| text "")


fileListing : List String -> Credentials -> Model -> Html Msg
fileListing requiredFileNames credentials model =
    case model.fileList of
        RemoteData.Success folderItems ->
            -- Everything alright. No setup needed, so we show nothing
            div []
                [ dropboxSubmissionInfo "File uploaded successfully" model.fileUpload
                , filesView requiredFileNames credentials folderItems
                , a
                    [ class "pull-right m-r-10"
                    , title "Click here to upload an arbitrary file."
                    , style
                        [ ( "transform", "translateY(-10px)" )
                        , ( "font-size", "20px" )
                        ]
                    ]
                    [ label
                        [ style [ ( "cursor", "pointer" ) ]
                        , class "fa fa-cloud-upload"
                        ]
                        [ uploadInput credentials Nothing ]
                    ]
                ]

        RemoteData.Failure PathNotFound ->
            folderCreation credentials model.createSourceFolder

        _ ->
            model.fileList
                |> dropboxShowWhenLoaded (always <| text "")


folderCreation credentials createSourceFolder =
    case createSourceFolder of
        RemoteData.NotAsked ->
            div
                [ class "alert alert-info" ]
                [ p
                    []
                    [ text "This item does not have a file directory yet."
                    ]
                , p
                    [ class "text-right m-t-10" ]
                    [ button
                        [ class "btn btn-info"
                        , onClick <| CreateSourceFolder credentials
                        ]
                        [ text "Create Directory" ]
                    ]
                ]

        _ ->
            createSourceFolder
                |> dropboxShowWhenLoaded (always <| text "Folder Created")


authenticationView =
    div
        [ class "alert alert-warning" ]
        [ p
            []
            [ text "No access to Dropbox configured. Please grant the CRM access to Centrus' Dropbox by logging as Centrus ADMIN."
            ]
        , p
            [ class "text-right m-t-10" ]
            [ a
                [ class "btn btn-warning"
                , href Routes.dropboxCredentialsAuthorise
                , target "blank"
                ]
                [ text "Grant access now" ]
            ]
        ]


toDropboxRequest : WebData a -> RemoteData RequestError a
toDropboxRequest webdata =
    webdata
        |> RemoteData.mapError HttpError



-- Ui.showWhenLoaded expects errors that our server sends, this one expects errors
-- that the Dropbox API server sends


dropboxShowWhenLoaded func val =
    case val of
        RemoteData.Success result ->
            func result

        RemoteData.Loading ->
            Ui.spinner

        RemoteData.NotAsked ->
            text "Stuck."

        RemoteData.Failure InvalidCredentials ->
            authenticationView

        RemoteData.Failure err ->
            errorMessage err
                |> Utils.FlashMessages.failure


dropboxSubmissionInfo : String -> RemoteData RequestError a -> Html Msg
dropboxSubmissionInfo okMessage webdata =
    case webdata of
        RemoteData.Failure er ->
            dropboxShowWhenLoaded (always <| text "") webdata

        _ ->
            -- We know this doesn't have an error, so we just put some
            -- random Http.Error there to allow it to be used by submissionInfo
            webdata
                |> RemoteData.mapError (always Http.Timeout)
                |> Ui.submissionInfo okMessage


errorMessage : RequestError -> String
errorMessage error =
    case error of
        HttpError httpErr ->
            Utils.Http.errorMessage httpErr

        _ ->
            toString error



-------------------------------------------------------------------------------
-- When we have credentials and the folder is created, this will be shown
-------------------------------------------------------------------------------


filesView : List String -> Credentials -> List FolderItem -> Html Msg
filesView requiredFileNames credentials folderItems =
    let
        isRequired file =
            List.member file.name requiredFileNames

        files =
            folderItems
                |> List.filterMap getFile

        fileNames =
            List.map .name files

        requiredFilesMissing =
            List.filter
                (\n -> List.Extra.notMember n fileNames)
                requiredFileNames

        requiredFilesUploaded =
            List.filter isRequired files

        extraFiles =
            List.filter (not << isRequired) files

        filesHtml =
            (List.map (nonUploadedFileView credentials) requiredFilesMissing)
                ++ (List.map (uploadedFileView True credentials) requiredFilesUploaded)
                ++ (List.map (uploadedFileView False credentials) extraFiles)
    in
        ul [ class "callout-list" ] filesHtml


onchange action =
    on
        "change"
        (Json.Decode.map action parseSelectedFiles)


getFile : FolderItem -> Maybe FileInfo
getFile item =
    case item of
        File f ->
            Just f

        _ ->
            Nothing


nonUploadedFileView : Credentials -> String -> Html Msg
nonUploadedFileView credentials fileName =
    li [ class "project-file-buttons callout-list-item callout-list-item-danger" ]
        [ text fileName
        , span
            []
            [ label
                [ class "btn btn-sm btn-info project-file-button-showOnHover"
                ]
                [ text "Upload"
                , uploadInput credentials (Just fileName)
                ]
            , button
                [ class "btn btn-sm btn-danger project-file-button-hideOnHover" ]
                [ text "Required" ]
            ]
        ]


uploadedFileView : Bool -> Credentials -> FileInfo -> Html Msg
uploadedFileView isRequired credentials file =
    let
        colorClass =
            if isRequired then
                "success"
            else
                "default"
    in
        li [ class <| "project-file-buttons callout-list-item callout-list-item-" ++ colorClass ]
            [ text <| file.name ++ file.extension
            , span
                []
                [ if file.downloadLink == RemoteData.Loading || file.deletion == RemoteData.Loading then
                    Ui.inlineSpinner
                  else
                    text ""
                , text " "
                , button
                    [ class "btn btn-sm btn-danger m-r-5 project-file-button-showOnHover"
                    , onClick <| Delete credentials file
                    ]
                    [ text "Delete" ]
                , button
                    [ class "btn btn-sm btn-info project-file-button-showOnHover"
                    , onClick <| FetchDownloadLink credentials file
                    ]
                    [ text "Download" ]
                , button
                    [ class <| "btn btn-sm btn-" ++ colorClass ++ " project-file-button-hideOnHover"
                    ]
                    [ text "Uploaded" ]
                ]
            ]


uploadInput : Credentials -> Maybe String -> Html Msg
uploadInput credentials mName =
    input
        [ type_ "file"
        , onchange (UploadFile credentials mName)
        , style [ ( "display", "none" ) ]
        ]
        []
