module Utils.FileStorage.Dropbox
    exposing
        ( fetchCredentials
        , fetchFolderContent
        , createFolder
        , fetchDownloadLink
        , deleteItem
        , upload
        , decodeError
        , getLink
        , RequestError(..)
        , Credentials
        , FileInfo
        , FolderInfo
        , FolderItem(..)
        , DownloadLink
        )

import Common.Types exposing (ProjectStructure)
import Utils.Http
import Utils.Routes as Routes
import Json.Decode exposing (Decoder, field, string, at, int, bool, oneOf, nullable, list)
import Json.Decode.Pipeline exposing (optional, decode, required, hardcoded)
import RemoteData exposing (WebData, RemoteData)
import Json.Encode
import Http
import Task exposing (Task)
import Regex
import Utils.FileReader as FileReader exposing (NativeFile)


type Credentials
    = Credentials
        { accessToken : String
        }


type RequestError
    = InvalidCredentials
    | PathNotFound
    | MalformedPath
    | MalformedCredentials
    | InvalidEndpoint
    | HttpError Http.Error


type DownloadLink
    = DownloadLink String


type alias FileInfo =
    { id : String
    , name : String
    , extension : String
    , size : Int
    , path : String
    , downloadLink : RemoteData RequestError DownloadLink
    , deletion : RemoteData RequestError ()
    }


type alias FolderInfo =
    { id : String
    , name : String
    , path : String
    }


type FolderItem
    = File FileInfo
    | Folder FolderInfo


fetchCredentials : Cmd (WebData (Maybe Credentials))
fetchCredentials =
    Utils.Http.get maybeCredentialsDecoder Routes.dropboxCredentials
        |> Utils.Http.attemptWithRemoteData


getLink : DownloadLink -> String
getLink (DownloadLink l) =
    l



-----------------------------------------------------------
------- Encoders & Decoders
-----------------------------------------------------------
-- Credetials


crendentialsDecoder : Decoder Credentials
crendentialsDecoder =
    at [ "access_token" ] string
        |> Json.Decode.map (\v -> Credentials { accessToken = v })


maybeCredentialsDecoder : Decoder (Maybe Credentials)
maybeCredentialsDecoder =
    Json.Decode.oneOf
        [ (nullable crendentialsDecoder)
        , Json.Decode.succeed Nothing
        ]



-- Folder Content


folderContentDecoder : Decoder (List FolderItem)
folderContentDecoder =
    at [ "entries" ] (list folderItemDecoder)



-- Folder Item


folderItemDecoder : Decoder FolderItem
folderItemDecoder =
    field ".tag" string
        |> Json.Decode.andThen
            (\tag ->
                case tag of
                    "file" ->
                        fileDecoder

                    "folder" ->
                        folderDecoder

                    _ ->
                        Json.Decode.fail <| "Invalid tag type: \"" ++ tag ++ "\""
            )


fileDecoder : Decoder FolderItem
fileDecoder =
    decode FileInfo
        |> required "id" string
        |> required "name" nameDecoder
        |> required "name" extensionDecoder
        |> required "size" int
        |> required "path_display" string
        |> hardcoded RemoteData.NotAsked
        |> hardcoded RemoteData.NotAsked
        |> Json.Decode.map File


extensionRegex =
    Regex.regex "\\.\\w+$"


getExtension : String -> String
getExtension fileName =
    Regex.find (Regex.AtMost 1) extensionRegex fileName
        |> List.map .match
        |> List.head
        |> Maybe.withDefault ""


removeExtension : String -> String
removeExtension fileName =
    Regex.replace
        (Regex.AtMost 1)
        extensionRegex
        (always "")
        fileName


extensionDecoder =
    Json.Decode.map getExtension string


nameDecoder =
    Json.Decode.map removeExtension string


folderDecoder : Decoder FolderItem
folderDecoder =
    decode FolderInfo
        |> required "id" string
        |> required "name" string
        |> required "path_display" string
        |> Json.Decode.map Folder



-- Some errors in this shitty Dropbox API are not JSON, they are just plain text,
-- even though the request header says that the return type should be JSON.
-- This function takes care of the JSON both of them


decodeError : String -> Result String RequestError
decodeError body =
    let
        decodedError =
            Json.Decode.decodeString jsonErrorDecoder body

        isInvalidPath =
            String.contains "Error in call to API function" body
                && String.contains "request body: path:" body

        isInvalidCredentials =
            String.contains "Invalid authorization value in HTTP header" body

        isInvalidEndpoint =
            String.contains "Unknown API function:" (Debug.log "BODY: " body)
    in
        case decodedError of
            Ok val ->
                Ok val

            Err err ->
                if isInvalidPath then
                    Ok MalformedPath
                else if isInvalidCredentials then
                    Ok MalformedCredentials
                else if isInvalidEndpoint then
                    Ok InvalidEndpoint
                else
                    Err err



-- This function takes care of only the errors that are returned as a JSON value


jsonErrorDecoder : Decoder RequestError
jsonErrorDecoder =
    at [ "error", ".tag" ] string
        |> Json.Decode.andThen
            (\errorTag ->
                case errorTag of
                    "path" ->
                        Json.Decode.succeed PathNotFound

                    "invalid_access_token" ->
                        Json.Decode.succeed InvalidCredentials

                    _ ->
                        Json.Decode.fail <| "Unknown Dropbox API error:" ++ errorTag
            )



-----------------------------------------------------------
------- DROPBOX API ENDPOINTS
-----------------------------------------------------------


fetchFolderContent : Credentials -> String -> Cmd (RemoteData RequestError (List FolderItem))
fetchFolderContent credentials folderPath =
    let
        endpoint =
            "https://api.dropboxapi.com/2/files/list_folder"

        decoder =
            folderContentDecoder

        body =
            Utils.Http.toJsonBody
                [ ( "path", Json.Encode.string folderPath )
                , ( "recursive", Json.Encode.bool False )
                , ( "include_media_info", Json.Encode.bool False )
                , ( "include_deleted", Json.Encode.bool False )
                , ( "include_has_explicit_shared_members", Json.Encode.bool False )
                ]
    in
        dropboxRequest credentials POST endpoint decoder body


createFolder : Credentials -> String -> Cmd (RemoteData RequestError FolderItem)
createFolder credentials folderPath =
    let
        endpoint =
            "https://api.dropboxapi.com/2/files/create_folder"

        decoder =
            folderDecoder

        body =
            Utils.Http.toJsonBody
                [ ( "path", Json.Encode.string folderPath )
                , ( "autorename", Json.Encode.bool False )
                ]
    in
        dropboxRequest credentials POST endpoint decoder body


fetchDownloadLink : Credentials -> FileInfo -> Cmd (RemoteData RequestError DownloadLink)
fetchDownloadLink credentials file =
    let
        endpoint =
            "https://api.dropboxapi.com/2/files/get_temporary_link"

        body =
            Utils.Http.toJsonBody
                [ ( "path", Json.Encode.string file.path )
                ]

        decoder =
            at [ "link" ] string
                |> Json.Decode.map DownloadLink
    in
        dropboxRequest credentials POST endpoint decoder body


deleteItem : Credentials -> FolderItem -> Cmd (RemoteData RequestError ())
deleteItem credentials item =
    let
        itemPath =
            case item of
                Folder f ->
                    f.path

                File f ->
                    f.path

        endpoint =
            "https://api.dropboxapi.com/2/files/delete"

        decoder =
            Json.Decode.succeed ()

        body =
            Utils.Http.toJsonBody
                [ ( "path", Json.Encode.string itemPath )
                ]
    in
        dropboxRequest credentials POST endpoint decoder body



-- Uploads a file to dropbox, optionally specifying a name for it.


upload : Credentials -> String -> Maybe String -> NativeFile -> Cmd (RemoteData RequestError FolderItem)
upload credentials folderPath mName nativeFile =
    let
        fileName =
            Maybe.withDefault nativeFile.name mName
                |> removeExtension

        filePath =
            folderPath
                ++ "/"
                ++ fileName
                ++ (getExtension nativeFile.name)

        uploadDescription =
            Json.Encode.encode 0 <|
                Json.Encode.object
                    [ ( "path", Json.Encode.string filePath )
                    , ( "mode", Json.Encode.string "add" )
                    , ( "autorename", Json.Encode.bool False )
                    , ( "mute", Json.Encode.bool False )
                    ]

        headers =
            [ authorisationHeader credentials
            , Http.header "Dropbox-API-Arg" uploadDescription
            ]

        body =
            FileReader.rawBody "application/octet-stream" nativeFile

        options =
            { method = toString POST
            , headers = headers
            , url = "https://content.dropboxapi.com/2/files/upload"
            , body = body
            , expect = Http.expectJson fileDecoder
            , timeout = Nothing
            , withCredentials = False
            }
    in
        Http.request options
            |> Http.toTask
            |> withDropboxError
            |> Utils.Http.attemptWithRemoteData



-------------------------------------------------------------------------------
--- Request Handler
-------------------------------------------------------------------------------


type Method
    = POST


authorisationHeader : Credentials -> Http.Header
authorisationHeader (Credentials { accessToken }) =
    Http.header "Authorization" ("Bearer " ++ accessToken)


dropboxRequest : Credentials -> Method -> String -> Decoder a -> Http.Body -> Cmd (RemoteData RequestError a)
dropboxRequest credentials method endpoint decoder body =
    let
        headers =
            [ authorisationHeader credentials
            ]

        options =
            { method = toString method
            , headers = headers
            , url = endpoint
            , body = body
            , expect = Http.expectJson decoder
            , timeout = Nothing
            , withCredentials = False
            }
    in
        Http.request options
            |> Http.toTask
            |> withDropboxError
            |> Utils.Http.attemptWithRemoteData


withDropboxError : Task Http.Error a -> Task RequestError a
withDropboxError t =
    let
        convertError err =
            case err of
                Http.BadStatus { status, body } ->
                    decodeError body
                        |> Result.withDefault (HttpError err)

                _ ->
                    HttpError err
    in
        Task.mapError convertError t
