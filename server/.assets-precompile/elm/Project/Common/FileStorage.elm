port module Project.Common.FileStorage
    exposing
        ( fetchCredentials
        , listFiles
        , receiveFileList
        , Credentials
        , Error(..)
        , FileDescription
        )

import Common.Types exposing (ProjectStructure)
import Utils.Http
import Utils.Routes as Routes
import Json.Decode exposing (Decoder, string, at, int, bool, oneOf, nullable, list)
import Json.Decode.Pipeline exposing (optional, decode, required)
import RemoteData exposing (WebData, RemoteData)
import Json.Encode


type Credentials
    = Credentials
        { accessToken : String
        }


type Error
    = InvalidCredentials
    | InvalidPayload String


type alias FileDescription =
    { name : String
    , size : Int
    }


fetchCredentials : Cmd (WebData (Maybe Credentials))
fetchCredentials =
    Utils.Http.get maybeCredentialsDecoder Routes.dropboxCredentials
        |> Utils.Http.attemptWithRemoteData


listFiles : Credentials -> String -> Cmd msg
listFiles (Credentials { accessToken }) folderPath =
    Json.Encode.object
        [ ( "accessToken", Json.Encode.string accessToken )
        , ( "folderPath", Json.Encode.string folderPath )
        ]
        |> listFilesPort


receiveFileList : (RemoteData Error (List FileDescription) -> msg) -> Sub msg
receiveFileList msg =
    receiveFileListPort (decodeDropboxRequest decodeFileList >> msg)


projectFolderName : ProjectStructure a a a -> String
projectFolderName project =
    project.id



-----------------------------------------------------------
------- Ports
-----------------------------------------------------------


port listFilesPort : Json.Encode.Value -> Cmd msg


port receiveFileListPort : (Json.Decode.Value -> msg) -> Sub msg



-----------------------------------------------------------
------- Encoders & Decoders
-----------------------------------------------------------


crendentialsDecoder : Decoder Credentials
crendentialsDecoder =
    at [ "access_token" ] string
        |> Json.Decode.map (\v -> Credentials { accessToken = v })


maybeCredentialsDecoder : Decoder (Maybe Credentials)
maybeCredentialsDecoder =
    nullable crendentialsDecoder


decodeFileList : Decoder (List FileDescription)
decodeFileList =
    at [ "entries" ] (list fileDescriptionDecoder)


fileDescriptionDecoder : Decoder FileDescription
fileDescriptionDecoder =
    decode FileDescription
        |> required "name" string
        |> required "size" int


decodeDropboxRequest : Decoder a -> Json.Decode.Value -> RemoteData Error a
decodeDropboxRequest subDecoder value =
    let
        decoded =
            Json.Decode.decodeValue
                subDecoder
                value
    in
        case decoded of
            Ok something ->
                RemoteData.succeed something

            Err description ->
                Json.Decode.decodeValue
                    invalidCredentialsDecoder
                    value
                    |> Result.withDefault (InvalidPayload description)
                    |> RemoteData.Failure


invalidCredentialsDecoder : Decoder Error
invalidCredentialsDecoder =
    at [ "invalidCredentials" ] bool
        |> Json.Decode.map (always InvalidCredentials)
