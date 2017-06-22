module Utils.Http exposing (getResponse, get, post, put, delete, toJsonBody, mapResult, errorMessage, attemptWithRemoteData)

import Http
import Task exposing (Task)
import Json.Encode
import Json.Decode exposing (Decoder, string, list, nullable)
import Json.Decode.Pipeline exposing (decode, required, optional)
import RemoteData exposing (RemoteData)


type alias Endpoint =
    String



-- This method returns the entire response with the parsed body.


getResponse : (Http.Response String -> Result String a) -> Endpoint -> Task Http.Error a
getResponse responseParser endpoint =
    let
        expect =
            Http.expectStringResponse responseParser
    in
        makeRequest "GET" Http.emptyBody expect endpoint
            |> Http.toTask


get : Decoder a -> Endpoint -> Task Http.Error a
get decoder endpoint =
    makeJsonRequest "GET" Http.emptyBody decoder endpoint
        |> Http.toTask


delete : String -> Task Http.Error ()
delete endpoint =
    makeRequest "DELETE" Http.emptyBody (Http.expectStringResponse <| always Ok "") endpoint
        |> Http.toTask
        |> Task.map (always ())


post : Decoder a -> Endpoint -> Http.Body -> Task Http.Error a
post decoder endpoint body =
    makeJsonRequest "POST" body decoder endpoint
        |> Http.toTask


put : Decoder a -> Endpoint -> Http.Body -> Task Http.Error a
put decoder endpoint body =
    makeJsonRequest "PUT" body decoder endpoint
        |> Http.toTask


makeJsonRequest : String -> Http.Body -> Decoder a -> Endpoint -> Http.Request a
makeJsonRequest method body decoder endpoint =
    makeRequest method body (Http.expectJson decoder) endpoint


makeRequest : String -> Http.Body -> Http.Expect a -> Endpoint -> Http.Request a
makeRequest method body expect endpoint =
    let
        options =
            { method = method
            , headers = []
            , url = endpoint
            , body = body
            , expect = expect
            , timeout = Nothing
            , withCredentials = False
            }
    in
        Http.request options


mapResult : (a -> msg) -> (b -> msg) -> Result a b -> msg
mapResult failure success result =
    case result of
        Result.Ok r ->
            success r

        Result.Err r ->
            failure r


toJsonBody : List ( String, Json.Encode.Value ) -> Http.Body
toJsonBody tupleList =
    tupleList
        |> Json.Encode.object
        |> Http.jsonBody


errorMessage : Http.Error -> String
errorMessage error =
    case error of
        Http.BadUrl wrongUrl ->
            "Invalid url: " ++ wrongUrl

        Http.Timeout ->
            "The server didn't respond on time. Please try again"

        Http.NetworkError ->
            "Unable to connect to server"

        Http.BadPayload errMessage { status } ->
            "Unable to parse server response: " ++ errMessage

        Http.BadStatus { status, body } ->
            let
                decodedError =
                    Json.Decode.decodeString serverErrorDecoder body
            in
                case decodedError of
                    Ok err ->
                        serverErrorToString err

                    Err _ ->
                        "Server returned " ++ (toString status.code) ++ ". " ++ status.message


type alias ServerError =
    { message : String
    , errors : List ServerErrorItem
    }


type alias ServerErrorItem =
    { message : String
    , field : Maybe String
    }


serverErrorDecoder : Decoder ServerError
serverErrorDecoder =
    decode ServerError
        |> required "message" string
        |> optional "errors" (list serverErrorItemDecoder) []


serverErrorItemDecoder : Decoder ServerErrorItem
serverErrorItemDecoder =
    decode ServerErrorItem
        |> required "message" string
        |> optional "field" (nullable string) Nothing


serverErrorToString : ServerError -> String
serverErrorToString err =
    let
        header =
            err.message

        body =
            err.errors
                |> List.map
                    (\e ->
                        e.field
                            |> Maybe.map (\v -> v ++ ": " ++ e.message)
                            |> Maybe.withDefault ""
                    )
                |> List.foldl (\a b -> b ++ "\n - " ++ a) ""
    in
        header ++ body


attemptWithRemoteData : Task x a -> Cmd (RemoteData x a)
attemptWithRemoteData =
    Task.attempt (mapResult RemoteData.Failure RemoteData.Success)
