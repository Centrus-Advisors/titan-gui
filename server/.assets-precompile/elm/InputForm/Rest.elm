module InputForm.Rest exposing (..)

import InputForm.Types exposing (..)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Decode exposing (Decoder)
import Http
import RemoteData exposing (RemoteData, WebData)
import Task exposing (Task)
import DatePicker exposing (DatePicker)


-------------------------------- API -------------------------------------------


dataEndpoint =
    "/data-input-api"


save : List ( String, DBType ) -> Cmd (WebData ())
save formList =
    let
        body =
            encodeFormList formList

        endpoint =
            dataEndpoint
    in
        post (Json.Decode.succeed ()) endpoint body
            |> attemptWithRemoteData



------------------------------- ENCODERS ---------------------------------------


encodeFormList : List ( String, DBType ) -> Http.Body
encodeFormList formList =
    formList
        |> List.map (\( a, b ) -> ( a, Encode.string a ))
        |> toJsonBody


encodeDbType : DBType -> Result String Encode.Value
encodeDbType dbType =
    case dbType of
        DBString nullable maxLen val ->
            ifNotNull nullable
                val
                (\txt ->
                    if String.length txt > maxLen then
                        Err <| "This field exceeds the maximum amount of " ++ (toString maxLen) ++ " characters"
                    else
                        Ok (Encode.string txt)
                )

        DBTimeStamp nullable datePicker ->
            emptyPicker nullable datePicker

        DBDate nullable datePicker ->
            emptyPicker nullable datePicker

        DBNumber nullable val ->
            if nullable && String.isEmpty val then
                Ok (Encode.null)
            else
                case String.toInt val of
                    Ok n ->
                        Ok (Encode.int n)

                    Err _ ->
                        Err <| "Could not convert \"" ++ val ++ "\" to integer. Please insert a valid number"

        DBFloat nullable val ->
            if nullable && String.isEmpty val then
                Ok (Encode.null)
            else
                case String.toFloat val of
                    Ok n ->
                        Ok (Encode.float n)

                    Err _ ->
                        Err <| "Could not convert \"" ++ val ++ "\" to float. Please insert a valid number"


ifNotNull : Bool -> String -> (String -> Result String Encode.Value) -> Result String Encode.Value
ifNotNull canBeNull val f =
    if String.isEmpty val && not canBeNull then
        Err "This field cannot be empty"
    else
        f val


emptyPicker : Bool -> DatePicker -> Result String Encode.Value
emptyPicker nullable datePicker =
    case DatePicker.getDate datePicker of
        Just aDate ->
            Ok (Encode.string <| toString aDate)

        Nothing ->
            if nullable then
                Ok (Encode.null)
            else
                Err "This field cannot be empty. Please choose a date"


toJsonBody : List ( String, Encode.Value ) -> Http.Body
toJsonBody tupleList =
    tupleList
        |> Encode.object
        |> Http.jsonBody



-------------------------------- DECODERS --------------------------------------
---------------------------- LOW LEVEL HTTP ------------------------------------


type alias Endpoint =
    String


post : Decoder a -> Endpoint -> Http.Body -> Task Http.Error a
post decoder endpoint body =
    makeJsonRequest "POST" body decoder endpoint
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


attemptWithRemoteData : Task x a -> Cmd (RemoteData x a)
attemptWithRemoteData =
    Task.attempt (mapResult RemoteData.Failure RemoteData.Success)


mapResult : (a -> msg) -> (b -> msg) -> Result a b -> msg
mapResult failure success result =
    case result of
        Result.Ok r ->
            success r

        Result.Err r ->
            failure r
