module InputForm.Rest exposing (..)

import InputForm.Types exposing (..)
import Json.Encode as Encode
import Json.Decode.Pipeline exposing (decode, optional, required)
import Json.Decode exposing (Decoder, string)
import Http
import RemoteData exposing (RemoteData, WebData)
import Task exposing (Task)
import DatePicker exposing (DatePicker)
import Date.Extra


-------------------------------- API -------------------------------------------


dataEndpoint =
    "/data-input-api"


save : Http.Body -> Cmd (WebData Trade)
save body =
    post tradeDecoder dataEndpoint body
        |> attemptWithRemoteData



------------------------------- ENCODERS ---------------------------------------


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
            emptyPicker nullable "yyyy-mm-dd HH:mm:ss" datePicker

        DBDate nullable datePicker ->
            emptyPicker nullable "yyyy-mm-dd" datePicker

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


emptyPicker : Bool -> String -> DatePicker -> Result String Encode.Value
emptyPicker nullable printFormat datePicker =
    case DatePicker.getDate datePicker of
        Just aDate ->
            Date.Extra.toFormattedString printFormat aDate
                |> Encode.string
                |> Ok

        Nothing ->
            if nullable then
                Ok (Encode.null)
            else
                Err "This field cannot be empty. Please choose a date"


encodeFormList : List ( String, String, DBType ) -> Result String Http.Body
encodeFormList formList =
    formList
        |> List.map (\( a, b, c ) -> ( a, encodeDbType c ))
        |> reduceList
        |> Result.map toJsonBody


reduceList : List ( String, Result String Encode.Value ) -> Result String (List ( String, Encode.Value ))
reduceList formList =
    List.foldl
        (\( title, rValue ) rOutcome ->
            Result.map2
                (\value outcome -> ( title, value ) :: outcome)
                rValue
                rOutcome
        )
        (Result.Ok [])
        formList


toJsonBody : List ( String, Encode.Value ) -> Http.Body
toJsonBody tupleList =
    tupleList
        |> Encode.object
        |> Http.jsonBody



-------------------------------- DECODERS --------------------------------------


tradeDecoder : Decoder Trade
tradeDecoder =
    decode Trade
        |> optional "RECORD_TYPE" string ""
        |> optional "PRODUCT" string ""
        |> optional "TRADE_ID" string ""
        |> optional "ROLE" string ""
        |> optional "TRADER_NAME" string ""
        |> optional "TRADER_DESK_CODE" string ""
        |> optional "TRADER_COMPANY" string ""
        |> optional "COUNTERPARTY_NAME" string ""
        |> optional "COUNTERPARTY_DESK_CODE" string ""
        |> optional "COUNTERPARTY_COMPANY" string ""
        |> optional "DEAL_DATE_TIME" string ""
        |> optional "TRADE_DATE" string ""
        |> optional "START_DATE" string ""
        |> optional "TERMINATION_DATE" string ""
        |> optional "SIDE" string ""
        |> optional "TICKER" string ""
        |> optional "SECURITY_DESC" string ""
        |> optional "TRADE_TYPE" string ""
        |> optional "LEG_NUMBER" string ""
        |> optional "IDENTIFIER" string ""
        |> optional "CALCULATION_TYPE" string ""
        |> optional "FLOATING_REF_PRICE" string ""
        |> optional "QUANTITY" string ""
        |> optional "QUANTITY_UNIT" string ""
        |> optional "PERIODICITY" string ""
        |> optional "FIXED_PRICE" string ""
        |> optional "CURRENCY" string ""
        |> optional "PRICE_IN" string ""
        |> optional "NEAR_LEG_FIXED_PRICE" string ""
        |> optional "MID_PRICE" string ""
        |> optional "NOTIONAL" string ""
        |> optional "SETTLEMENT_DATE" string ""
        |> optional "SETTLEMENT_CCY" string ""
        |> optional "MARKET_TYPE" string ""
        |> optional "FIXING_SOURCE" string ""
        |> optional "FIXING_DATE" string ""
        |> optional "REGISTRATION" string ""
        |> optional "DELIVERY_LOCATION" string ""
        |> optional "NOTES" string ""
        |> optional "COMPETING_QUOTES" string ""
        |> optional "SAVINGS" string ""
        |> optional "EXECUTION_VENUE" string ""
        |> optional "VENUE_NAME" string ""
        |> optional "TRADER_LEI" string ""
        |> optional "COUNTERPARTY_LEI" string ""
        |> optional "VENUE_EXECUTION_FEE" string ""
        |> optional "USI_UTI_NAMESPACE" string ""
        |> optional "USI_UTI_ID" string ""
        |> optional "REPORTING_PARTY" string ""
        |> optional "CLIENT_ORDER_ID" string ""
        |> optional "ACCOUNT_NAME" string ""
        |> optional "ACCOUNT_DESC" string ""
        |> optional "ACCOUNT_SIDE" string ""
        |> optional "ACCOUNT_VOLUME" string ""



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
