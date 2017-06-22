module InputForm.State exposing (init, update, subscriptions, validateType, entireFormIsValid)

import InputForm.Types exposing (..)
import RemoteData
import Time exposing (Time)
import DatePicker
import List.Extra


init : { todayDate : Time } -> ( Model, Cmd Msg )
init _ =
    { records = form
    , validate = False
    , submission = RemoteData.NotAsked
    }
        ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DoNothing ->
            model ! []

        ChangeDate idx pickerMsg ->
            let
                mPicker =
                    model.records
                        |> List.Extra.getAt idx
                        |> Maybe.map (\( a, b, dbType ) -> dbType)
                        |> Maybe.andThen getPicker
            in
                case mPicker of
                    Nothing ->
                        model ! []

                    Just oldPicker ->
                        let
                            ( newPicker, newPickerCmd, _ ) =
                                DatePicker.update pickerMsg oldPicker

                            records =
                                List.Extra.updateIfIndex
                                    ((==) idx)
                                    (tupleMapThird (updateDatePicker newPicker))
                                    model.records
                        in
                            { model | records = records }
                                ! [ newPickerCmd |> Cmd.map (ChangeDate idx) ]

        ChangeRecord idx val ->
            let
                records =
                    List.Extra.updateIfIndex
                        ((==) idx)
                        (tupleMapThird (updateRecord val))
                        model.records
            in
                { model | records = records } ! []

        Submit ->
            if entireFormIsValid model.records then
                { model | submission = RemoteData.Success () } ! []
            else
                { model | validate = True } ! []


tupleMapThird : (c -> c) -> ( a, b, c ) -> ( a, b, c )
tupleMapThird f ( a, b, c ) =
    ( a, b, f c )


updateDatePicker : DatePicker.DatePicker -> DBType -> DBType
updateDatePicker newDatePicker dbType =
    case dbType of
        DBTimeStamp nullable datePicker ->
            DBTimeStamp nullable newDatePicker

        DBDate nullable datePicker ->
            DBDate nullable newDatePicker

        _ ->
            dbType


updateRecord : String -> DBType -> DBType
updateRecord val dbType =
    case dbType of
        DBTimeStamp nullable datePicker ->
            dbType

        DBDate nullable datePicker ->
            dbType

        DBString nullable maxLen oldVal ->
            DBString nullable maxLen val

        DBNumber nullable oldVal ->
            DBNumber nullable val

        DBFloat nullable oldVal ->
            DBFloat nullable val


getPicker : DBType -> Maybe DatePicker.DatePicker
getPicker v =
    case v of
        DBTimeStamp nullable datePicker ->
            Just datePicker

        DBDate nullable datePicker ->
            Just datePicker

        _ ->
            Nothing


subscriptions model =
    Sub.none



--------------
-- Form fields
--------------


entireFormIsValid : List ( String, String, DBType ) -> Bool
entireFormIsValid form =
    form
        |> List.map (\( a, b, c ) -> c)
        |> List.map validateType
        |> List.foldl (\elValid outcome -> Result.andThen (always elValid) outcome) (Ok ())
        |> Result.map (always True)
        |> Result.withDefault False


validateType : DBType -> Result String ()
validateType v =
    case v of
        DBString nullable maxLen val ->
            ifNotNull nullable
                val
                (\txt ->
                    if String.length txt > maxLen then
                        Err <| "This field exceeds the maximum amount of " ++ (toString maxLen) ++ " characters"
                    else
                        Ok ()
                )

        DBTimeStamp nullable datePicker ->
            emptyPicker nullable datePicker

        DBDate nullable datePicker ->
            emptyPicker nullable datePicker

        DBNumber nullable val ->
            if nullable && String.isEmpty val then
                Ok ()
            else
                case String.toInt val of
                    Ok _ ->
                        Ok ()

                    Err _ ->
                        Err <| "Could not convert \"" ++ val ++ "\" to integer. Please insert a valid number"

        DBFloat nullable val ->
            if nullable && String.isEmpty val then
                Ok ()
            else
                case String.toFloat val of
                    Ok _ ->
                        Ok ()

                    Err _ ->
                        Err <| "Could not convert \"" ++ val ++ "\" to float. Please insert a valid number"


ifNotNull canBeNull val f =
    if String.isEmpty val && not canBeNull then
        Err "This field cannot be empty"
    else
        f val


emptyPicker nullable datePicker =
    case DatePicker.getDate datePicker of
        Just aDate ->
            Ok ()

        Nothing ->
            if nullable then
                Ok ()
            else
                Err "This field cannot be empty. Please choose a date"


form =
    [ ( "RECORD_TYPE", "Record Type", DBString True 50 "" )
    , ( "PRODUCT", "Product", DBString True 50 "" )
    , ( "TRADE_ID", "Trade Id", DBString False 50 "" )
    , ( "ROLE", "Role", DBString True 50 "" )
    , ( "TRADER_NAME", "Trader Name", DBString True 50 "" )
    , ( "TRADER_DESK_CODE", "Trader Desk Code", DBString True 50 "" )
    , ( "TRADER_COMPANY", "Trader Company", DBString True 57 "" )
    , ( "COUNTERPARTY_NAME", "Counterparty Name", DBString True 53 "" )
    , ( "COUNTERPARTY_DESK_CODE", "Counterparty Desk Code", DBString True 50 "" )
    , ( "COUNTERPARTY_COMPANY", "Counterparty Company", DBString True 57 "" )
    , ( "DEAL_DATE_TIME", "Deal Date Time", DBTimeStamp False initialDbDate )
    , ( "TRADE_DATE", "Trade Date", DBDate False initialDbDate )
    , ( "START_DATE", "Start Date", DBDate True initialDbDate )
    , ( "TERMINATION_DATE", "Termination Date", DBDate True initialDbDate )
    , ( "SIDE", "Side", DBString True 50 "" )
    , ( "TICKER", "Ticker", DBString True 56 "" )
    , ( "SECURITY_DESC", "Security Desc", DBString True 50 "" )
    , ( "TRADE_TYPE", "Trade Type", DBString True 50 "" )
    , ( "LEG_NUMBER", "Leg Number", DBString True 50 "" )
    , ( "IDENTIFIER", "Identifier", DBString True 50 "" )
    , ( "CALCULATION_TYPE", "Calculation Type", DBString True 50 "" )
    , ( "FLOATING_REF_PRICE", "Floating Ref Price", DBString True 50 "" )
    , ( "QUANTITY", "Quantity", DBNumber True "" )
    , ( "QUANTITY_UNIT", "Quantity Unit", DBString True 50 "" )
    , ( "PERIODICITY", "Periodicity", DBString True 50 "" )
    , ( "FIXED_PRICE", "Fixed Price", DBFloat True "" )
    , ( "CURRENCY", "Currency", DBString True 50 "" )
    , ( "PRICE_IN", "Price In", DBString True 50 "" )
    , ( "NEAR_LEG_FIXED_PRICE", "Near Leg Fixed Price", DBNumber True "" )
    , ( "MID_PRICE", "Mid Price", DBNumber True "" )
    , ( "NOTIONAL", "Notional", DBNumber True "" )
    , ( "SETTLEMENT_DATE", "Settlement Date", DBDate True initialDbDate )
    , ( "SETTLEMENT_CCY", "Settlement Ccy", DBString True 50 "" )
    , ( "MARKET_TYPE", "Market Type", DBString True 50 "" )
    , ( "FIXING_SOURCE", "Fixing Source", DBString True 50 "" )
    , ( "FIXING_DATE", "Fixing Date", DBDate True initialDbDate )
    , ( "REGISTRATION", "Registration", DBString True 50 "" )
    , ( "DELIVERY_LOCATION", "Delivery Location", DBString True 50 "" )
    , ( "NOTES", "Notes", DBString True 50 "" )
    , ( "COMPETING_QUOTES", "Competing Quotes", DBString True 53 "" )
    , ( "SAVINGS", "Savings", DBString True 53 "" )
    , ( "EXECUTION_VENUE", "Execution Venue", DBString True 50 "" )
    , ( "VENUE_NAME", "Venue Name", DBString True 50 "" )
    , ( "TRADER_LEI", "Trader Lei", DBString True 50 "" )
    , ( "COUNTERPARTY_LEI", "Counterparty Lei", DBString True 50 "" )
    , ( "VENUE_EXECUTION_FEE", "Venue Execution Fee", DBString True 50 "" )
    , ( "USI_UTI_NAMESPACE", "Usi Uti Namespace", DBString True 50 "" )
    , ( "USI_UTI_ID", "Usi Uti Id", DBString True 50 "" )
    , ( "REPORTING_PARTY", "Reporting Party", DBString True 50 "" )
    , ( "CLIENT_ORDER_ID", "Client Order Id", DBString True 50 "" )
    , ( "ACCOUNT_NAME", "Account Name", DBString True 50 "" )
    , ( "ACCOUNT_DESC", "Account Desc", DBString True 50 "" )
    , ( "ACCOUNT_SIDE", "Account Side", DBString True 50 "" )
    , ( "ACCOUNT_VOLUME", "Account Volume", DBString True 50 "" )
    ]


initialDbDate =
    let
        defaultSettings =
            DatePicker.defaultSettings

        ( picker, pickerCmd ) =
            DatePicker.init
                { defaultSettings
                    | pickedDate = Nothing
                    , inputClassList = [ ( "form-control", True ) ]
                }
    in
        picker
