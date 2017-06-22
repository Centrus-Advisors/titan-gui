port module InputForm.State exposing (init, update, subscriptions)

import InputForm.Types exposing (..)
import RemoteData
import Time exposing (Time)
import DatePicker


init : { todayDate : Time } -> ( Model, Cmd Msg )
init _ =
    { records = form } ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    model ! []


subscriptions model =
    Sub.none


validate : DBType -> Result String ()
validate v =
    case v of
        DBString nullable maxLen mContent ->
            ifNotNull nullable
                mContent
                (\txt ->
                    if String.length txt > maxLen then
                        Err <| "This field exceeds the maximum amount of " ++ (toString maxLen) ++ " characters"
                    else
                        Ok ()
                )

        DBTimeStamp nullable datePicker ->
            ifNotNull nullable (DatePicker.getDate datePicker) (always <| Ok ())

        DBDate nullable datePicker ->
            ifNotNull nullable (DatePicker.getDate datePicker) (always <| Ok ())

        DBNumber nullable mInt ->
            ifNotNull nullable mInt (always <| Ok ())

        DBFloat nullable mFloat ->
            ifNotNull nullable mFloat (always <| Ok ())


ifNotNull canBeNull mValue f =
    case mValue of
        Nothing ->
            if not canBeNull then
                Err "This field cannot be empty"
            else
                Ok ()

        Just value ->
            f value


form =
    [ ( "RECORD_TYPE", "Record Type", DBString True 50 Nothing )
    , ( "PRODUCT", "Product", DBString True 50 Nothing )
    , ( "TRADE_ID", "Trade Id", DBString False 50 Nothing )
    , ( "ROLE", "Role", DBString True 50 Nothing )
    , ( "TRADER_NAME", "Trader Name", DBString True 50 Nothing )
    , ( "TRADER_DESK_CODE", "Trader Desk Code", DBString True 50 Nothing )
    , ( "TRADER_COMPANY", "Trader Company", DBString True 57 Nothing )
    , ( "COUNTERPARTY_NAME", "Counterparty Name", DBString True 53 Nothing )
    , ( "COUNTERPARTY_DESK_CODE", "Counterparty Desk Code", DBString True 50 Nothing )
    , ( "COUNTERPARTY_COMPANY", "Counterparty Company", DBString True 57 Nothing )
    , ( "DEAL_DATE_TIME", "Deal Date Time", DBTimeStamp False initialDbDate )
    , ( "TRADE_DATE", "Trade Date", DBDate False initialDbDate )
    , ( "START_DATE", "Start Date", DBDate True initialDbDate )
    , ( "TERMINATION_DATE", "Termination Date", DBDate True initialDbDate )
    , ( "SIDE", "Side", DBString True 50 Nothing )
    , ( "TICKER", "Ticker", DBString True 56 Nothing )
    , ( "SECURITY_DESC", "Security Desc", DBString True 50 Nothing )
    , ( "TRADE_TYPE", "Trade Type", DBString True 50 Nothing )
    , ( "LEG_NUMBER", "Leg Number", DBString True 50 Nothing )
    , ( "IDENTIFIER", "Identifier", DBString True 50 Nothing )
    , ( "CALCULATION_TYPE", "Calculation Type", DBString True 50 Nothing )
    , ( "FLOATING_REF_PRICE", "Floating Ref Price", DBString True 50 Nothing )
    , ( "QUANTITY", "Quantity", DBNumber True Nothing )
    , ( "QUANTITY_UNIT", "Quantity Unit", DBString True 50 Nothing )
    , ( "PERIODICITY", "Periodicity", DBString True 50 Nothing )
    , ( "FIXED_PRICE", "Fixed Price", DBFloat True Nothing )
    , ( "CURRENCY", "Currency", DBString True 50 Nothing )
    , ( "PRICE_IN", "Price In", DBString True 50 Nothing )
    , ( "NEAR_LEG_FIXED_PRICE", "Near Leg Fixed Price", DBNumber True Nothing )
    , ( "MID_PRICE", "Mid Price", DBNumber True Nothing )
    , ( "NOTIONAL", "Notional", DBNumber True Nothing )
    , ( "SETTLEMENT_DATE", "Settlement Date", DBDate True initialDbDate )
    , ( "SETTLEMENT_CCY", "Settlement Ccy", DBString True 50 Nothing )
    , ( "MARKET_TYPE", "Market Type", DBString True 50 Nothing )
    , ( "FIXING_SOURCE", "Fixing Source", DBString True 50 Nothing )
    , ( "FIXING_DATE", "Fixing Date", DBDate True initialDbDate )
    , ( "REGISTRATION", "Registration", DBString True 50 Nothing )
    , ( "DELIVERY_LOCATION", "Delivery Location", DBString True 50 Nothing )
    , ( "NOTES", "Notes", DBString True 50 Nothing )
    , ( "COMPETING_QUOTES", "Competing Quotes", DBString True 53 Nothing )
    , ( "SAVINGS", "Savings", DBString True 53 Nothing )
    , ( "EXECUTION_VENUE", "Execution Venue", DBString True 50 Nothing )
    , ( "VENUE_NAME", "Venue Name", DBString True 50 Nothing )
    , ( "TRADER_LEI", "Trader Lei", DBString True 50 Nothing )
    , ( "COUNTERPARTY_LEI", "Counterparty Lei", DBString True 50 Nothing )
    , ( "VENUE_EXECUTION_FEE", "Venue Execution Fee", DBString True 50 Nothing )
    , ( "USI_UTI_NAMESPACE", "Usi Uti Namespace", DBString True 50 Nothing )
    , ( "USI_UTI_ID", "Usi Uti Id", DBString True 50 Nothing )
    , ( "REPORTING_PARTY", "Reporting Party", DBString True 50 Nothing )
    , ( "CLIENT_ORDER_ID", "Client Order Id", DBString True 50 Nothing )
    , ( "ACCOUNT_NAME", "Account Name", DBString True 50 Nothing )
    , ( "ACCOUNT_DESC", "Account Desc", DBString True 50 Nothing )
    , ( "ACCOUNT_SIDE", "Account Side", DBString True 50 Nothing )
    , ( "ACCOUNT_VOLUME", "Account Volume", DBString True 50 Nothing )
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
