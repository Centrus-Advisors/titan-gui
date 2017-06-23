module InputForm.Types exposing (..)

import RemoteData exposing (WebData)
import Date exposing (Date)
import DatePicker exposing (DatePicker)


type alias Model =
    { records : List ( String, String, DBType )
    , validate : Bool
    , submission : WebData Trade
    }


type alias Index =
    Int


type Msg
    = DoNothing
    | ChangeDate Index DatePicker.Msg
    | ChangeRecord Index String
    | Submit
    | SubmissionInfo (WebData Trade)


type alias MaxLength =
    Int


type alias Nullable =
    Bool


type DBType
    = DBString Nullable MaxLength String
    | DBTimeStamp Nullable String
    | DBDate Nullable DatePicker
    | DBNumber Nullable String
    | DBFloat Nullable String


type alias Trade =
    { record_type : String
    , product : String
    , trade_id : String
    , role : String
    , trader_name : String
    , trader_desk_code : String
    , trader_company : String
    , counterparty_name : String
    , counterparty_desk_code : String
    , counterparty_company : String
    , deal_date_time : String
    , trade_date : String
    , start_date : String
    , termination_date : String
    , side : String
    , ticker : String
    , security_desc : String
    , trade_type : String
    , leg_number : String
    , identifier : String
    , calculation_type : String
    , floating_ref_price : String
    , quantity : String
    , quantity_unit : String
    , periodicity : String
    , fixed_price : String
    , currency : String
    , price_in : String
    , near_leg_fixed_price : String
    , mid_price : String
    , notional : String
    , settlement_date : String
    , settlement_ccy : String
    , market_type : String
    , fixing_source : String
    , fixing_date : String
    , registration : String
    , delivery_location : String
    , notes : String
    , competing_quotes : String
    , savings : String
    , execution_venue : String
    , venue_name : String
    , trader_lei : String
    , counterparty_lei : String
    , venue_execution_fee : String
    , usi_uti_namespace : String
    , usi_uti_id : String
    , reporting_party : String
    , client_order_id : String
    , account_name : String
    , account_desc : String
    , account_side : String
    , account_volume : String
    }
