module InputForm.Types exposing (..)

import RemoteData exposing (WebData)
import Date exposing (Date)
import DatePicker exposing (DatePicker)


type alias Model =
    { records : List ( String, String, DBType )
    , validate : Bool
    }


type alias Index =
    Int


type Msg
    = DoNothing
    | ChangeDate Index DatePicker.Msg
    | ChangeRecord Index String


type alias MaxLength =
    Int


type alias Nullable =
    Bool


type DBType
    = DBString Nullable MaxLength String
    | DBTimeStamp Nullable DatePicker
    | DBDate Nullable DatePicker
    | DBNumber Nullable String
    | DBFloat Nullable String



{-
   "RECORD_TYPE",              NVARCHAR2(50),
   "PRODUCT",                  NVARCHAR2(50),
   "TRADE_ID",                 NVARCHAR2(50)  NOT NULL,
   "ROLE",                     NVARCHAR2(50),
   "TRADER_NAME",              NVARCHAR2(50),
   "TRADER_DESK_CODE",         NVARCHAR2(50),
   "TRADER_COMPANY",           NVARCHAR2(57),
   "COUNTERPARTY_NAME",        NVARCHAR2(53),
   "COUNTERPARTY_DESK_CODE",   NVARCHAR2(50),
   "COUNTERPARTY_COMPANY",     NVARCHAR2(57),
   "DEAL_DATE_TIME",           TIMESTAMP(6)   NOT NULL,
   "TRADE_DATE",               DATE           NOT NULL,
   "START_DATE",               DATE,
   "TERMINATION_DATE",         DATE,
   "SIDE",                     NVARCHAR2(50),
   "TICKER",                   NVARCHAR2(56),
   "SECURITY_DESC",            NVARCHAR2(50),
   "TRADE_TYPE",               NVARCHAR2(50),
   "LEG_NUMBER",               NVARCHAR2(50),
   "IDENTIFIER",               NVARCHAR2(50),
   "CALCULATION_TYPE",         NVARCHAR2(50),
   "FLOATING_REF_PRICE",       NVARCHAR2(50),
   "QUANTITY",                 NUMBER,
   "QUANTITY_UNIT",            NVARCHAR2(50),
   "PERIODICITY",              NVARCHAR2(50),
   "FIXED_PRICE",              NUMBER(28,10),
   "CURRENCY",                 NVARCHAR2(50),
   "PRICE_IN",                 NVARCHAR2(50),
   "NEAR_LEG_FIXED_PRICE",     NUMBER,
   "MID_PRICE",                NUMBER,
   "NOTIONAL",                 NUMBER,
   "SETTLEMENT_DATE",          DATE,
   "SETTLEMENT_CCY",           NVARCHAR2(50),
   "MARKET_TYPE",              NVARCHAR2(50),
   "FIXING_SOURCE",            NVARCHAR2(50),
   "FIXING_DATE",              DATE,
   "REGISTRATION",             NVARCHAR2(50),
   "DELIVERY_LOCATION",        NVARCHAR2(50),
   "NOTES",                    NVARCHAR2(50),
   "COMPETING_QUOTES",         NVARCHAR2(53),
   "SAVINGS",                  NVARCHAR2(53),
   "EXECUTION_VENUE",          NVARCHAR2(50),
   "VENUE_NAME",               NVARCHAR2(50),
   "TRADER_LEI",               NVARCHAR2(50),
   "COUNTERPARTY_LEI",         NVARCHAR2(50),
   "VENUE_EXECUTION_FEE",      NVARCHAR2(50),
   "USI_UTI_NAMESPACE",        NVARCHAR2(50),
   "USI_UTI_ID",               NVARCHAR2(50),
   "REPORTING_PARTY",          NVARCHAR2(50),
   "CLIENT_ORDER_ID",          NVARCHAR2(50),
   "ACCOUNT_NAME",             NVARCHAR2(50),
   "ACCOUNT_DESC",             NVARCHAR2(50),
   "ACCOUNT_SIDE",             NVARCHAR2(50),
   "ACCOUNT_VOLUME",           NVARCHAR2(50)
-}
