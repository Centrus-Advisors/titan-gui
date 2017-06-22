port module TableCompanies exposing (..)

import Html
import Utils.Table.Main as Table
import Common.Types exposing (CompanyS)
import Common.Decoders as Decoders
import Json.Decode
import Html exposing (text, a)
import Html.Attributes exposing (href)
import RemoteData exposing (WebData)
import Task
import Utils.Api as Api
import Utils.Routes as Routes
import Utils.Http


main =
    Html.programWithFlags
        { init = Table.init tableConfig
        , view = Table.view
        , update = Table.update
        , subscriptions = Table.subscriptions
        }


tableConfig =
    { columns =
        [ { title = "Name"
          , fieldName = "name"
          , renderCell = \c -> a [ href <| Routes.companyPage c.id ] [ text c.name ]
          }
        , { title = "Classification"
          , fieldName = "classification"
          , renderCell = .classification >> text
          }
        , { title = "Investor Type"
          , fieldName = "investorType"
          , renderCell = .investorType >> text
          }
        , { title = "Investor Classification"
          , fieldName = "investorClassification"
          , renderCell = .investorClassification >> text
          }
        , { title = "Sector"
          , fieldName = "sector"
          , renderCell = .sector >> text
          }
        ]
    , filters =
        [ Table.textFilter "Name" (\v -> [ ( "name", v ) ])
        , Table.textFilter "Classification" (\v -> [ ( "classification", v ) ])
        , Table.textFilter "Investor Type" (\v -> [ ( "investorType", v ) ])
        , Table.textFilter "Investor Classification" (\v -> [ ( "investorClassification", v ) ])
        , Table.textFilter "Sector" (\v -> [ ( "sector", v ) ])
        ]
    , defaultSortColumn = "name"
    , search = searchCompanies
    }


searchCompanies searchObj =
    Api.searchWithMetadata Decoders.companyS searchObj (Routes.companyApi "")
        |> Utils.Http.attemptWithRemoteData
