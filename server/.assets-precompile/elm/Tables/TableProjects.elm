module TableProjects exposing (..)

import Html
import Utils.Table.Main as Table
import Common.Types
import Common.Decoders as Decoders
import Json.Decode exposing (string)
import Html exposing (text, a)
import Html.Attributes exposing (href)
import RemoteData exposing (WebData)
import Task
import Utils.Api as Api
import Utils.Routes as Routes
import Utils.Http
import Utils.Time exposing (dayMonthYearLong)


-- main : Program Never Model Msg


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
          , renderCell = \p -> a [ href <| Routes.projectPage p.id ] [ text p.name ]
          }
        , { title = "Company"
          , fieldName = "company"
          , renderCell = \p -> a [ href <| Routes.companyPage p.company.id ] [ text p.company.name ]
          }
        , { title = "Owner"
          , fieldName = "owner"
          , renderCell = \p -> a [ href <| Routes.userPage p.owner.id ] [ text p.owner.email ]
          }
        , { title = "Start Date"
          , fieldName = "startDate"
          , renderCell = .startDate >> dayMonthYearLong >> text
          }
        , { title = "End Date"
          , fieldName = "endDate"
          , renderCell = \p -> p.endDate |> Maybe.map dayMonthYearLong |> Maybe.withDefault "" |> text
          }
        ]
    , filters =
        [ Table.textFilter "Name" (\v -> [ ( "name", v ) ])
        ]
    , defaultSortColumn = "endDate"
    , search = searchProjects
    }


searchProjects searchObj =
    let
        newSearchObj =
            { searchObj | embed = [ "company", "owner", "created_by" ] }

        decoder =
            Decoders.projectStructure Decoders.userS Decoders.companyS string
    in
        Api.searchWithMetadata decoder newSearchObj (Routes.projectApi "")
            |> Utils.Http.attemptWithRemoteData
