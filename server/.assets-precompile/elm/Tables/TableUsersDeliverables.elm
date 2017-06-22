module TableUsersDeliverables exposing (..)

import Html
import Utils.Table.Main as Table
import Common.Types exposing (Deliverable)
import Common.Decoders as Decoders
import Common.View exposing (deliverableStatus)
import Json.Decode exposing (string)
import Html exposing (text, a, div)
import Html.Attributes exposing (href, class)
import RemoteData exposing (WebData)
import Task
import Utils.Api as Api
import Utils.Routes as Routes
import Utils.Http
import Utils.Time exposing (dayMonthYearLong)
import Date exposing (Date)


{-
   IMPORTANT:
   For this table to work, make sure to add a { users: [ USER_ID ] } filter
   in the initiation flag
-}
-- main : Program Never Model Msg


main =
    Html.programWithFlags
        { init = init
        , view = Table.view
        , update = Table.update
        , subscriptions = Table.subscriptions
        }



-- init : Json.Decode.Value -> String


init jsonVal =
    let
        flags =
            decodeFlags jsonVal
    in
        case flags of
            Ok f ->
                Table.init (tableConfig f.todayDate) f.filters

            Err err ->
                Table.init (tableConfig dummyDate) jsonVal


dummyDate =
    Date.fromTime 0


decodeFlags jsonVal =
    let
        toFlagObject =
            (\todayDate filters ->
                { todayDate = Date.fromTime todayDate
                , filters = filters
                }
            )

        decoder =
            Json.Decode.map2 toFlagObject
                (Json.Decode.field "todayDate" Json.Decode.float)
                (Json.Decode.field "filters" Json.Decode.value)
    in
        Json.Decode.decodeValue decoder jsonVal


tableConfig : Date -> Table.Config Deliverable
tableConfig todayDate =
    { columns =
        [ { title = "Title"
          , fieldName = "title"
          , renderCell = \d -> a [ href <| Routes.deliverablePage d.id ] [ text d.title ]
          }
        , { title = "Project"
          , fieldName = "project"
          , renderCell = \d -> a [ href <| Routes.projectPage d.project.id ] [ text d.project.name ]
          }
        , { title = "Owner"
          , fieldName = "owner"
          , renderCell = \p -> a [ href <| Routes.userPage p.owner.id ] [ text p.owner.email ]
          }
        , { title = "Start Date"
          , fieldName = "startDate"
          , renderCell = .startDate >> dayMonthYearLong >> text
          }
        , { title = "Deadline"
          , fieldName = "deadline"
          , renderCell = .deadline >> Maybe.map dayMonthYearLong >> Maybe.withDefault "" >> text
          }
        , { title = "Status"
          , fieldName = "status"
          , renderCell =
                (\d ->
                    div [ class "text-center" ]
                        [ deliverableStatus todayDate d ]
                )
          }
        ]
    , filters =
        [ Table.textFilter "Title" (\v -> [ ( "title", v ) ])
        ]
    , defaultSortColumn = "deadline"
    , search = searchDeliverables
    }


searchDeliverables searchObj =
    let
        newSearchObj =
            { searchObj | embed = [ "project", "owner", "created_by", "assignees" ] }

        decoder =
            Decoders.deliverable
    in
        Api.searchWithMetadata decoder newSearchObj Routes.userDeliverablesApi
            |> Utils.Http.attemptWithRemoteData
