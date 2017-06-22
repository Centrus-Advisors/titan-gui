module TableContacts exposing (..)

import Html
import Utils.Table.Main as Table
import Common.Types exposing (ContactStructure, CompanyS)
import Common.Decoders as Decoders
import Json.Decode
import Html exposing (text, a)
import Html.Attributes exposing (href)
import RemoteData exposing (WebData)
import Task
import Utils.Api as Api
import Utils.Routes as Routes
import Utils.Http


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
          , renderCell = \c -> a [ href <| Routes.contactPage c.id ] [ text c.name ]
          }
        , { title = "Company"
          , fieldName = "company"
          , renderCell =
                (\c ->
                    case c.company of
                        Just company ->
                            a [ href <| Routes.companyPage company.id ] [ text company.name ]

                        Nothing ->
                            text ""
                )
          }
        , { title = "Position"
          , fieldName = "position"
          , renderCell = .position >> text
          }
        , { title = "Email"
          , fieldName = "email"
          , renderCell = .email >> text
          }
        , { title = "Phone"
          , fieldName = "phones"
          , renderCell = \c -> List.head c.phones |> Maybe.map .number |> Maybe.withDefault "" |> text
          }
        ]
    , filters =
        [ Table.textFilter "Name" (\v -> [ ( "name", v ) ])
        , Table.textFilter "Position" (\v -> [ ( "position", v ) ])
        , Table.textFilter "Email" (\v -> [ ( "email", v ) ])
        ]
    , defaultSortColumn = "name"
    , search = searchContacts
    }


searchContacts searchObj =
    let
        newSearchObj =
            { searchObj | embed = [ "company" ] }

        decoder =
            Decoders.contactStructure Decoders.companyS Json.Decode.string
    in
        Api.searchWithMetadata decoder newSearchObj (Routes.contactApi "")
            |> Utils.Http.attemptWithRemoteData
