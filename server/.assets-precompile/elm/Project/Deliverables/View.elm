module Project.Deliverables.View exposing (root)

import Project.Deliverables.Types exposing (..)
import Common.Types exposing (DeliverableStatus(Cancelled, Completed, Active))
import Common.View exposing (deliverableStatus)
import Html exposing (Html, div, text, strong, thead, tbody, h4, h3, tr, th, td, input, label, table, a, span, button, select, option)
import Html.Attributes exposing (class, href, value, style, type_, selected, placeholder, disabled)
import Html.Events exposing (onInput, onClick, on, targetValue)
import Utils.Routes as Routes
import Date.Extra
import Date exposing (Date)
import Table exposing (defaultCustomizations)
import Utils.SearchBox as SearchBox
import DatePicker
import Utils.Ui as Ui
import RemoteData exposing (WebData)
import Utils.Time exposing (weekDayDayMonthYear)
import Json.Decode
import NewDeliverable.Main as NewDeliverable


root : Date -> Model -> Html Msg
root todayDate model =
    div [ class "card-box table-responsive" ]
        [ newDeliverableView model
        , Ui.showWhenLoaded (Table.view (tableConfig todayDate) model.tableState) model.deliverables
        ]


tableConfig : Date -> Table.Config Deliverable Msg
tableConfig todayDate =
    Table.customConfig
        { toId = .id
        , toMsg = SetTableState
        , columns =
            [ Table.veryCustomColumn
                { name = "Title"
                , viewData =
                    \d ->
                        { attributes = []
                        , children =
                            [ a
                                [ href <| Routes.deliverablePage d.id ]
                                [ text d.title ]
                            ]
                        }
                , sorter = Table.increasingOrDecreasingBy .title
                }
            , Table.stringColumn "Milestone" .milestone
            , Table.veryCustomColumn
                { name = "Owner"
                , viewData =
                    \d ->
                        { attributes = []
                        , children =
                            [ a
                                [ href <| Routes.userPage d.owner.id ]
                                [ text d.owner.email ]
                            ]
                        }
                , sorter = Table.increasingOrDecreasingBy (.owner >> .email)
                }
            , Table.customColumn
                { name = "Start Date"
                , viewData = .startDate >> weekDayDayMonthYear
                , sorter = Table.increasingOrDecreasingBy (.startDate >> Date.toTime)
                }
            , Table.customColumn
                { name = "Deadline"
                , viewData = (.deadline >> Maybe.map weekDayDayMonthYear >> Maybe.withDefault "")
                , sorter = Table.increasingOrDecreasingBy (.deadline >> Maybe.map Date.toTime >> Maybe.withDefault 9999999999999)
                }
            , Table.veryCustomColumn
                { name = "Status"
                , viewData =
                    \d ->
                        { attributes = [ class "text-center" ]
                        , children = [ deliverableStatus todayDate d ]
                        }
                , sorter = Table.unsortable
                }
            ]
        , customizations =
            { defaultCustomizations
                | tableAttrs = [ class "table table-striped table-bordered dt-responsive nowrap" ]
                , thead = defaultCustomizations.thead >> (\c -> { c | children = [ tr [] c.children ] })
            }
        }


newDeliverableView : Model -> Html Msg
newDeliverableView model =
    div []
        [ button
            [ class "btn btn-sm btn-inverted waves-effect w-md waves-light m-b-5 pull-right"
            , onClick (ShowNewDeliverable True)
            ]
            [ text "New Deliverable" ]
        , case model.new of
            Nothing ->
                text ""

            Just new ->
                NewDeliverable.view new
                    |> Html.map NewDeliverableUpdate
        ]
