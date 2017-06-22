module Common.View
    exposing
        ( costView
        , timeRecordingView
        , deliverableStatus
        , deliverableTypeSelector
        , deliverableTypeToString
        )

import Common.Types exposing (..)
import Html exposing (Html, div, a, text, p, strong, span, i, select, option)
import Html.Attributes exposing (class, style, href, selected)
import Html.Events exposing (on, targetValue)
import Json.Decode
import Utils.Number
import Utils.Routes as Routes
import Utils.Time
    exposing
        ( intervalDescription
        , timeOfDay
        )
import Date.Extra
import Date exposing (Date)
import Time


------------------------------------------
--- COST VIEW
--- used by Project and Deliverable
------------------------------------------


costView : Cost -> Html msg
costView cost =
    timeAndCost
        (intervalDescription <| toFloat cost.duration)
        ("£" ++ (Utils.Number.pretty 2 ',' '.' cost.cost))


timeAndCost : String -> String -> Html msg
timeAndCost time cost =
    div
        [ class "row  projectDetails-buttonGroup" ]
        [ div [ class "col-lg-6" ]
            [ div [ class "btn-group btn-group-justified m-b-10" ]
                [ a [ class "btn" ]
                    [ text " Total Time" ]
                , a [ class "btn" ]
                    [ text time ]
                ]
            ]
        , div [ class "col-lg-6" ]
            [ div [ class "btn-group btn-group-justified m-b-10" ]
                [ a [ class "btn" ]
                    [ text " Total Cost" ]
                , a [ class "btn" ]
                    [ text cost ]
                ]
            ]
        ]



------------------------------------------
--- TIME RECORDING VIEW
--- used by Topbar and Deliverable.TimeTracking
------------------------------------------


timeRecordingView :
    TimeRecordingStructure project deliverable (UserStructure s ContactS s)
    -> List (Html msg)
    -> Html msg
timeRecordingView recording footerContent =
    let
        endTime =
            Date.toTime recording.startDate
                |> floor
                |> (+) recording.duration
                |> toFloat
                |> Date.fromTime
    in
        div [ class "activity-item" ]
            [ div
                [ class "activity-item-icon activity-item-icon-clock" ]
                [ i [ class "fa fa-clock-o" ] [] ]
            , p
                [ class "clearfix"
                , style [ ( "font-size", "20px" ) ]
                ]
                [ strong
                    []
                    [ text <| intervalDescription (toFloat recording.duration) ]
                , span
                    [ class "pull-right font-13 text-muted" ]
                    [ text <| timeOfDay recording.startDate
                    , text " - "
                    , text <| timeOfDay endTime
                    ]
                ]
            , p
                []
                [ userLink recording.createdBy
                , span
                    [ class "pull-right font-13 text-muted" ]
                    [ text <| costDescription recording.cost
                    ]
                ]
            , timeline recording
            , div [] footerContent
            ]


timeline recording =
    let
        oneDay =
            floor <| Time.hour * 24

        percentOfDay =
            recording.duration
                |> (\x -> (x * 100) // oneDay)
                |> max 1

        uncappedOffsetLeft =
            recording.startDate
                |> Date.Extra.floor Date.Extra.Day
                |> (\x -> Date.Extra.diff Date.Extra.Millisecond x recording.startDate)
                |> (\x -> (x * 100) // oneDay)

        offsetLeft =
            --  Here we are enforcing this: uncappedOffsetLeft + percentOfDay <= 100
            min
                (100 - percentOfDay)
                uncappedOffsetLeft
    in
        div
            [ class "timeline"
            , style
                [ ( "height", "3px" )
                , ( "position", "relative" )
                , ( "background-color", "#f0f0f0" )
                , ( "overflow", "hidden" )
                ]
            ]
            [ div
                [ class "time-recorded"
                , style
                    [ ( "position", "absolute" )
                    , ( "top", "0" )
                    , ( "height", "3px" )
                    , ( "background-color", "#f05a28" )
                    , ( "width", (toString percentOfDay) ++ "%" )
                    , ( "left", (toString offsetLeft) ++ "%" )
                    ]
                ]
                []
            ]


costDescription : Float -> String
costDescription cost =
    cost
        |> (*) 100
        |> floor
        |> toString
        |> String.padLeft 3 '0'
        |> (\x -> (String.dropRight 2 x) ++ "." ++ (String.right 2 x))
        |> (++) "£"


userLink user =
    a
        [ href <| Routes.userPage user.id ]
        [ text <| userName user ]


userName user =
    user
        |> .contact
        |> Maybe.map .name
        |> Maybe.withDefault user.email



------------------------------------------
--- DELIVERABLE VIEW
--- Project and Deliverable
------------------------------------------


deliverableStatus : Date -> DeliverableStructure a b -> Html msg
deliverableStatus todayDate d =
    case d.status of
        Completed ->
            span [ class "label label-lg label-success" ] [ text "Completed" ]

        Cancelled ->
            span [ class "label label-lg label-inverse" ] [ text "Cancelled" ]

        Active ->
            case Date.Extra.compare todayDate d.startDate of
                LT ->
                    span [ class "label label-lg label-default" ] [ text "Not Started" ]

                _ ->
                    case d.deadline of
                        Nothing ->
                            span [ class "label label-lg label-primary" ] [ text "Ongoing" ]

                        Just deadline ->
                            case Date.Extra.compare todayDate deadline of
                                LT ->
                                    span [ class "label label-lg label-primary" ] [ text "Ongoing" ]

                                _ ->
                                    span [ class "label label-lg label-danger" ] [ text "Late" ]


deliverableTypeToString : DeliverableType -> String
deliverableTypeToString t =
    case t of
        DReport ->
            "Report"

        DMeeting ->
            "Meeting"

        DAnalysis ->
            "Analysis"

        DPhone ->
            "Phone"

        DEmail ->
            "Email"

        DBusinessDevelopment ->
            "Business Service"

        DServiceTicket ->
            "Service"

        DTechnical ->
            "Technical"

        DOther ->
            "Other"


stringToDeliverableType : String -> DeliverableType
stringToDeliverableType t =
    case t of
        "Report" ->
            DReport

        "Meeting" ->
            DMeeting

        "Analysis" ->
            DAnalysis

        "Phone" ->
            DPhone

        "Email" ->
            DEmail

        "Business Service" ->
            DBusinessDevelopment

        "Service" ->
            DServiceTicket

        "Technical" ->
            DTechnical

        _ ->
            DOther


deliverableTypes =
    [ DReport
    , DMeeting
    , DAnalysis
    , DPhone
    , DEmail
    , DBusinessDevelopment
    , DServiceTicket
    , DTechnical
    , DOther
    ]


deliverableTypeSelector msg selectedType =
    let
        toOption dType =
            option [ selected (dType == selectedType) ] [ text <| deliverableTypeToString dType ]
    in
        select
            [ class "form-control"
            , on "change" (Json.Decode.map (stringToDeliverableType >> msg) targetValue)
            ]
            (List.map toOption deliverableTypes)
