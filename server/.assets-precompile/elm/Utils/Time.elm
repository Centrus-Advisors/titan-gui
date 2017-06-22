module Utils.Time
    exposing
        ( intervalWatchNotation
        , intervalDescription
        , timeOfDay
        , weekDayDayMonthYear
        , dayMonthYear
        , timeDifference
        , dayMonthYearLong
        )

import Time exposing (Time)
import Date exposing (Date)
import Date.Extra


oneSecond =
    1000


oneMinute =
    oneSecond * 60


oneHour =
    oneMinute * 60


oneDay =
    oneHour * 24


hourCount time =
    time // oneHour


outstandingMinutes time =
    (time % oneHour) // oneMinute


outstandingSeconds time =
    (time % oneMinute) // oneSecond


intervalWatchNotation : Time -> String
intervalWatchNotation time =
    let
        interval =
            floor time

        timeItems =
            if interval > oneHour then
                [ hourCount interval
                , outstandingMinutes interval
                , outstandingSeconds interval
                ]
            else
                [ outstandingMinutes interval
                , outstandingSeconds interval
                ]
    in
        timeItems
            |> List.map toTwoDigits
            |> listJoin ":"


toTwoDigits : Int -> String
toTwoDigits =
    toString >> (++) "00" >> String.right 2


listJoin : String -> List String -> String
listJoin joint list =
    List.intersperse joint list
        |> List.foldr (++) ""



---------


pluralise word count =
    let
        p =
            (toString count) ++ " " ++ word
    in
        if count > 1 then
            p ++ "s"
        else
            p


intervalDescription : Time -> String
intervalDescription timeInterval =
    let
        interval =
            floor timeInterval

        descriptionItems =
            if interval >= oneHour then
                [ pluralise "hr" <| hourCount interval
                , pluralise "min" <| outstandingMinutes interval
                ]
            else if interval >= oneMinute then
                [ pluralise "min" <| outstandingMinutes interval
                ]
            else
                [ pluralise "second" <| outstandingSeconds interval
                ]
    in
        listJoin " " descriptionItems



---------


timeOfDay : Date -> String
timeOfDay date =
    Date.Extra.toFormattedString "hh:mm a" date


weekDayDayMonthYear : Date -> String
weekDayDayMonthYear date =
    Date.Extra.toFormattedString "EE, MMMM d, y" date


dayMonthYear : Date -> String
dayMonthYear date =
    Date.Extra.toFormattedString "dd MMM YY" date


dayMonthYearLong : Date -> String
dayMonthYearLong date =
    Date.Extra.toFormattedString "dd MMMM YYYY" date


timeDifference : Date -> Date -> Time
timeDifference fromDate toDate =
    Date.toTime toDate - Date.toTime fromDate
