module DownloadForm.State exposing (init, update, subscriptions)

import DownloadForm.Types exposing (..)
import RemoteData
import Time exposing (Time)
import DatePicker
import List.Extra
import Date.Extra
import Date


init : { todayDate : Time } -> ( Model, Cmd Msg )
init { todayDate } =
    let
        defaultSettings =
            DatePicker.defaultSettings

        ( picker, pickerCmd ) =
            DatePicker.init
                { defaultSettings
                    | pickedDate = Just <| Date.fromTime todayDate
                    , inputClassList = [ ( "form-control", True ) ]
                    , dateFormatter = Date.Extra.toFormattedString "ddd MMMM yyyy"
                }
    in
        { date = picker }
            ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeDate subMsg ->
            let
                ( newPicker, newPickerCmd, _ ) =
                    DatePicker.update subMsg model.date
            in
                { model | date = newPicker } ! [ Cmd.map ChangeDate newPickerCmd ]


subscriptions model =
    Sub.none



--------------
-- Form fields
--------------


initialDbDate todayDate =
    let
        defaultSettings =
            DatePicker.defaultSettings

        ( picker, pickerCmd ) =
            DatePicker.init
                { defaultSettings
                    | pickedDate = Just <| Date.fromTime todayDate
                    , inputClassList =
                        [ ( "form-control", True )
                        ]
                    , dateFormatter = Date.Extra.toFormattedString "yyyy-MM-dd"
                }
    in
        picker
