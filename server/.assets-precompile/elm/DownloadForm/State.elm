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

        ( fromPicker, fromPickerCmd ) =
            DatePicker.init
                { defaultSettings
                    | pickedDate = Just <| Date.fromTime todayDate
                    , inputClassList = [ ( "form-control", True ) ]
                    , dateFormatter = Date.Extra.toFormattedString "ddd MMMM yyyy"
                }

        ( toPicker, toPickerCmd ) =
            DatePicker.init
                { defaultSettings
                    | pickedDate = Just <| Date.Extra.add Date.Extra.Day 1 <| Date.fromTime todayDate
                    , inputClassList = [ ( "form-control", True ) ]
                    , dateFormatter = Date.Extra.toFormattedString "ddd MMMM yyyy"
                }
    in
        { fromDate = fromPicker
        , toDate = toPicker
        }
            ! [ Cmd.map FromDate fromPickerCmd
              , Cmd.map ToDate toPickerCmd
              ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FromDate subMsg ->
            let
                ( newPicker, newPickerCmd, _ ) =
                    DatePicker.update subMsg model.fromDate
            in
                { model | fromDate = newPicker } ! [ Cmd.map FromDate newPickerCmd ]

        ToDate subMsg ->
            let
                ( newPicker, newPickerCmd, _ ) =
                    DatePicker.update subMsg model.toDate
            in
                { model | toDate = newPicker } ! [ Cmd.map ToDate newPickerCmd ]


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
