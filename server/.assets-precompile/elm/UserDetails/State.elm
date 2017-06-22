module UserDetails.State exposing (..)

import UserDetails.Types exposing (Model, Msg(..))
import Common.Types exposing (User, Position)
import UserDetails.Rest exposing (userDecoder)
import UserDetails.Headers as Headers
import RemoteData
import Utils.Http
import Utils.Activities.Main as Activities
import Time exposing (Time)
import Json.Decode


init : { todayDate : Time, userJson : String } -> ( Model, Cmd Msg )
init { todayDate, userJson } =
    let
        decoded =
            Json.Decode.decodeString userDecoder userJson
    in
        case decoded of
            Ok user ->
                let
                    ( activities, activitiesMsg ) =
                        Activities.initFromUser todayDate user.id

                    content =
                        { user = user
                        , activities = activities
                        }
                in
                    (Just content) ! [ Cmd.map ActivitiesMsg activitiesMsg, Headers.set user ]

            _ ->
                Nothing ! []


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( ActivitiesMsg dMsg, Just content ) ->
            Activities.update dMsg content.activities
                |> Tuple.mapFirst (\a -> Just { content | activities = a })
                |> Tuple.mapSecond (Cmd.map ActivitiesMsg)

        _ ->
            model ! []
