module Utils.Activities.ActivityList.Rest exposing (..)

import Utils.Activities.Rest exposing (activityDecoder)
import Utils.Activities.CommonTypes exposing (..)
import Utils.Api as Api
import Utils.Routes as Routes
import Utils.Http
import RemoteData exposing (WebData)


fetchActivities : Api.SearchObj -> Cmd (WebData (List Activity))
fetchActivities searchObj =
    Api.search activityDecoder searchObj (Routes.activityApi "")
        |> Utils.Http.attemptWithRemoteData


deleteActivity : String -> Cmd (WebData ())
deleteActivity id =
    let
        content =
            { fields = [], embed = [] }
    in
        Api.delete (Routes.activityApi id)
            |> Utils.Http.attemptWithRemoteData
