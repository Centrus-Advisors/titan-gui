module Project.Deliverables.Rest exposing (fetchDeliverables)

import Project.Deliverables.Types exposing (..)
import Common.Decoders
import Json.Decode exposing (Decoder, string)
import Utils.Api as Api exposing (defaultSearchObj)
import Utils.Routes as Routes
import Utils.Http
import RemoteData exposing (WebData)


deliverableContent =
    { fields = []
    , embed = [ "owner", "created_by" ]
    }


deliverableDecoder : Decoder Deliverable
deliverableDecoder =
    Common.Decoders.deliverableStructure string Common.Decoders.userS


fetchDeliverables : String -> Cmd (WebData (List Deliverable))
fetchDeliverables projectID =
    let
        searchObj =
            { defaultSearchObj
                | search = projectID
                , search_fields = [ "project" ]
                , embed = [ "owner", "created_by" ]
                , max_results = 200
            }
    in
        Api.search deliverableDecoder searchObj (Routes.deliverableApi "")
            |> Utils.Http.attemptWithRemoteData
