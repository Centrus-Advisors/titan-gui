module UserDetails.Rest exposing (..)

import Common.Decoders
import Common.Types
    exposing
        ( User
        , UserStructure
        , ContactS
        , TeamS
        , Position
        )
import Json.Decode exposing (Decoder, string, list, at, nullable, bool)
import Json.Decode.Pipeline exposing (decode, required, requiredAt, optional)
import Utils.Http
import Utils.Api as Api
import Utils.Routes as Routes
import RemoteData exposing (WebData)


userDecoder : Decoder User
userDecoder =
    Common.Decoders.user


userContent : Api.Content
userContent =
    { fields = []
    , embed = [ "contact", "team", "position" ]
    }


fetchUser : String -> Cmd (WebData User)
fetchUser userId =
    Api.fetch userDecoder userContent (Routes.userApi userId)
        |> Utils.Http.attemptWithRemoteData
