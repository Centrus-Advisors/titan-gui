port module TeamDetails.Headers exposing (set)

import TeamDetails.Types exposing (Team)
import List.Extra
import Utils.Routes as Routes


-- Sets the page headers in JQuery


type alias HeaderDetails =
    { name : String
    , leader : String
    }



-- port for sending headers to JavaScript


set : Team -> Cmd msg
set team =
    let
        leader =
            case team.leader of
                Just aLeader ->
                    case aLeader.contact of
                        Just aContact ->
                            aContact.name

                        Nothing ->
                            aLeader.email

                Nothing ->
                    ""
    in
        setHeaders
            { name = team.name
            , leader = leader
            }


port setHeaders : HeaderDetails -> Cmd msg
