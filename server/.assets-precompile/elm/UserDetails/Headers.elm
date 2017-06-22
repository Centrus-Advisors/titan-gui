port module UserDetails.Headers exposing (set)

import Common.Types exposing (User)
import List.Extra
import Utils.Routes as Routes


-- Sets the page headers in JQuery


type alias HeaderDetails =
    { name : String
    , email : String
    , position : String
    , team : String
    }



-- port for sending headers to JavaScript


set : User -> Cmd msg
set user =
    setHeaders
        { email = user.email
        , name = Maybe.withDefault "" <| Maybe.map .name user.contact
        , position = Maybe.withDefault "" <| Maybe.map .name user.position
        , team = Maybe.withDefault "" <| Maybe.map .name user.team
        }


port setHeaders : HeaderDetails -> Cmd msg
