port module ContactDetails.Headers exposing (set)

import Common.Types exposing (Contact)
import List.Extra
import Utils.Routes as Routes


-- Sets the page headers in JQuery


type alias HeaderDetails =
    { name : String
    , companyName : String
    , companyAddress : String
    , phone : String
    , email : String
    , position : String
    }



-- port for sending headers to JavaScript


set : Contact -> Cmd msg
set contact =
    setHeaders
        { name = contact.name
        , companyName = Maybe.map .name contact.company |> Maybe.withDefault ""
        , companyAddress = Maybe.map .id contact.company |> Maybe.map Routes.companyPage |> Maybe.withDefault ""
        , phone = List.Extra.getAt 0 contact.phones |> Maybe.map .number |> Maybe.withDefault ""
        , email = contact.email
        , position = contact.position
        }


port setHeaders : HeaderDetails -> Cmd msg
