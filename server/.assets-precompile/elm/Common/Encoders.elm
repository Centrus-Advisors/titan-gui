module Common.Encoders exposing (..)

import Common.Types exposing (..)
import Json.Encode exposing (string)


encodeDeliverableType : DeliverableType -> Json.Encode.Value
encodeDeliverableType t =
    string <|
        case t of
            DReport ->
                "report"

            DMeeting ->
                "meeting"

            DAnalysis ->
                "analysis"

            DPhone ->
                "phone call"

            DEmail ->
                "email"

            DBusinessDevelopment ->
                "business development"

            DServiceTicket ->
                "service ticket"

            DTechnical ->
                "technical"

            DOther ->
                "other"


encodeProjectType : ProjectType -> Json.Encode.Value
encodeProjectType t =
    string <|
        case t of
            Retainer ->
                "retainer"

            Standard ->
                "standard"


encodeUserPermission : Permission -> Json.Encode.Value
encodeUserPermission t =
    string <|
        case t of
            ADMINISTRATOR ->
                "ADMINISTRATOR"

            PRIVILEGED ->
                "PRIVILEGED"

            STANDARD ->
                "STANDARD"
