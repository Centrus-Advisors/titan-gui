module Common.Decoders exposing (..)

import Common.Types exposing (..)
import Json.Decode
    exposing
        ( Decoder
        , string
        , bool
        , at
        , nullable
        , float
        , list
        , int
        )
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Decode.Extra exposing (date, fromResult)


adtDecoder : (String -> Decoder a) -> Decoder a
adtDecoder stringToType =
    string |> Json.Decode.andThen stringToType



-- DELIVERABLE


deliverableS : Decoder DeliverableS
deliverableS =
    deliverableStructure string string


deliverable : Decoder Deliverable
deliverable =
    deliverableStructure projectS userS


deliverableStructure : Decoder project -> Decoder user -> Decoder (DeliverableStructure project user)
deliverableStructure projectDecoder userDecoder =
    decode DeliverableStructure
        |> required "_id" string
        |> required "title" string
        |> optional "milestone" string ""
        |> optional "type" (adtDecoder deliverableType) DOther
        |> required "description" string
        |> required "status" (adtDecoder deliverableStatue)
        |> optional "hourly_rate" floatToString ""
        |> required "startDate" date
        |> optional "deadline" (nullable date) Nothing
        |> required "owner" userDecoder
        |> required "assignees" (list userDecoder)
        |> required "project" projectDecoder
        |> required "created_by" userDecoder


deliverableType : String -> Decoder DeliverableType
deliverableType v =
    case v of
        "report" ->
            Json.Decode.succeed DReport

        "meeting" ->
            Json.Decode.succeed DMeeting

        "analysis" ->
            Json.Decode.succeed DAnalysis

        "phone call" ->
            Json.Decode.succeed DPhone

        "email" ->
            Json.Decode.succeed DEmail

        "business development" ->
            Json.Decode.succeed DBusinessDevelopment

        "service ticket" ->
            Json.Decode.succeed DServiceTicket

        "technical" ->
            Json.Decode.succeed DTechnical

        "other" ->
            Json.Decode.succeed DOther

        _ ->
            Json.Decode.fail <| "Invalid type: '" ++ v ++ "'"


deliverableStatue : String -> Decoder DeliverableStatus
deliverableStatue v =
    case v of
        "completed" ->
            Json.Decode.succeed Completed

        "cancelled" ->
            Json.Decode.succeed Cancelled

        "active" ->
            Json.Decode.succeed Active

        _ ->
            Json.Decode.fail <| "Invalid status: '" ++ v ++ "'"


floatToString : Decoder String
floatToString =
    Json.Decode.andThen
        (\intVal -> Json.Decode.succeed <| toString intVal)
        float



-- PROJECT


projectS : Decoder ProjectS
projectS =
    projectStructure string string string


project : Decoder Project
project =
    projectStructure userS companyS contactS


projectStructure :
    Decoder user
    -> Decoder company
    -> Decoder contact
    -> Decoder (ProjectStructure user company contact)
projectStructure userDecoder companyDecoder contactDecoder =
    decode ProjectStructure
        |> required "_id" string
        |> required "name" string
        |> required "description" string
        |> required "type" (adtDecoder projectType)
        |> required "startDate" date
        |> optional "endDate" (nullable date) Nothing
        |> optional "budget" floatToString ""
        |> required "requiredFiles" (list string)
        |> optional "notes" string ""
        |> required "company" companyDecoder
        |> required "owner" userDecoder
        |> required "contacts" (list (projectContact contactDecoder))
        |> required "rates" (list positionRate)
        |> required "created_by" userDecoder


projectType : String -> Decoder ProjectType
projectType v =
    case v of
        "retainer" ->
            Json.Decode.succeed Retainer

        "standard" ->
            Json.Decode.succeed Standard

        _ ->
            Json.Decode.fail <| "Invalid project type: " ++ v


projectContact : Decoder contact -> Decoder (ProjectContact contact)
projectContact contactDecoder =
    decode ProjectContact
        |> required "description" string
        |> required "contact" contactDecoder


positionRate : Decoder PositionRate
positionRate =
    decode PositionRate
        |> required "position" position
        |> required "hourly_rate" floatToString



-- POSITION


position : Decoder Position
position =
    decode Position
        |> required "_id" string
        |> required "name" string



-- CONTACT


contactS : Decoder ContactS
contactS =
    contactStructure string string


contact =
    contactStructure companyS userS


contactStructure : Decoder company -> Decoder user -> Decoder (ContactStructure company user)
contactStructure companyDecoder userDecoder =
    decode ContactStructure
        |> required "_id" string
        |> required "name" string
        |> optional "company" (nullable companyDecoder) Nothing
        |> optional "position" string ""
        |> required "requiredFiles" (list string)
        |> optional "phones" (list phone) []
        |> optional "email" string ""
        |> optional "gender" string ""
        |> optional "address" string ""
        |> optional "location" string ""
        |> optional "notes" string ""
        |> required "createdAt" string
        |> required "created_by" userDecoder


phone : Decoder Phone
phone =
    decode Phone
        |> required "number" string
        |> required "description" string



-- USER


userS : Decoder UserS
userS =
    userStructure string string string


user : Decoder User
user =
    userStructure position contactS teamS


userStructure : Decoder position -> Decoder contact -> Decoder team -> Decoder (UserStructure position contact team)
userStructure positionDecoder contactDecoder teamDecoder =
    decode UserStructure
        |> required "_id" string
        |> required "email" string
        |> required "active" bool
        |> required "permission" (adtDecoder userPermission)
        |> optional "position" (nullable positionDecoder) Nothing
        |> optional "contact" (nullable contactDecoder) Nothing
        |> optional "team" (nullable teamDecoder) Nothing


userPermission : String -> Decoder Permission
userPermission v =
    case v of
        "ADMINISTRATOR" ->
            Json.Decode.succeed ADMINISTRATOR

        "PRIVILEGED" ->
            Json.Decode.succeed PRIVILEGED

        "STANDARD" ->
            Json.Decode.succeed STANDARD

        _ ->
            Json.Decode.fail <| "Invalid user permission level: " ++ v



-- USER


teamS : Decoder TeamS
teamS =
    teamStructure string


team : Decoder Team
team =
    teamStructure userS


teamStructure : Decoder user -> Decoder (TeamStructure user)
teamStructure userDecoder =
    decode TeamStructure
        |> required "_id" string
        |> required "name" string
        |> optional "leader" (nullable userDecoder) Nothing



-- COMPANY


companyS : Decoder CompanyS
companyS =
    companyStructure string string


company : Decoder Company
company =
    companyStructure contactS userS


companyStructure : Decoder contact -> Decoder user -> Decoder (CompanyStructure contact user)
companyStructure contactDecoder userDecoder =
    decode CompanyStructure
        |> required "_id" string
        |> optional "name" string ""
        |> optional "location" string ""
        |> optional "phones" (list phone) []
        |> optional "address" string ""
        |> optional "postcode" string ""
        |> optional "email" string ""
        |> required "sector" string
        |> required "classification" string
        |> required "investorClassification" string
        |> required "investorType" string
        |> optional "website" string ""
        |> required "requiredFiles" (list string)
        |> optional "relatedContacts" (list <| companyContact contactDecoder) []
        |> optional "archived" bool False
        |> optional "notes" string ""
        |> required "created_by" userDecoder


companyContact contactDecoder =
    decode CompanyContact
        |> required "relation" string
        |> required "contact" (nullable contactDecoder)



-- Cost


cost : Decoder Cost
cost =
    decode Cost
        |> required "_id" string
        |> required "cost" float
        |> required "duration" int



-- TimeRecording


timeRecording =
    timeRecordingStructure project deliverable user


timeRecordingStructure : Decoder project -> Decoder deliverable -> Decoder user -> Decoder (TimeRecordingStructure project deliverable user)
timeRecordingStructure projectDecoder deliverableDecoder userDecoder =
    decode TimeRecordingStructure
        |> required "_id" string
        |> required "project" projectDecoder
        |> required "deliverable" deliverableDecoder
        |> required "startDate" date
        |> required "duration" int
        |> required "costBreakdown" costBreakdown
        |> required "cost" float
        |> required "created_by" userDecoder
        |> required "createdAt" date


costBreakdown : Decoder CostBreakdown
costBreakdown =
    decode CostBreakdown
        |> optional "positionRate" (nullable float) Nothing
        |> optional "projectPositionRate" (nullable float) Nothing
        |> optional "deliverableRate" (nullable float) Nothing
