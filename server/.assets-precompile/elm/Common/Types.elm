module Common.Types exposing (..)

import Date exposing (Date)


-- Project


type alias ProjectS =
    ProjectStructure String String String


type alias ProjectStructure user company contact =
    { id : String
    , name : String
    , description : String
    , type_ : ProjectType
    , startDate : Date
    , endDate : Maybe Date
    , budget : String
    , requiredFiles : List String
    , notes : String
    , company : company
    , owner : user
    , contacts : List (ProjectContact contact)
    , rates : List PositionRate
    , created_by : user
    }


type alias Project =
    ProjectStructure UserS CompanyS ContactS


type ProjectType
    = Retainer
    | Standard


type alias ProjectContact contact =
    { description : String
    , contact : contact
    }


type alias PositionRate =
    { position : Position
    , hourly_rate : String
    }



-- Position


type alias Position =
    { id : String
    , name : String
    }



-- Deliverable


type alias DeliverableStructure project user =
    { id : String
    , title : String
    , milestone : String
    , type_ : DeliverableType
    , description : String
    , status : DeliverableStatus
    , hourly_rate : String
    , startDate : Date
    , deadline : Maybe Date
    , owner : user
    , assignees : List user
    , project : project
    , created_by : user
    }


type alias DeliverableS =
    DeliverableStructure String String


type alias Deliverable =
    DeliverableStructure ProjectS UserS


type DeliverableStatus
    = Completed
    | Cancelled
    | Active


type DeliverableType
    = DReport
    | DMeeting
    | DAnalysis
    | DPhone
    | DEmail
    | DBusinessDevelopment
    | DServiceTicket
    | DTechnical
    | DOther



-- Contact


type alias ContactS =
    ContactStructure String String


type alias Contact =
    ContactStructure CompanyS UserS


type alias ContactStructure company user =
    { id : String
    , name : String
    , company : Maybe company
    , position : String
    , requiredFiles : List String
    , phones : List Phone
    , email : String
    , gender : String
    , address : String
    , location : String
    , notes : String
    , createdAt : String
    , created_by : user
    }


type alias Phone =
    { number : String
    , description : String
    }



-- User


type alias UserS =
    UserStructure String String String


type alias User =
    UserStructure Position ContactS TeamS


type alias UserStructure position contact team =
    { id : String
    , email : String
    , active : Bool
    , permission : Permission
    , position : Maybe position
    , contact : Maybe contact
    , team : Maybe team
    }


type Permission
    = ADMINISTRATOR
    | STANDARD
    | PRIVILEGED



-- Team


type alias TeamS =
    TeamStructure String


type alias Team =
    TeamStructure UserS


type alias TeamStructure user =
    { id : String
    , name : String
    , leader : Maybe user
    }



-- Company


type alias CompanyS =
    CompanyStructure String String


type alias Company =
    CompanyStructure ContactS UserS


type alias CompanyStructure contact user =
    { id : String
    , name : String
    , location : String
    , phones : List Phone
    , address : String
    , postcode : String
    , email : String
    , sector : String
    , classification : String
    , investorClassification : String
    , investorType : String
    , website : String
    , requiredFiles : List String
    , relatedContacts : List (CompanyContact contact)
    , archived : Bool
    , notes : String
    , created_by : user
    }


type alias CompanyContact contact =
    { relation : String
    , contact : Maybe contact
    }



-- Cost


type alias Cost =
    { id : String
    , cost : Float
    , duration : Int
    }



-- Cost


type alias TimeRecordingS =
    TimeRecordingStructure String String String


type alias TimeRecording =
    TimeRecordingStructure Project Deliverable User


type alias TimeRecordingStructure project deliverable user =
    { id : String
    , project : project
    , deliverable : deliverable
    , startDate : Date
    , duration : Int
    , costBreakdown : CostBreakdown
    , cost : Float
    , createdBy : user
    , createdAt : Date
    }


type alias CostBreakdown =
    { positionRate : Maybe Float
    , projectPositionRate : Maybe Float
    , deliverableRate : Maybe Float
    }
