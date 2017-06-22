module Utils.Routes exposing (..)

-- API


projectApi id =
    "/api/projects/" ++ id


contactApi id =
    "/api/contacts/" ++ id


companyApi id =
    "/api/companies/" ++ id


deliverableApi id =
    "/api/deliverables/" ++ id


activityApi id =
    "/api/activities/" ++ id


activityTagApi id =
    "/api/activityTags/" ++ id


positionApi id =
    "/api/positions/" ++ id


userApi id =
    "/api/users/" ++ id


teamApi id =
    "/api/teams/" ++ id


timeRecordingApi id =
    "/api/timeRecordings/" ++ id


projectCostApi id =
    "/api/cost/project/" ++ id


deliverableCostApi id =
    "/api/cost/deliverable/" ++ id


userDeliverablesApi =
    "/api/users-deliverables/"



-- Datatables


companyDataTables =
    "/contacts/api.company-list"


contactDataTables =
    "/contacts/api.contact-list"


positionDataTables =
    "/api/positions-datatables"


deliverablesDataTables =
    "/api/deliverables-datatables"


activitiesDataTables =
    "/api/activities-datatables"



-- Pages


projectPage id =
    "/projects/project-details/" ++ id


contactPage id =
    "/contacts/contact-details/" ++ id


teamPage id =
    "/staff/team-details/" ++ id


userPage id =
    "/staff/user-details/" ++ id


companyPage id =
    "/contacts/company-details/" ++ id


deliverablePage id =
    "/projects/deliverable-details/" ++ id



--- Other


dropboxCredentials =
    "/file-storage/dropbox/credentials/"


dropboxCredentialsAuthorise =
    "/file-storage/dropbox/auth/"
