module Utils.Api exposing (SearchObj, SearchData, fetch, search, searchWithMetadata, create, save, delete, errorMessage, defaultSearchObj, Content)

import Utils.Http
import Http
import Json.Decode.Pipeline exposing (decode, required, optional)
import Json.Decode exposing (Decoder, string, list)
import Json.Encode
import Task exposing (Task)
import Http
import Dict


--------------------------------------------------------------------------
--  ENDPOINTS
--------------------------------------------------------------------------


type alias Content =
    { fields : List String
    , embed : List String
    }


type alias SearchObj =
    { search : String
    , search_fields : List String
    , fields : List String
    , embed : List String
    , sort : String
    , max_results : Int
    , page : Int
    , filters : List ( String, String )
    }


type alias SearchData a =
    { results : List a
    , totalDocuments : Int
    }


search : Decoder a -> SearchObj -> String -> Task Http.Error (List a)
search decoder searchObj endpoint =
    Utils.Http.get
        (Json.Decode.list decoder)
        (buildUrl endpoint searchObj)


searchWithMetadata : Decoder a -> SearchObj -> String -> Task Http.Error (SearchData a)
searchWithMetadata decoder searchObj endpoint =
    let
        getTotalDocuments =
            (\response ->
                Dict.get "x-search-document-count" response.headers
                    |> Result.fromMaybe "No document count in headers"
                    |> Result.andThen String.toInt
            )

        getResults =
            (\response ->
                Json.Decode.decodeString (Json.Decode.list decoder) response.body
            )

        responseParser =
            (\response ->
                Result.map2
                    (\results totalDocuments -> { results = results, totalDocuments = totalDocuments })
                    (getResults response)
                    (getTotalDocuments response)
            )
    in
        Utils.Http.getResponse
            responseParser
            (buildUrl endpoint searchObj)


fetch : Decoder a -> Content -> String -> Task Http.Error a
fetch decoder content endpoint =
    Utils.Http.get
        decoder
        (buildUrlFromContent endpoint content)


create : Decoder a -> Content -> String -> Http.Body -> Task Http.Error a
create decoder content endpoint body =
    Utils.Http.post
        decoder
        (buildUrlFromContent endpoint content)
        body


save : Decoder a -> Content -> String -> Http.Body -> Task Http.Error a
save decoder content endpoint body =
    Utils.Http.put
        decoder
        (buildUrlFromContent endpoint content)
        body


delete : String -> Task Http.Error ()
delete endpoint =
    Utils.Http.delete
        (buildUrlFromContent endpoint { fields = [], embed = [] })


defaultSearchObj =
    { search = ""
    , search_fields = []
    , filters = []
    , fields = []
    , embed = []
    , sort = ""
    , max_results = 20
    , page = 0
    }


buildUrl : String -> SearchObj -> String
buildUrl url s =
    let
        joinWith joiner aList =
            List.intersperse joiner aList
                |> List.foldl (++) ""

        filters =
            s.filters
                |> List.map
                    (\( f, v ) ->
                        "filters[" ++ (Http.encodeUri f) ++ "]=" ++ (Http.encodeUri v)
                    )

        urlParams =
            [ (++) "search=" <| Http.encodeUri s.search
            , (++) "search_fields=" <| joinWith "," s.search_fields
            , (++) "fields=" <| joinWith "," <| List.map Http.encodeUri s.fields
            , (++) "embed=" <| joinWith "," <| List.map Http.encodeUri s.embed
            , (++) "sort=" s.sort
            , (++) "max_results=" <| toString s.max_results
            , (++) "page=" <| toString s.page
            ]
                |> (++) filters
                |> joinWith "&"
    in
        url ++ "?" ++ urlParams


buildUrlFromContent url content =
    buildUrl url { defaultSearchObj | fields = content.fields, embed = content.embed }



--------------------------------------------------------------------------
--  ERROR MESSAGE
--------------------------------------------------------------------------


errorMessage : Http.Error -> String
errorMessage error =
    case error of
        Http.BadUrl wrongUrl ->
            "Invalid url: " ++ wrongUrl

        Http.Timeout ->
            "The server didn't respond on time. Please try again"

        Http.NetworkError ->
            "Unable to connect to server"

        Http.BadPayload errMessage { status } ->
            "Unable to parse server response: " ++ errMessage

        Http.BadStatus { status, body } ->
            let
                decodedError =
                    if (status.code > 399) && (status.code < 500) then
                        decodeValidationError body
                    else
                        decodeServerError body
            in
                decodedError
                    |> Result.withDefault (unknownError status body)


unknownError status body =
    "Server returned " ++ (toString status.code) ++ ". " ++ status.message


type alias FieldValidationError =
    { field : String
    , message : String
    }


type alias ValidationError =
    { message : String
    , errors : List FieldValidationError
    }


decodeValidationError : String -> Result String String
decodeValidationError body =
    let
        listFieldErrors errors =
            errors
                |> List.map (\err -> err.field ++ ": " ++ err.message)
                |> List.foldl (\a b -> a ++ "\n" ++ b) "\n"
    in
        Json.Decode.decodeString
            validationErrorDecoder
            body
            |> Result.map (\err -> err.message ++ (listFieldErrors err.errors))


fieldValidationErrorDecoder : Decoder FieldValidationError
fieldValidationErrorDecoder =
    decode FieldValidationError
        |> required "field" string
        |> required "message" string


validationErrorDecoder : Decoder ValidationError
validationErrorDecoder =
    decode ValidationError
        |> required "message" string
        |> optional "errors" (list fieldValidationErrorDecoder) []


type alias ServerError =
    { error : String
    }


serverErrorDecoder : Decoder ServerError
serverErrorDecoder =
    decode ServerError
        |> required "error" string


decodeServerError : String -> Result String String
decodeServerError body =
    Json.Decode.decodeString
        serverErrorDecoder
        body
        |> Result.map (\err -> err.error)
