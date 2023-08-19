module Api exposing (getTeam, getTeamsForUser)

import AccessToken exposing (AccessToken)
import DatabaseApiToken exposing (DatabaseApiToken)
import Effect exposing (Effect)
import Http
import Json.Decode exposing (Decoder)
import Task exposing (Task)
import Team exposing (Team)
import TeamSummary exposing (TeamSummary)
import Teammedlem
import Url.Builder as Url


getTeam : (Result Team.Error Team -> msg) -> DatabaseApiToken -> String -> Effect msg
getTeam msg apiKey teamShortName =
    getFromDatabaseTask
        { apiKey = apiKey
        , table = "team"
        , query =
            [ Url.string "shortname" ("eq." ++ teamShortName)
            , Url.string "select" "name,id,shortname,rotation_length,proper_random,owner_id"
            ]
        , decoder = decodeTeamFromList
        }
        |> Task.mapError Team.HttpErrorForTeam
        |> Task.andThen (getTeammedlemmer apiKey)
        |> Task.attempt msg
        |> Effect.sendCmd


getTeammedlemmer : DatabaseApiToken -> Result Team.Error TeamSummary -> Task Team.Error Team
getTeammedlemmer apiKey teamSummaryResult =
    case teamSummaryResult of
        Ok teamSummary ->
            getFromDatabaseTask
                { apiKey = apiKey
                , table = "team_member"
                , query =
                    [ Url.string "team_id" ("eq." ++ TeamSummary.id teamSummary)
                    , Url.string "select" "name,id"
                    ]
                , decoder =
                    Json.Decode.list Teammedlem.decoder
                        |> Json.Decode.map (Team.init teamSummary)
                }
                |> Task.mapError Team.HttpErrorForTeammedlemmer

        Err error ->
            Task.fail error


decodeTeamFromList : Decoder (Result Team.Error TeamSummary)
decodeTeamFromList =
    Json.Decode.list TeamSummary.decoder
        |> Json.Decode.andThen
            (\decodedList ->
                case decodedList of
                    singleElement :: [] ->
                        Json.Decode.succeed (Ok singleElement)

                    [] ->
                        Json.Decode.succeed (Err Team.FantIkkeTeam)

                    _ ->
                        Json.Decode.fail "Listen returnerte flere enn ett element"
            )


getTeamsForUser : DatabaseApiToken -> (Result Http.Error (List TeamSummary) -> msg) -> AccessToken -> Effect msg
getTeamsForUser apiKey msg accessToken =
    getFromDatabase
        { apiKey = apiKey
        , table = "team"
        , query =
            [ Url.string "owner_id" ("eq." ++ AccessToken.userId accessToken)
            , Url.string "select" "name,id,shortname,rotation_length,proper_random,owner_id"
            ]
        , expect = Http.expectJson msg (Json.Decode.list TeamSummary.decoder)
        }


getFromDatabase :
    { apiKey : DatabaseApiToken
    , table : String
    , query : List Url.QueryParameter
    , expect : Http.Expect msg
    }
    -> Effect msg
getFromDatabase { apiKey, table, query, expect } =
    Http.request
        { method = "GET"
        , headers = [ Http.header "apikey" (DatabaseApiToken.toString apiKey) ]
        , url =
            Url.crossOrigin
                "https://xluvzigagcthclpwrzhj.supabase.co"
                [ "rest", "v1", table ]
                query
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }
        |> Effect.sendCmd


getFromDatabaseTask :
    { apiKey : DatabaseApiToken
    , table : String
    , query : List Url.QueryParameter
    , decoder : Decoder a
    }
    -> Task Http.Error a
getFromDatabaseTask { apiKey, table, query, decoder } =
    Http.task
        { method = "GET"
        , headers = [ Http.header "apikey" (DatabaseApiToken.toString apiKey) ]
        , url =
            Url.crossOrigin
                "https://xluvzigagcthclpwrzhj.supabase.co"
                [ "rest", "v1", table ]
                query
        , body = Http.emptyBody
        , resolver = jsonResolver decoder
        , timeout = Nothing
        }


jsonResolver : Decoder a -> Http.Resolver Http.Error a
jsonResolver decoder =
    Http.stringResolver
        (\response ->
            case response of
                Http.BadUrl_ s ->
                    Err (Http.BadUrl s)

                Http.Timeout_ ->
                    Err Http.Timeout

                Http.NetworkError_ ->
                    Err Http.NetworkError

                Http.BadStatus_ metadata _ ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ _ body ->
                    Json.Decode.decodeString decoder body
                        |> Result.mapError (Json.Decode.errorToString >> Http.BadBody)
        )
