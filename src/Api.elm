module Api exposing (getTeam, getTeam2)

import DatabaseApiToken exposing (DatabaseApiToken)
import Effect exposing (Effect)
import Http
import Json.Decode exposing (Decoder)
import Task exposing (Task)
import Team exposing (Team)
import Url.Builder as Url


getTeam : (Result Http.Error Team -> msg) -> DatabaseApiToken -> String -> Effect msg
getTeam msg apiKey teamShortName =
    getFromDatabaseTask
        { apiKey = apiKey
        , table = "team"
        , query =
            [ Url.string "shortname" ("eq." ++ teamShortName)
            , Url.string "select" "navn,id,shortname"
            ]
        , decoder = listToSingleElementDecoder Team.decoder
        }
        |> Task.andThen (getTeammedlemmer apiKey)
        |> Task.attempt msg
        |> Effect.sendCmd


getTeammedlemmer : DatabaseApiToken -> Team -> Task Http.Error Team
getTeammedlemmer apiKey team =
    getFromDatabaseTask
        { apiKey = apiKey
        , table = "teammedlem"
        , query =
            [ Url.string "team_id" ("eq." ++ Team.id team)
            , Url.string "select" "navn"
            ]
        , decoder =
            Team.teammedlemmerDecoder
                |> Json.Decode.map (Team.addMedlemmer team)
        }


getTeam2 : (Result Http.Error Team -> msg) -> DatabaseApiToken -> String -> Effect msg
getTeam2 msg apiKey teamShortName =
    getFromDatabase
        { apiKey = apiKey
        , table = "teammedlem"
        , query =
            [ Url.string "team.shortname" ("eq." ++ teamShortName)
            , Url.string "select" "navn,team(shortname,navn)"
            ]
        , expect = Http.expectJson msg Team.decoder
        }


listToSingleElementDecoder : Decoder a -> Decoder a
listToSingleElementDecoder decoder =
    Json.Decode.list decoder
        |> Json.Decode.andThen
            (\decodedList ->
                case decodedList of
                    singleElement :: [] ->
                        Json.Decode.succeed singleElement

                    [] ->
                        Json.Decode.fail "Listen returnerte ingen elementer"

                    _ ->
                        Json.Decode.fail "Listen returnerte flere enn ett element"
            )


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

                Http.BadStatus_ metadata body ->
                    Err (Http.BadStatus metadata.statusCode)

                Http.GoodStatus_ metadata body ->
                    Json.Decode.decodeString decoder body
                        |> Result.mapError (Json.Decode.errorToString >> Http.BadBody)
        )
