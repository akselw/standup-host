module Api exposing
    ( checkSlugUniqueness
    , createTeam
    , getTeam
    , getTeamsForUser
    , leggTilTeammedlem
    , slettTeammedlem
    , updateTeam
    , updateTeammedlemNavn
    )

import AccessToken exposing (AccessToken)
import DatabaseApiToken exposing (DatabaseApiToken)
import Effect exposing (Effect)
import Http
import Json.Decode exposing (Decoder)
import Json.Encode
import LeggTilTeamForm exposing (ValidatedLeggTilTeamForm)
import SlugUniqueness exposing (SlugUniquenessCheck)
import Task exposing (Task)
import Team exposing (Team)
import TeamId
import TeamSettingsForm exposing (TeamSettingsForm, ValidatedTeamSettingsForm)
import TeamSummary exposing (TeamSummary)
import Teammedlem exposing (Teammedlem)
import Url.Builder as Url
import UserId


getTeam : (Result Team.Error Team -> msg) -> DatabaseApiToken -> String -> Effect msg
getTeam msg apiKey teamShortName =
    getFromDatabaseTask
        { apiKey = apiKey
        , table = "team"
        , query =
            [ Url.string "slug" ("eq." ++ teamShortName)
            , Url.string "select" "name,id,slug,rotation_length,proper_random,owner_id"
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
                    [ Url.string "team_id" ("eq." ++ (TeamSummary.id >> TeamId.toString) teamSummary)
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
    let
        userId =
            accessToken
                |> AccessToken.userId
                |> UserId.toString
    in
    getFromDatabase
        { apiKey = apiKey
        , table = "team"
        , query =
            [ Url.string "owner_id" ("eq." ++ userId)
            , Url.string "select" "name,id,slug,rotation_length,proper_random,owner_id"
            ]
        , expect = Http.expectJson msg (Json.Decode.list TeamSummary.decoder)
        }


checkSlugUniqueness : DatabaseApiToken -> (Result Http.Error SlugUniquenessCheck -> msg) -> String -> Effect msg
checkSlugUniqueness apiKey msg slug =
    getFromDatabase
        { apiKey = apiKey
        , table = "team"
        , query =
            [ Url.string "slug" ("eq." ++ slug)
            , Url.string "select" "slug"
            ]
        , expect = Http.expectJson msg SlugUniqueness.decoder
        }


updateTeammedlemNavn : DatabaseApiToken -> (Result Http.Error Teammedlem -> msg) -> AccessToken -> Teammedlem -> String -> Effect msg
updateTeammedlemNavn apiKey msg accessToken teammedlem oppdatertNavn =
    updateInDatabase
        { apiKey = apiKey
        , accessToken = accessToken
        , table = "team_member"
        , query =
            [ Url.string "id" ("eq." ++ Teammedlem.id teammedlem)
            , Url.string "select" "name,id"
            ]
        , body =
            Json.Encode.object [ ( "name", Json.Encode.string oppdatertNavn ) ]
        , expect = expectSingleElement msg Teammedlem.decoder
        }


slettTeammedlem : DatabaseApiToken -> (Result Http.Error () -> msg) -> AccessToken -> Teammedlem -> Effect msg
slettTeammedlem apiKey msg accessToken teammedlem =
    deleteFromDatabase
        { apiKey = apiKey
        , accessToken = accessToken
        , table = "team_member"
        , id = Teammedlem.id teammedlem
        , msg = msg
        }


leggTilTeammedlem : DatabaseApiToken -> (Result Http.Error Teammedlem -> msg) -> AccessToken -> Team -> String -> Effect msg
leggTilTeammedlem apiKey msg accessToken team navn =
    addToDatabase
        { apiKey = apiKey
        , accessToken = accessToken
        , table = "team_member"
        , query = []
        , body =
            Json.Encode.object
                [ ( "name", Json.Encode.string navn )
                , ( "team_id", (Team.id >> TeamId.encode) team )
                ]
        , expect = expectSingleElement msg Teammedlem.decoder
        }


updateTeam : DatabaseApiToken -> (Result Http.Error TeamSummary -> msg) -> AccessToken -> ValidatedTeamSettingsForm -> Effect msg
updateTeam apiKey msg accessToken form =
    updateInDatabase
        { apiKey = apiKey
        , accessToken = accessToken
        , table = "team"
        , query =
            [ Url.string "id" ("eq." ++ TeamSettingsForm.teamId form)
            , Url.string "select" "name,id,slug,rotation_length,proper_random,owner_id"
            ]
        , body = TeamSettingsForm.encode form
        , expect = expectSingleElement msg TeamSummary.decoder
        }


createTeam : DatabaseApiToken -> (Result Http.Error TeamSummary -> msg) -> AccessToken -> ValidatedLeggTilTeamForm -> Effect msg
createTeam apiKey msg accessToken form =
    addToDatabase
        { apiKey = apiKey
        , accessToken = accessToken
        , table = "team"
        , query =
            [ Url.string "select" "name,id,slug,rotation_length,proper_random,owner_id"
            ]
        , body =
            accessToken
                |> AccessToken.userId
                |> LeggTilTeamForm.encode form
        , expect = expectSingleElement msg TeamSummary.decoder
        }


expectSingleElement : (Result Http.Error a -> msg) -> Decoder a -> Http.Expect msg
expectSingleElement msg decoder =
    Http.expectJson msg
        (Json.Decode.list decoder
            |> Json.Decode.andThen
                (\decodedList ->
                    case decodedList of
                        singleElement :: [] ->
                            Json.Decode.succeed singleElement

                        [] ->
                            Json.Decode.fail "Listen er tom, forventet ett element"

                        _ ->
                            Json.Decode.fail "Listen inneholdt flere enn ett element"
                )
        )


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


addToDatabase :
    { apiKey : DatabaseApiToken
    , accessToken : AccessToken
    , table : String
    , query : List Url.QueryParameter
    , body : Json.Encode.Value
    , expect : Http.Expect msg
    }
    -> Effect msg
addToDatabase { apiKey, accessToken, table, query, body, expect } =
    Http.request
        { method = "POST"
        , headers =
            [ Http.header "apikey" (DatabaseApiToken.toString apiKey)
            , Http.header "Authorization" ("Bearer " ++ AccessToken.token accessToken)
            , Http.header "Prefer" "return=representation"
            ]
        , url =
            Url.crossOrigin
                "https://xluvzigagcthclpwrzhj.supabase.co"
                [ "rest", "v1", table ]
                query
        , body = Http.jsonBody body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }
        |> Effect.sendCmd


updateInDatabase :
    { apiKey : DatabaseApiToken
    , accessToken : AccessToken
    , table : String
    , query : List Url.QueryParameter
    , body : Json.Encode.Value
    , expect : Http.Expect msg
    }
    -> Effect msg
updateInDatabase { apiKey, accessToken, table, query, body, expect } =
    Http.request
        { method = "PATCH"
        , headers =
            [ Http.header "apikey" (DatabaseApiToken.toString apiKey)
            , Http.header "Authorization" ("Bearer " ++ AccessToken.token accessToken)
            , Http.header "Prefer" "return=representation"
            ]
        , url =
            Url.crossOrigin
                "https://xluvzigagcthclpwrzhj.supabase.co"
                [ "rest", "v1", table ]
                query
        , body = Http.jsonBody body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }
        |> Effect.sendCmd


deleteFromDatabase :
    { apiKey : DatabaseApiToken
    , accessToken : AccessToken
    , table : String
    , id : String
    , msg : Result Http.Error () -> msg
    }
    -> Effect msg
deleteFromDatabase { apiKey, accessToken, table, id, msg } =
    Http.request
        { method = "DELETE"
        , headers =
            [ Http.header "apikey" (DatabaseApiToken.toString apiKey)
            , Http.header "Authorization" ("Bearer " ++ AccessToken.token accessToken)
            ]
        , url =
            Url.crossOrigin
                "https://xluvzigagcthclpwrzhj.supabase.co"
                [ "rest", "v1", table ]
                [ Url.string "id" ("eq." ++ id) ]
        , body = Http.emptyBody
        , expect = Http.expectWhatever msg
        , timeout = Nothing
        , tracker = Nothing
        }
        |> Effect.sendCmd
