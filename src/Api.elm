module Api exposing (getTeam)

import DatabaseApiToken exposing (DatabaseApiToken)
import Effect exposing (Effect)
import Http
import Json.Decode exposing (Decoder)
import Team exposing (Team)
import Url.Builder as Url


getTeam : (Result Http.Error Team -> msg) -> DatabaseApiToken -> String -> Effect msg
getTeam msg apiKey teamShortName =
    getFromDatabase
        { apiKey = apiKey
        , table = "team"
        , query =
            [ Url.string "shortname" ("eq." ++ teamShortName)
            , Url.string "select" "navn,id,shortname"
            ]
        , expect = Http.expectJson msg (listToSingleElementDecoder Team.decoder)
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
