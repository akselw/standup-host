module Api exposing (getTeam)

import DatabaseApiToken exposing (DatabaseApiToken)
import Effect exposing (Effect)
import Http
import Team exposing (Team)
import Url.Builder as Url


getTeam : DatabaseApiToken -> String -> (Result Http.Error Team -> msg) -> Effect msg
getTeam apiKey teamShortName msg =
    getFromDatabase
        { apiKey = apiKey
        , table = "team"
        , query =
            [ Url.string "shortname" teamShortName
            , Url.string "select" "navn,id,debatt(navn)"
            ]
        , expect = Http.expectJson msg Team.decoder
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
