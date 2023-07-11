module Team exposing
    ( BackendTeam
    , BackendTeammedlemmer
    , Team
    , fromBackendTypes
    , id
    , medlemmer
    , navn
    , teamDecoder
    , teammedlemmerDecoder
    )

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type Team
    = Team TeamInfo


type alias TeamInfo =
    { navn : String
    , shortname : String
    , medlemmer : List String
    }


fromBackendTypes : BackendTeam -> BackendTeammedlemmer -> Team
fromBackendTypes (BackendTeam team) (BackendTeammedlemmer teammedlemmer) =
    Team
        { navn = team.navn
        , shortname = team.shortname
        , medlemmer = teammedlemmer
        }



--- FELTER ---


navn : Team -> String
navn (Team team) =
    team.navn


medlemmer : Team -> List String
medlemmer (Team team) =
    team.medlemmer


id : BackendTeam -> String
id (BackendTeam team) =
    team.id



--- DECODING ---


type BackendTeam
    = BackendTeam BackendTeamInfo


type alias BackendTeamInfo =
    { navn : String
    , shortname : String
    , id : String
    }


type BackendTeammedlemmer
    = BackendTeammedlemmer (List String)


teammedlemmerDecoder : Decoder BackendTeammedlemmer
teammedlemmerDecoder =
    Json.Decode.list (Json.Decode.field "navn" Json.Decode.string)
        |> Json.Decode.map BackendTeammedlemmer


type alias BackendDataTeammedlem =
    { navn : String
    , teamNavn : String
    , teamShortname : String
    }


teamDecoder : Decoder BackendTeam
teamDecoder =
    Json.Decode.succeed BackendTeamInfo
        |> required "navn" Json.Decode.string
        |> required "shortname" Json.Decode.string
        |> required "id" Json.Decode.string
        |> Json.Decode.map BackendTeam
