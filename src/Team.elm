module Team exposing
    ( BackendTeam
    , BackendTeammedlemmer
    , Error(..)
    , Team
    , fromBackendTypes
    , id
    , medlemmer
    , navn
    , teamDecoder
    , teammedlemmerDecoder
    )

import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import RotationLength exposing (RotationLength)


type Team
    = Team TeamInfo


type alias TeamInfo =
    { navn : String
    , shortname : String
    , medlemmer : List String
    , rotationLength : RotationLength
    , properRandom : Bool
    }


fromBackendTypes : BackendTeam -> BackendTeammedlemmer -> Team
fromBackendTypes (BackendTeam team) (BackendTeammedlemmer teammedlemmer) =
    Team
        { navn = team.navn
        , shortname = team.shortname
        , medlemmer = teammedlemmer
        , rotationLength = team.rotationLength
        , properRandom = team.properRandom
        }



--- Felter ---


navn : Team -> String
navn (Team team) =
    team.navn


medlemmer : Team -> List String
medlemmer (Team team) =
    team.medlemmer


id : BackendTeam -> String
id (BackendTeam team) =
    team.id



--- Error ---


type Error
    = FantIkkeTeam
    | IngenTeammedlemmer
    | HttpErrorForTeam Http.Error
    | HttpErrorForTeammedlemmer Http.Error



--- Decoding ---


type BackendTeam
    = BackendTeam BackendTeamInfo


type alias BackendTeamInfo =
    { navn : String
    , shortname : String
    , id : String
    , rotationLength : RotationLength
    , properRandom : Bool
    }


type BackendTeammedlemmer
    = BackendTeammedlemmer (List String)


teammedlemmerDecoder : Decoder BackendTeammedlemmer
teammedlemmerDecoder =
    Json.Decode.list (Json.Decode.field "name" Json.Decode.string)
        |> Json.Decode.map BackendTeammedlemmer


type alias BackendDataTeammedlem =
    { navn : String
    , teamNavn : String
    , teamShortname : String
    }


teamDecoder : Decoder BackendTeam
teamDecoder =
    Json.Decode.succeed BackendTeamInfo
        |> required "name" Json.Decode.string
        |> required "shortname" Json.Decode.string
        |> required "id" Json.Decode.string
        |> required "rotation_length" RotationLength.decoder
        |> required "proper_random" Json.Decode.bool
        |> Json.Decode.map BackendTeam
