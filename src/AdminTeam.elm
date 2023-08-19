module AdminTeam exposing
    ( AdminTeam
    , BackendAdminTeam
    , Error(..)
    , fromBackendTypes
    , id
    , medlemmer
    , navn
    , teamDecoder
    )

import Http
import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import RotationLength exposing (RotationLength)
import Teammedlem exposing (Teammedlem)


type AdminTeam
    = AdminTeam AdminTeamInfo


type alias AdminTeamInfo =
    { navn : String
    , shortname : String
    , medlemmer : List Teammedlem
    , rotationLength : RotationLength
    , properRandom : Bool
    , ownerId : String
    }


type alias AdminTeammedlem =
    { navn : String
    , id : String
    }


fromBackendTypes : BackendAdminTeam -> List Teammedlem -> AdminTeam
fromBackendTypes (BackendAdminTeam team) teammedlemmer =
    AdminTeam
        { navn = team.navn
        , shortname = team.shortname
        , medlemmer = teammedlemmer
        , rotationLength = team.rotationLength
        , properRandom = team.properRandom
        , ownerId = team.ownerId
        }



--- Felter ---


navn : AdminTeam -> String
navn (AdminTeam team) =
    team.navn


medlemmer : AdminTeam -> List Teammedlem
medlemmer (AdminTeam team) =
    team.medlemmer


id : BackendAdminTeam -> String
id (BackendAdminTeam team) =
    team.id



--- Error ---


type Error
    = FantIkkeTeam
    | IngenTeammedlemmer
    | HttpErrorForTeam Http.Error
    | HttpErrorForTeammedlemmer Http.Error



--- Decoding ---


type BackendAdminTeam
    = BackendAdminTeam BackendAdminTeamInfo


type alias BackendAdminTeamInfo =
    { navn : String
    , shortname : String
    , id : String
    , rotationLength : RotationLength
    , properRandom : Bool
    , ownerId : String
    }


type alias BackendDataTeammedlem =
    { navn : String
    , teamNavn : String
    , teamShortname : String
    }


teamDecoder : Decoder BackendAdminTeam
teamDecoder =
    Json.Decode.succeed BackendAdminTeamInfo
        |> required "name" Json.Decode.string
        |> required "shortname" Json.Decode.string
        |> required "id" Json.Decode.string
        |> required "rotation_length" RotationLength.decoder
        |> required "proper_random" Json.Decode.bool
        |> required "owner_id" Json.Decode.string
        |> Json.Decode.map BackendAdminTeam
