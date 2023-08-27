module Team exposing
    ( Error(..)
    , Team
    , hasOwner
    , id
    , init
    , medlemmer
    , navn
    , shortname
    )

import Http
import TeamSummary exposing (TeamSummary)
import Teammedlem exposing (Teammedlem)
import UserId exposing (UserId)


type Team
    = Team
        { summary : TeamSummary
        , medlemmer : List Teammedlem
        }


init : TeamSummary -> List Teammedlem -> Team
init summary teammedlemmer =
    Team
        { summary = summary
        , medlemmer = teammedlemmer
        }



--- Felter ---


navn : Team -> String
navn (Team { summary }) =
    TeamSummary.navn summary


shortname : Team -> String
shortname (Team { summary }) =
    TeamSummary.shortname summary


id : Team -> String
id (Team { summary }) =
    TeamSummary.id summary


medlemmer : Team -> List Teammedlem
medlemmer (Team team) =
    team.medlemmer



--- Helper ---


hasOwner : Team -> UserId -> Bool
hasOwner (Team { summary }) userId =
    TeamSummary.hasOwner summary userId



--- Error ---


type Error
    = FantIkkeTeam
    | HttpErrorForTeam Http.Error
    | HttpErrorForTeammedlemmer Http.Error
