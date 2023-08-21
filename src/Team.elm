module Team exposing
    ( Error(..)
    , Team
    , init
    , isOwner
    , medlemmer
    , navn
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


medlemmer : Team -> List Teammedlem
medlemmer (Team team) =
    team.medlemmer



--- Helper ---


isOwner : Team -> UserId -> Bool
isOwner (Team { summary }) userId =
    TeamSummary.isOwner summary userId



--- Error ---


type Error
    = FantIkkeTeam
    | IngenTeammedlemmer
    | HttpErrorForTeam Http.Error
    | HttpErrorForTeammedlemmer Http.Error
