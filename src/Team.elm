module Team exposing
    ( Error(..)
    , Team
    , hasOwner
    , id
    , init
    , medlemmer
    , navn
    , slug
    , slugString
    , updateTeamSummary
    )

import Http
import Slug exposing (Slug)
import TeamId exposing (TeamId)
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


slug : Team -> Slug
slug (Team { summary }) =
    TeamSummary.slug summary


slugString : Team -> String
slugString (Team { summary }) =
    TeamSummary.slugString summary


id : Team -> TeamId
id (Team { summary }) =
    TeamSummary.id summary


medlemmer : Team -> List Teammedlem
medlemmer (Team team) =
    team.medlemmer



--- Oppdatering ---


updateTeamSummary : Team -> TeamSummary -> Team
updateTeamSummary (Team team) teamSummary =
    if TeamSummary.id team.summary == TeamSummary.id teamSummary then
        Team { team | summary = teamSummary }

    else
        Team team



--- Helper ---


hasOwner : Team -> UserId -> Bool
hasOwner (Team { summary }) userId =
    TeamSummary.hasOwner summary userId



--- Error ---


type Error
    = FantIkkeTeam
    | HttpErrorForTeam Http.Error
    | HttpErrorForTeammedlemmer Http.Error
