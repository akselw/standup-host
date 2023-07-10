module Team exposing (Team, decoder, medlemmer, navn)

import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required, requiredAt)


type Team
    = Team TeamInfo


type alias TeamInfo =
    { navn : String
    , shortname : String
    , medlemmer : List String
    }



--- FELTER ---


navn : Team -> String
navn (Team team) =
    team.navn


medlemmer : Team -> List String
medlemmer (Team team) =
    team.medlemmer



--- DECODING ---


type alias BackendDataTeammedlem =
    { navn : String
    , teamNavn : String
    , teamShortname : String
    }


decoder : Decoder Team
decoder =
    Json.Decode.list decoderBackendData
        |> Json.Decode.andThen backendDataToTeam


decoderBackendData : Decoder BackendDataTeammedlem
decoderBackendData =
    succeed BackendDataTeammedlem
        |> required "navn" Json.Decode.string
        |> requiredAt [ "team", "navn" ] Json.Decode.string
        |> requiredAt [ "team", "shortname" ] Json.Decode.string


backendDataToTeam : List BackendDataTeammedlem -> Decoder Team
backendDataToTeam teammedlemmer =
    case teammedlemmer of
        first :: _ ->
            Json.Decode.succeed
                (Team
                    { navn = first.teamNavn
                    , shortname = first.teamShortname
                    , medlemmer = List.map .navn teammedlemmer
                    }
                )

        _ ->
            -- TODO: Hva om et team ikke har teammedlemmer ennå?
            -- Kanskje to API-kall, som chaines med Task?
            Json.Decode.fail "Team har ingen teammedlemmer"
