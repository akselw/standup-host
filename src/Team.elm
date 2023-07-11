module Team exposing (Team, addMedlemmer, decoder, id, medlemmer, navn, teammedlemmerDecoder)

import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (hardcoded, required, requiredAt)


type Team
    = Team TeamInfo


type alias TeamInfo =
    { navn : String
    , shortname : String
    , id : String
    , medlemmer : List String
    }



--- FELTER ---


navn : Team -> String
navn (Team team) =
    team.navn


medlemmer : Team -> List String
medlemmer (Team team) =
    team.medlemmer


id : Team -> String
id (Team team) =
    team.id



--- TODO: Endre dette ---


addMedlemmer : Team -> List String -> Team
addMedlemmer (Team team) medlemliste =
    Team { team | medlemmer = medlemliste }



--- DECODING ---


teammedlemmerDecoder : Decoder (List String)
teammedlemmerDecoder =
    Json.Decode.list (Json.Decode.field "navn" Json.Decode.string)


type alias BackendDataTeammedlem =
    { navn : String
    , teamNavn : String
    , teamShortname : String
    }


decoder : Decoder Team
decoder =
    Json.Decode.succeed TeamInfo
        |> required "navn" Json.Decode.string
        |> required "shortname" Json.Decode.string
        |> required "id" Json.Decode.string
        |> hardcoded []
        |> Json.Decode.map Team



--
--decoder2 : Decoder Team
--decoder2 =
--    Json.Decode.list decoderBackendData
--        |> Json.Decode.andThen backendDataToTeam
--
--
--decoderBackendData : Decoder BackendDataTeammedlem
--decoderBackendData =
--    succeed BackendDataTeammedlem
--        |> required "navn" Json.Decode.string
--        |> requiredAt [ "team", "navn" ] Json.Decode.string
--        |> requiredAt [ "team", "shortname" ] Json.Decode.string
--
--
--backendDataToTeam : List BackendDataTeammedlem -> Decoder Team
--backendDataToTeam teammedlemmer =
--    case teammedlemmer of
--        first :: _ ->
--            Json.Decode.succeed
--                (Team
--                    { navn = first.teamNavn
--                    , shortname = first.teamShortname
--                    , medlemmer =
--                        List.map .navn teammedlemmer
--                            |> List.sort
--                    }
--                )
--
--        _ ->
--            -- TODO: Hva om et team ikke har teammedlemmer ennå?
--            -- Kanskje to API-kall, som chaines med Task?
--            Json.Decode.fail "Team har ingen teammedlemmer"
