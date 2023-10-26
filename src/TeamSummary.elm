module TeamSummary exposing (TeamSummary, decoder, hasOwner, id, navn, slug)

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import RotationLength exposing (RotationLength)
import TeamId exposing (TeamId)
import UserId exposing (UserId)


type TeamSummary
    = TeamSummary TeamSummaryInfo


type alias TeamSummaryInfo =
    { navn : String
    , slug : String
    , id : TeamId
    , rotationLength : RotationLength
    , properRandom : Bool
    , ownerId : UserId
    }



--- Values ---


navn : TeamSummary -> String
navn (TeamSummary team) =
    team.navn


slug : TeamSummary -> String
slug (TeamSummary team) =
    team.slug


id : TeamSummary -> TeamId
id (TeamSummary team) =
    team.id



--- Helper ---


hasOwner : TeamSummary -> UserId -> Bool
hasOwner (TeamSummary team) userId =
    UserId.equals team.ownerId userId



--- Decoding ---


decoder : Decoder TeamSummary
decoder =
    Json.Decode.succeed TeamSummaryInfo
        |> required "name" Json.Decode.string
        |> required "slug" Json.Decode.string
        |> required "id" TeamId.decoder
        |> required "rotation_length" RotationLength.decoder
        |> required "proper_random" Json.Decode.bool
        |> required "owner_id" UserId.decoder
        |> Json.Decode.map TeamSummary
