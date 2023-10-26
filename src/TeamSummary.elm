module TeamSummary exposing (TeamSummary, decoder, hasOwner, id, navn, slug, slugString)

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import RotationLength exposing (RotationLength)
import Slug exposing (Slug)
import TeamId exposing (TeamId)
import UserId exposing (UserId)


type TeamSummary
    = TeamSummary TeamSummaryInfo


type alias TeamSummaryInfo =
    { navn : String
    , slug : Slug
    , id : TeamId
    , rotationLength : RotationLength
    , properRandom : Bool
    , ownerId : UserId
    }



--- Values ---


navn : TeamSummary -> String
navn (TeamSummary team) =
    team.navn


slug : TeamSummary -> Slug
slug (TeamSummary team) =
    team.slug


slugString : TeamSummary -> String
slugString (TeamSummary team) =
    Slug.toString team.slug


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
        |> required "slug" Slug.decoder
        |> required "id" TeamId.decoder
        |> required "rotation_length" RotationLength.decoder
        |> required "proper_random" Json.Decode.bool
        |> required "owner_id" UserId.decoder
        |> Json.Decode.map TeamSummary
