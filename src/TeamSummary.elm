module TeamSummary exposing (TeamSummary, decoder, id, navn, shortname)

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import RotationLength exposing (RotationLength)
import UserId exposing (UserId)


type TeamSummary
    = TeamSummary TeamSummaryInfo


type alias TeamSummaryInfo =
    { navn : String
    , shortname : String
    , id : String
    , rotationLength : RotationLength
    , properRandom : Bool
    , ownerId : UserId
    }



--- Values ---


navn : TeamSummary -> String
navn (TeamSummary team) =
    team.navn


shortname : TeamSummary -> String
shortname (TeamSummary team) =
    team.shortname


id : TeamSummary -> String
id (TeamSummary team) =
    team.id



--- Decoding ---


decoder : Decoder TeamSummary
decoder =
    Json.Decode.succeed TeamSummaryInfo
        |> required "name" Json.Decode.string
        |> required "shortname" Json.Decode.string
        |> required "id" Json.Decode.string
        |> required "rotation_length" RotationLength.decoder
        |> required "proper_random" Json.Decode.bool
        |> required "owner_id" UserId.decoder
        |> Json.Decode.map TeamSummary
