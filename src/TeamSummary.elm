module TeamSummary exposing (TeamSummary, decoder, navn, shortname)

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)
import RotationLength exposing (RotationLength)


type TeamSummary
    = TeamSummary TeamSummaryInfo


type alias TeamSummaryInfo =
    { navn : String
    , shortname : String
    , id : String
    , rotationLength : RotationLength
    , properRandom : Bool
    , ownerId : String
    }



--- Values ---


navn : TeamSummary -> String
navn (TeamSummary team) =
    team.navn


shortname : TeamSummary -> String
shortname (TeamSummary team) =
    team.shortname



--- Decoding ---


decoder : Decoder TeamSummary
decoder =
    Json.Decode.succeed TeamSummaryInfo
        |> required "name" Json.Decode.string
        |> required "shortname" Json.Decode.string
        |> required "id" Json.Decode.string
        |> required "rotation_length" RotationLength.decoder
        |> required "proper_random" Json.Decode.bool
        |> required "owner_id" Json.Decode.string
        |> Json.Decode.map TeamSummary