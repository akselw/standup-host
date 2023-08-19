module RotationLength exposing (RotationLength(..), decoder)

import Json.Decode exposing (Decoder)


type RotationLength
    = Daily
    | Weekly


decoder : Decoder RotationLength
decoder =
    Json.Decode.string
        |> Json.Decode.andThen
            (\rotationLength ->
                case rotationLength of
                    "DAILY" ->
                        Json.Decode.succeed Daily

                    "WEEKLY" ->
                        Json.Decode.succeed Weekly

                    _ ->
                        Json.Decode.fail ("Klarte ikke å decode rotationLength med verdi: " ++ rotationLength)
            )
