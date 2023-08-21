module UserId exposing (UserId, decoder, toString)

import Json.Decode exposing (Decoder)


type UserId
    = UserId String


toString : UserId -> String
toString (UserId userId) =
    userId


decoder : Decoder UserId
decoder =
    Json.Decode.string
        |> Json.Decode.map UserId
