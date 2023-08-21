module UserId exposing (UserId, decoder, equals, toString)

import Json.Decode exposing (Decoder)


type UserId
    = UserId String


toString : UserId -> String
toString (UserId userId) =
    userId


equals : UserId -> UserId -> Bool
equals (UserId userId1) (UserId userId2) =
    userId1 == userId2


decoder : Decoder UserId
decoder =
    Json.Decode.string
        |> Json.Decode.map UserId
