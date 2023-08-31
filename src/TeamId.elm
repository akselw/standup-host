module TeamId exposing (TeamId, decoder, encode, toString)

import Json.Decode exposing (Decoder)
import Json.Encode


type TeamId
    = TeamId String


decoder : Decoder TeamId
decoder =
    Json.Decode.string
        |> Json.Decode.map TeamId


encode : TeamId -> Json.Encode.Value
encode (TeamId id) =
    Json.Encode.string id


toString : TeamId -> String
toString (TeamId id) =
    id
