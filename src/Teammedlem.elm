module Teammedlem exposing (Teammedlem, decoder, id, navn)

import Json.Decode exposing (Decoder)
import Json.Decode.Pipeline exposing (required)


type Teammedlem
    = Teammedlem TeammedlemInfo


type alias TeammedlemInfo =
    { navn : String
    , id : String
    }



--- Values ---


navn : Teammedlem -> String
navn (Teammedlem teammedlem) =
    teammedlem.navn


id : Teammedlem -> String
id (Teammedlem teammedlem) =
    teammedlem.id



--- Decoding ---


decoder : Decoder Teammedlem
decoder =
    Json.Decode.succeed TeammedlemInfo
        |> required "name" Json.Decode.string
        |> required "id" Json.Decode.string
        |> Json.Decode.map Teammedlem
