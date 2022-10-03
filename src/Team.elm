module Team exposing (Team, decoder)

import Json.Decode exposing (Decoder, succeed)
import Json.Decode.Pipeline exposing (required)


type Team
    = Team TeamInfo


type alias TeamInfo =
    { navn : String
    , shortname : String
    }


decoder : Decoder Team
decoder =
    succeed TeamInfo
        |> required "navn" Json.Decode.string
        |> required "shortname" Json.Decode.string
        |> Json.Decode.map Team
