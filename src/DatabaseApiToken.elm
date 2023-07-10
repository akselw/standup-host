module DatabaseApiToken exposing (DatabaseApiToken, decoder, init, toString)

import Json.Decode exposing (Decoder)


type DatabaseApiToken
    = DatabaseApiToken String


init : String -> DatabaseApiToken
init s =
    DatabaseApiToken s


toString : DatabaseApiToken -> String
toString (DatabaseApiToken s) =
    s



--- DECODER


decoder : Decoder DatabaseApiToken
decoder =
    Json.Decode.string
        |> Json.Decode.map DatabaseApiToken
