port module Teams exposing (fetchTeam, teamReceivedFromJs)

import Json.Decode
import Team exposing (Team)


port fetchTeam : String -> Cmd msg


port teamReceivedFromJs : (Json.Decode.Value -> msg) -> Sub msg
