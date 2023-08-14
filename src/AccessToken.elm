module AccessToken exposing (AccessToken, init, token, userId)

import Json.Decode exposing (Decoder)
import Jwt


type AccessToken
    = AccessToken
        { accessToken : String
        , userId : String
        }


init : String -> Maybe AccessToken
init string =
    Just
        (AccessToken
            { accessToken = string
            , userId = "TODO"
            }
        )



--- VALUES ---


token : AccessToken -> String
token (AccessToken accessToken) =
    accessToken.accessToken


userId : AccessToken -> String
userId (AccessToken accessToken) =
    case Jwt.decodeToken userIdDecoder accessToken.accessToken of
        Ok userIdString ->
            userIdString

        Err error ->
            -- TODO
            Jwt.errorToString error


userIdDecoder : Decoder String
userIdDecoder =
    Json.Decode.field "sub" Json.Decode.string
