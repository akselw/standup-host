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
    case Jwt.decodeToken userIdDecoder string of
        Ok userIdString ->
            Just
                (AccessToken
                    { accessToken = string
                    , userId = userIdString
                    }
                )

        Err _ ->
            Nothing



--- VALUES ---


token : AccessToken -> String
token (AccessToken accessToken) =
    accessToken.accessToken


userId : AccessToken -> String
userId (AccessToken accessToken) =
    accessToken.userId


userIdDecoder : Decoder String
userIdDecoder =
    Json.Decode.field "sub" Json.Decode.string
