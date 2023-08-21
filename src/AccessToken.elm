module AccessToken exposing (AccessToken, init, token, userId)

import Json.Decode exposing (Decoder)
import Jwt
import UserId exposing (UserId)


type AccessToken
    = AccessToken
        { accessToken : String
        , userId : UserId
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


userId : AccessToken -> UserId
userId (AccessToken accessToken) =
    accessToken.userId


userIdDecoder : Decoder UserId
userIdDecoder =
    Json.Decode.field "sub" UserId.decoder
