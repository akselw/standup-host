port module Authentication exposing (AccessTokenStatus(..), checkAccessToken, login, loginWithRedirectUrl, logout, redirectKey)

import AccessToken exposing (AccessToken)
import Json.Encode
import Jwt
import Route.Path as Path exposing (Path)
import Task


port authentication : Json.Encode.Value -> Cmd msg


type AccessTokenStatus
    = ValidToken AccessToken
    | ExpiredToken


checkAccessToken : (Result Jwt.JwtError AccessTokenStatus -> msg) -> AccessToken -> Cmd msg
checkAccessToken msg accessToken =
    accessToken
        |> AccessToken.token
        |> Jwt.checkTokenExpiry
        |> Task.map
            (\expired ->
                if expired then
                    ExpiredToken

                else
                    ValidToken accessToken
            )
        |> Task.attempt msg


redirectKey : String
redirectKey =
    "redirect"


login : Cmd msg
login =
    Json.Encode.object
        [ ( "type", Json.Encode.string "LOGIN" ) ]
        |> authentication


loginWithRedirectUrl : Path -> Cmd msg
loginWithRedirectUrl path =
    Json.Encode.object
        [ ( "type", Json.Encode.string "LOGIN" )
        , ( "redirectUrl", Json.Encode.string (Path.toString path) )
        ]
        |> authentication


logout : Cmd msg
logout =
    Json.Encode.object
        [ ( "type", Json.Encode.string "LOGOUT" ) ]
        |> authentication
