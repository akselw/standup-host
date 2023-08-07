port module Authentication exposing (AccessTokenStatus(..), checkAccessToken, login, loginWithRedirectUrl, redirectKey)

import Json.Encode
import Jwt
import Route.Path as Path exposing (Path)
import Task


port authentication : Json.Encode.Value -> Cmd msg


type AccessTokenStatus
    = ValidToken String
    | ExpiredToken


checkAccessToken : (Result Jwt.JwtError AccessTokenStatus -> msg) -> String -> Cmd msg
checkAccessToken msg accessToken =
    accessToken
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
