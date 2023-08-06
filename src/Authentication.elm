port module Authentication exposing (login, loginWithRedirectUrl, redirectKey)

import Json.Encode
import Route.Path as Path exposing (Path)


port authentication : Json.Encode.Value -> Cmd msg


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
