port module Authentication exposing (..)

import Json.Encode


port authentication : Json.Encode.Value -> Cmd msg


login : Cmd msg
login =
    Json.Encode.object
        [ ( "type", Json.Encode.string "LOGIN" )
        ]
        |> authentication
