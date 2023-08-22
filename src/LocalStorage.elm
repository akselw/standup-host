port module LocalStorage exposing (removeItem, setItem)

import Effect exposing (Effect)
import Json.Encode


port localStorage : Json.Encode.Value -> Cmd msg


setItem : String -> Json.Encode.Value -> Effect msg
setItem key value =
    Json.Encode.object
        [ ( "type", Json.Encode.string "SET_ITEM" )
        , ( "key", Json.Encode.string key )
        , ( "value", value )
        ]
        |> localStorage
        |> Effect.sendCmd


removeItem : String -> Effect msg
removeItem key =
    Json.Encode.object
        [ ( "type", Json.Encode.string "REMOVE_ITEM" )
        , ( "key", Json.Encode.string key )
        ]
        |> localStorage
        |> Effect.sendCmd
