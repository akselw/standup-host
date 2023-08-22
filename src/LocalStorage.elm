port module LocalStorage exposing (removeItem, setItem)

import Json.Encode


port localStorage : Json.Encode.Value -> Cmd msg


setItem : String -> Json.Encode.Value -> Cmd msg
setItem key value =
    Json.Encode.object
        [ ( "type", Json.Encode.string "SET_ITEM" )
        , ( "key", Json.Encode.string key )
        , ( "value", value )
        ]
        |> localStorage


removeItem : String -> Cmd msg
removeItem key =
    Json.Encode.object
        [ ( "type", Json.Encode.string "REMOVE_ITEM" )
        , ( "key", Json.Encode.string key )
        ]
        |> localStorage
