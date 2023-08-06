module Shared exposing
    ( Flags, decoder
    , Model, Msg
    , init, update, subscriptions
    )

{-|

@docs Flags, decoder
@docs Model, Msg
@docs init, update, subscriptions

-}

import DatabaseApiToken exposing (DatabaseApiToken)
import Effect exposing (Effect)
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import Jwt
import Route exposing (Route)
import Shared.Model exposing (AccessTokenStatus(..))
import Shared.Msg exposing (Msg(..))
import Task



-- FLAGS


type alias Flags =
    { apiKey : DatabaseApiToken
    , accessToken : Maybe String
    }


decoder : Json.Decode.Decoder Flags
decoder =
    Json.Decode.succeed Flags
        |> required "apiKey" DatabaseApiToken.decoder
        |> required "accessToken" (Json.Decode.maybe Json.Decode.string)



-- INIT


type alias Model =
    Shared.Model.Model


init : Result Json.Decode.Error Flags -> Route () -> ( Model, Effect Msg )
init flagsResult route =
    case flagsResult of
        Ok flags ->
            case flags.accessToken of
                Just accessToken ->
                    ( { apiKey = flags.apiKey
                      , accessToken = CheckingToken
                      }
                    , accessToken
                        |> Jwt.checkTokenExpiry
                        |> Task.attempt (AccessTokenExpiredChecked accessToken)
                        |> Effect.sendCmd
                    )

                Nothing ->
                    ( { apiKey = flags.apiKey
                      , accessToken = NoToken
                      }
                    , Effect.none
                    )

        Err _ ->
            Debug.todo "Finn ut hva som skal skje om flags ikke er der"



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        AccessTokenChanged accessToken ->
            ( { model | accessToken = Token accessToken }
            , Effect.none
            )

        AccessTokenExpiredChecked accessToken (Ok False) ->
            ( { model | accessToken = Token accessToken }
            , Effect.none
            )

        AccessTokenExpiredChecked _ _ ->
            ( { model | accessToken = NoToken }
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
