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

import AccessToken
import Authentication
import DatabaseApiToken exposing (DatabaseApiToken)
import Effect exposing (Effect)
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import Jwt
import LocalStorage
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
                Just accessTokenString ->
                    case AccessToken.init accessTokenString of
                        Just accessToken ->
                            ( { apiKey = flags.apiKey
                              , accessToken = CheckingToken
                              }
                            , accessToken
                                |> Authentication.checkAccessToken AccessTokenExpiredChecked
                                |> Effect.sendCmd
                            )

                        Nothing ->
                            ( { apiKey = flags.apiKey
                              , accessToken = NoToken
                              }
                            , Effect.none
                            )

                Nothing ->
                    ( { apiKey = flags.apiKey
                      , accessToken = NoToken
                      }
                    , Effect.none
                    )

        Err _ ->
            ( { apiKey = DatabaseApiToken.invalidToken
              , accessToken = NoToken
              }
            , Effect.none
              -- TODO: Logg dette
            )



-- UPDATE


type alias Msg =
    Shared.Msg.Msg


update : Route () -> Msg -> Model -> ( Model, Effect Msg )
update route msg model =
    case msg of
        AccessTokenChanged accessTokenString ->
            case AccessToken.init accessTokenString of
                Just accessToken ->
                    ( { model | accessToken = Token accessToken }
                    , Effect.none
                    )

                Nothing ->
                    ( model, Effect.none )

        AccessTokenExpiredChecked (Ok (Authentication.ValidToken accessToken)) ->
            ( { model | accessToken = Token accessToken }
            , Effect.none
            )

        AccessTokenExpiredChecked _ ->
            ( { model | accessToken = NoToken }
            , Effect.none
            )

        Logout ->
            ( { model | accessToken = NoToken }
            , Effect.batch
                [ Authentication.logout
                    |> Effect.sendCmd
                , LocalStorage.removeItem "hvem-har-standup:access_token"
                    |> Effect.sendCmd
                ]
            )



-- SUBSCRIPTIONS


subscriptions : Route () -> Model -> Sub Msg
subscriptions route model =
    Sub.none
