module Shared.Model exposing (AccessTokenStatus(..), Model, accessToken)

import AccessToken exposing (AccessToken)
import DatabaseApiToken exposing (DatabaseApiToken)


{-| Normally, this value would live in "Shared.elm"
but that would lead to a circular dependency import cycle.

For that reason, both `Shared.Model` and `Shared.Msg` are in their
own file, so they can be imported by `Effect.elm`

-}
type alias Model =
    { apiKey : DatabaseApiToken
    , accessToken : AccessTokenStatus
    }


accessToken : Model -> Maybe AccessToken
accessToken model =
    case model.accessToken of
        Token accessToken_ ->
            Just accessToken_

        _ ->
            Nothing


type AccessTokenStatus
    = CheckingToken
    | Token AccessToken
    | LoggingOut
    | NoToken
