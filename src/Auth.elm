module Auth exposing (User, onPageLoad, viewLoadingPage)

import Auth.Action
import Dict
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


type alias User =
    { accessToken : String }


{-| Called before an auth-only page is loaded.
-}
onPageLoad : Shared.Model -> Route () -> Auth.Action.Action User
onPageLoad shared route =
    case shared.accessToken of
        Just accessToken ->
            -- TODO: Legg til validering av om accesstokenet er gyldig? Evt. la det bli håndtert i shared
            Auth.Action.loadPageWithUser { accessToken = accessToken }

        Nothing ->
            -- TODO: Redirect til login-side
            Auth.Action.pushRoute
                { path = Route.Path.NotFound_
                , query = Dict.empty
                , hash = Nothing
                }


{-| Renders whenever `Auth.Action.showLoadingPage` is returned from `onPageLoad`.
-}
viewLoadingPage : Shared.Model -> Route () -> View Never
viewLoadingPage shared route =
    View.fromString "Loading..."