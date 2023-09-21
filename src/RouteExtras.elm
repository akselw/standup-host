module RouteExtras exposing (href, isProtected)

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes
import Route.Path exposing (Path(..))


href : Route.Path.Path -> Attribute msg
href path =
    path
        |> Route.Path.toString
        |> Attributes.href


isProtected : Route.Path.Path -> Bool
isProtected path =
    case path of
        Home_ ->
            False

        Login ->
            True

        MineTeam ->
            True

        MineTeam_LeggTil ->
            True

        Oauth ->
            True

        Team_ record ->
            False

        Team__Settings record ->
            True

        NotFound_ ->
            False
