module RouteExtras exposing (href)

import Html.Styled exposing (Attribute)
import Html.Styled.Attributes as Attributes
import Route.Path


href : Route.Path.Path -> Attribute msg
href path =
    path
        |> Route.Path.toString
        |> Attributes.href
