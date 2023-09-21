module View.Page exposing (viewPageWrapper)

import Css
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes


viewPageWrapper : List (Html msg) -> List (Html msg)
viewPageWrapper children =
    [ div [ Attributes.css [ Css.maxWidth (Css.px 632), Css.margin Css.auto, Css.padding2 Css.zero (Css.px 16) ] ]
        children
    ]
