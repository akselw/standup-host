module View.Link exposing
    ( Link
    , link
    , withoutColor
    , withCss
    , toHtml
    )

{-|

@docs Link
@docs link
@docs withoutColor
@docs withCss
@docs toHtml

-}

import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Route.Path
import RouteExtras


type Link msg
    = Link
        { path : Route.Path.Path
        , children : List (Html msg)
        , withoutColor : Bool
        , css : List Css.Style
        }


link : Route.Path.Path -> List (Html msg) -> Link msg
link path children =
    Link
        { path = path
        , children = children
        , withoutColor = False
        , css = []
        }



--- Options ---


withoutColor : Link msg -> Link msg
withoutColor (Link options) =
    Link { options | withoutColor = True }


withCss : List Css.Style -> Link msg -> Link msg
withCss css (Link options) =
    Link { options | css = options.css ++ css }



--- Html ---


toHtml : Link msg -> Html msg
toHtml (Link options) =
    Html.a
        [ RouteExtras.href options.path
        , Attributes.css
            [ color { withoutColor_ = options.withoutColor }
            , Css.textDecoration Css.none
            , Css.batch options.css
            ]
        ]
        options.children


color : { withoutColor_ : Bool } -> Css.Style
color { withoutColor_ } =
    if withoutColor_ then
        Css.color (Css.hex "040F16")

    else
        Css.color (Css.hex "0000EE")
