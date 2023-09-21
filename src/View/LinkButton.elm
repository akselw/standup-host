module View.LinkButton exposing
    ( LinkButton
    , linkButton
    , withVariant, withSize
    , toHtml
    )

{-|

@docs LinkButton
@docs linkButton
@docs withVariant, withSize
@docs toHtml

-}

import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Route.Path
import RouteExtras
import View.Button as Button


type LinkButton msg
    = LinkButton
        { path : Route.Path.Path
        , children : List (Html msg)
        , variant : Button.Variant
        , size : Button.Size
        }


linkButton : Route.Path.Path -> List (Html msg) -> LinkButton msg
linkButton path children =
    LinkButton
        { path = path
        , children = children
        , variant = Button.Primary
        , size = Button.Large
        }



--- Options ---


withVariant : Button.Variant -> LinkButton msg -> LinkButton msg
withVariant variant (LinkButton options) =
    LinkButton { options | variant = variant }


withSize : Button.Size -> LinkButton msg -> LinkButton msg
withSize size (LinkButton options) =
    LinkButton { options | size = size }



--- Html ---


toHtml : LinkButton msg -> Html msg
toHtml (LinkButton options) =
    Html.a
        [ RouteExtras.href options.path
        , Attributes.css
            [ Css.fontFamilies [ "Open Sans", "Helvetica Neue", "sans-serif" ]
            , Css.fontSize (Css.px 14)
            , Css.letterSpacing (Css.px 1.2)
            , Css.color (textColor options.variant)
            , Css.textDecoration Css.none
            , Css.backgroundColor (backgroundColor options.variant)
            , Css.border Css.zero
            , border options.variant
            , Css.borderRadius (Css.rem 0.375)
            , padding options.size
            , Css.cursor Css.pointer
            , Css.hover [ Css.backgroundColor (hoverColor options.variant) ]
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.property "gap" "4px"
            ]
        ]
        options.children


backgroundColor : Button.Variant -> Css.Color
backgroundColor variant =
    case variant of
        Button.Primary ->
            Css.hex "4F46E5"

        Button.Secondary ->
            Css.hex "FFFFFF"

        Button.Tertiary ->
            Css.hex "0D2F44"


hoverColor : Button.Variant -> Css.Color
hoverColor variant =
    case variant of
        Button.Primary ->
            Css.hex "6366f1"

        Button.Secondary ->
            Css.hex "F9FAFB"

        Button.Tertiary ->
            Css.hex "334155"


textColor : Button.Variant -> Css.Color
textColor variant =
    case variant of
        Button.Primary ->
            Css.hex "FFFFFF"

        Button.Secondary ->
            Css.hex "111827"

        Button.Tertiary ->
            Css.hex "FFFFFF"


border : Button.Variant -> Css.Style
border variant =
    case variant of
        Button.Primary ->
            Css.batch []

        Button.Secondary ->
            Css.boxShadow6 Css.inset (Css.px 0) (Css.px 0) (Css.px 0) (Css.px 1) (Css.hex "979FAF")

        Button.Tertiary ->
            Css.batch []


padding : Button.Size -> Css.Style
padding size =
    case size of
        Button.Medium ->
            Css.padding2 (Css.rem 0.5) (Css.rem 0.75)

        Button.Large ->
            Css.padding2 (Css.rem 0.625) (Css.rem 0.875)
