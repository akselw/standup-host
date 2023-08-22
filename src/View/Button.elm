module View.Button exposing (Button, Variant(..), button, toHtml, withVariant)

import Css
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onClick)


button : msg -> String -> Button msg
button onClick label =
    Button
        { label = label
        , onClick = onClick
        , variant = Primary
        }


type Button msg
    = Button (Options msg)


type alias Options msg =
    { label : String
    , onClick : msg
    , variant : Variant
    }


type Variant
    = Primary
    | Secondary
    | Tertiary



--- Options ---


withVariant : Variant -> Button msg -> Button msg
withVariant variant (Button options) =
    Button { options | variant = variant }



--- Html ---


backgroundColor : Variant -> Css.Color
backgroundColor variant =
    case variant of
        Primary ->
            Css.hex "4F46E5"

        Secondary ->
            Css.hex "FFFFFF"

        Tertiary ->
            Css.hex "0D2F44"


hoverColor : Variant -> Css.Color
hoverColor variant =
    case variant of
        Primary ->
            Css.hex "6366f1"

        Secondary ->
            Css.hex "F9FAFB"

        Tertiary ->
            Css.hex "334155"


textColor : Variant -> Css.Color
textColor variant =
    case variant of
        Primary ->
            Css.hex "FFFFFF"

        Secondary ->
            Css.hex "111827"

        Tertiary ->
            Css.hex "FFFFFF"


border : Variant -> Css.Style
border variant =
    case variant of
        Primary ->
            Css.border Css.zero

        Secondary ->
            Css.border3 (Css.px 1) Css.solid (Css.hex "979FAF")

        Tertiary ->
            Css.border Css.zero


toHtml : Button msg -> Html msg
toHtml (Button options) =
    Html.button
        [ onClick options.onClick
        , Attributes.css
            [ Css.fontFamilies [ "Open Sans", "Helvetica Neue", "sans-serif" ]
            , Css.fontSize (Css.px 14)
            , Css.letterSpacing (Css.px 1.2)
            , Css.color (textColor options.variant)
            , Css.backgroundColor (backgroundColor options.variant)
            , border options.variant
            , Css.borderRadius (Css.rem 0.375)
            , Css.padding2 (Css.rem 0.625) (Css.rem 0.875)
            , Css.cursor Css.pointer
            , Css.hover [ Css.backgroundColor (hoverColor options.variant) ]
            ]
        ]
        [ text options.label ]
