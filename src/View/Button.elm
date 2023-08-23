module View.Button exposing (Button, Size(..), Variant(..), button, submit, toHtml, withSize, withVariant)

import Css
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onClick)


button : msg -> String -> Button msg
button onClick label =
    Button
        { label = label
        , variant = Primary
        , size = Medium
        , type_ = TypeButton onClick
        }


submit : String -> Button msg
submit label =
    Button
        { label = label
        , variant = Primary
        , size = Medium
        , type_ = TypeSubmit
        }


type Button msg
    = Button (Options msg)


type ButtonType msg
    = TypeButton msg
    | TypeSubmit


type alias Options msg =
    { label : String
    , variant : Variant
    , size : Size
    , type_ : ButtonType msg
    }


type Variant
    = Primary
    | Secondary
    | Tertiary


type Size
    = Medium
    | Large



--- Options ---


withVariant : Variant -> Button msg -> Button msg
withVariant variant (Button options) =
    Button { options | variant = variant }


withSize : Size -> Button msg -> Button msg
withSize size (Button options) =
    Button { options | size = size }



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
            Css.batch []

        Secondary ->
            Css.boxShadow6 Css.inset (Css.px 0) (Css.px 0) (Css.px 0) (Css.px 1) (Css.hex "979FAF")

        Tertiary ->
            Css.batch []


padding : Size -> Css.Style
padding size =
    case size of
        Medium ->
            Css.padding2 (Css.rem 0.5) (Css.rem 0.75)

        Large ->
            Css.padding2 (Css.rem 0.625) (Css.rem 0.875)


toHtml : Button msg -> Html msg
toHtml (Button options) =
    Html.button
        [ case options.type_ of
            TypeButton _ ->
                Attributes.type_ "button"

            TypeSubmit ->
                Attributes.type_ "submit"
        , case options.type_ of
            TypeButton onClickMsg ->
                onClick onClickMsg

            TypeSubmit ->
                Attributes.classList []
        , Attributes.css
            [ Css.fontFamilies [ "Open Sans", "Helvetica Neue", "sans-serif" ]
            , Css.fontSize (Css.px 14)
            , Css.letterSpacing (Css.px 1.2)
            , Css.color (textColor options.variant)
            , Css.backgroundColor (backgroundColor options.variant)
            , Css.border Css.zero
            , border options.variant
            , Css.borderRadius (Css.rem 0.375)
            , padding options.size
            , Css.cursor Css.pointer
            , Css.hover [ Css.backgroundColor (hoverColor options.variant) ]
            ]
        ]
        [ text options.label ]
