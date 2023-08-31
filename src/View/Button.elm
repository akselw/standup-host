module View.Button exposing (Button, Size(..), Variant(..), button, submit, toHtml, withCss, withLoading, withSize, withVariant)

import Css
import Css.Animations
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onClick)
import Svg.Styled as Svg exposing (svg)
import Svg.Styled.Attributes as SvgAttributes


button : msg -> String -> Button msg
button onClick label =
    Button
        { label = label
        , variant = Primary
        , size = Medium
        , type_ = TypeButton onClick
        , isLoading = False
        , css = []
        }


submit : String -> Button msg
submit label =
    Button
        { label = label
        , variant = Primary
        , size = Medium
        , type_ = TypeSubmit
        , isLoading = False
        , css = []
        }


type Button msg
    = Button (Options msg)


type alias Options msg =
    { label : String
    , variant : Variant
    , size : Size
    , type_ : ButtonType msg
    , isLoading : Bool
    , css : List Css.Style
    }


type ButtonType msg
    = TypeButton msg
    | TypeSubmit


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


withLoading : Bool -> Button msg -> Button msg
withLoading isLoading (Button options) =
    Button { options | isLoading = isLoading }


withCss : List Css.Style -> Button msg -> Button msg
withCss styles (Button options) =
    Button { options | css = options.css ++ styles }



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
        , Attributes.disabled options.isLoading
        , Attributes.css
            [ Css.batch options.css
            , Css.fontFamilies [ "Open Sans", "Helvetica Neue", "sans-serif" ]
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
            , Css.displayFlex
            , Css.alignItems Css.center
            , Css.property "gap" "4px"
            ]
        ]
        [ text options.label
        , if options.isLoading then
            loadingSpinner

          else
            text ""
        ]


{-| Hentet fra NAV sitt designsystem

<https://aksel.nav.no/komponenter/core/button#buttondemo-loading>

-}
loadingSpinner : Html msg
loadingSpinner =
    svg
        [ SvgAttributes.viewBox "0 0 50 50"
        , SvgAttributes.preserveAspectRatio "xMidYMid"
        , SvgAttributes.strokeWidth "7px"
        , SvgAttributes.css
            [ Css.width (Css.px 24)
            , Css.animationName (Css.Animations.keyframes [ ( 100, [ Css.Animations.transform [ Css.rotate (Css.turn 1) ] ] ) ])
            , Css.animationDuration (Css.sec 1.8)
            , Css.property "animation-timing-function" "linear"
            , Css.animationIterationCount Css.infinite
            ]
        ]
        [ Svg.circle
            [ SvgAttributes.cx "25"
            , SvgAttributes.cy "25"
            , SvgAttributes.r "20"
            , SvgAttributes.fill "none"
            , SvgAttributes.stroke "rgba(255, 255, 255, 0.3)"
            , SvgAttributes.strokeWidth "6.8px"
            ]
            []
        , Svg.circle
            [ SvgAttributes.cx "25"
            , SvgAttributes.cy "25"
            , SvgAttributes.r "20"
            , SvgAttributes.fill "none"
            , SvgAttributes.stroke "white"
            , SvgAttributes.strokeWidth "7px"
            , SvgAttributes.css
                [ Css.animationName
                    (Css.Animations.keyframes
                        [ ( 0
                          , [ Css.Animations.custom "stroke-dasharray" "1px,200px"
                            , Css.Animations.custom "stroke-dashoffset" "0"
                            ]
                          )
                        , ( 50
                          , [ Css.Animations.custom "stroke-dasharray" "100px,200px"
                            , Css.Animations.custom "stroke-dashoffset" "-15px"
                            ]
                          )
                        , ( 100
                          , [ Css.Animations.custom "stroke-dasharray" "1px,200px"
                            , Css.Animations.custom "stroke-dashoffset" "-120px"
                            ]
                          )
                        ]
                    )
                , Css.animationDuration (Css.sec 1.8)
                , Css.property "animation-timing-function" "ease-in-out"
                , Css.animationIterationCount Css.infinite
                ]
            ]
            []
        ]
