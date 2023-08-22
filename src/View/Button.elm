module View.Button exposing (Button, button, toHtml)

import Css
import Html.Styled as Html exposing (Html, text)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onClick)


button : msg -> String -> Button msg
button onClick label =
    Button
        { label = label
        , onClick = onClick
        }


type Button msg
    = Button (Options msg)


type alias Options msg =
    { label : String
    , onClick : msg
    }


toHtml : Button msg -> Html msg
toHtml (Button options) =
    Html.button
        [ onClick options.onClick
        , Attributes.css
            [ Css.fontFamilies [ "Open Sans", "Helvetica Neue", "sans-serif" ]
            , Css.fontSize (Css.px 14)
            , Css.letterSpacing (Css.px 1.2)
            , Css.color (Css.hex "FFFFFF")
            , Css.backgroundColor (Css.hex "0D2F44")
            , Css.border Css.zero
            , Css.borderRadius (Css.rem 0.375)
            , Css.padding2 (Css.rem 0.625) (Css.rem 0.875)
            , Css.cursor Css.pointer
            , Css.hover [ Css.backgroundColor (Css.hex "334155") ]
            ]
        ]
        [ text options.label ]
