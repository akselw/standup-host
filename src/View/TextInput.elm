module View.TextInput exposing (TextInput, input, toHtml, withCss, withDisabled)

import Css
import Html.Styled as Html exposing (Html)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onInput)


input : { msg : String -> msg, label : String } -> String -> TextInput msg
input { msg, label } value =
    TextInput
        { label = label
        , msg = msg
        , value = value
        , disabled = False
        , css = []
        }


type TextInput msg
    = TextInput (Options msg)


type alias Options msg =
    { label : String
    , msg : String -> msg
    , value : String
    , disabled : Bool
    , css : List Css.Style
    }



--- Options ---


withDisabled : Bool -> TextInput msg -> TextInput msg
withDisabled disabled (TextInput options) =
    TextInput { options | disabled = disabled }


withCss : List Css.Style -> TextInput msg -> TextInput msg
withCss styles (TextInput options) =
    TextInput { options | css = options.css ++ styles }



--- Html ---


toHtml : TextInput msg -> Html msg
toHtml (TextInput options) =
    Html.label
        [ Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.batch options.css
            ]
        ]
        [ Html.span
            [ Attributes.css
                [ Css.fontSize (Css.px 14)
                , Css.fontWeight (Css.int 700)
                , Css.marginBottom (Css.px 4)
                ]
            ]
            [ Html.text options.label ]
        , Html.input
            [ Attributes.value options.value
            , Attributes.disabled options.disabled
            , onInput options.msg
            , Attributes.css
                [ Css.padding2 (Css.px 8) (Css.px 16)
                , Css.borderRadius (Css.px 6)
                , Css.border3 (Css.px 1) Css.solid (Css.hex "979FAF")
                , Css.fontFamilies [ "Open Sans", "Helvetica Neue", "sans-serif" ]
                , Css.fontSize (Css.px 14)
                ]
            ]
            []
        ]
