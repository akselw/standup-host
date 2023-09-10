module View.TextInput exposing
    ( TextInput
    , input
    , withId
    , withDisabled
    , StatusIcon(..), withStatusIcon
    , withCss
    , toHtml
    )

{-|

@docs TextInput
@docs input
@docs withId
@docs withDisabled
@docs StatusIcon, withStatusIcon
@docs withCss
@docs toHtml

-}

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
        , id = Nothing
        , disabled = False
        , statusIcon = Nothing
        , css = []
        }


type TextInput msg
    = TextInput (Options msg)


type alias Options msg =
    { label : String
    , msg : String -> msg
    , value : String
    , id : Maybe String
    , disabled : Bool
    , statusIcon : Maybe StatusIcon
    , css : List Css.Style
    }


type StatusIcon
    = LoadingSpinner
    | Checkmark
    | Error



--- Options ---


withId : String -> TextInput msg -> TextInput msg
withId id (TextInput options) =
    TextInput { options | id = Just id }


withDisabled : Bool -> TextInput msg -> TextInput msg
withDisabled disabled (TextInput options) =
    TextInput { options | disabled = disabled }


withStatusIcon : Maybe StatusIcon -> TextInput msg -> TextInput msg
withStatusIcon statusIcon (TextInput options) =
    TextInput { options | statusIcon = statusIcon }


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
                , backgroundImage options.statusIcon
                ]
            , options.id
                |> Maybe.map Attributes.id
                |> Maybe.withDefault (Attributes.classList [])
            ]
            []
        ]


backgroundImage : Maybe StatusIcon -> Css.Style
backgroundImage maybeStatusIcon =
    case maybeStatusIcon of
        Just icon ->
            Css.batch
                [ Css.backgroundRepeat Css.noRepeat
                , Css.property "background-position" "right 8px center"
                , case icon of
                    LoadingSpinner ->
                        Css.backgroundImage (Css.url "data:image/svg+xml;base64,PHN2ZyB2aWV3Qm94PSIwIDAgNTAgNTAiIHByZXNlcnZlQXNwZWN0UmF0aW89InhNaWRZTWlkIiBzdHJva2Utd2lkdGg9IjdweCIgY2xhc3M9InRleHQtaW5wdXQtbG9hZGluZy1zcGlubmVyIj4KICAgIDxjaXJjbGUgY3g9IjI1IiBjeT0iMjUiIHI9IjIwIiBmaWxsPSJub25lIiBzdHJva2U9InJnYmEoMTcsIDQxLCA2NCwgMC4xMykiIHN0cm9rZS13aWR0aD0iNi44cHgiLz4KICAgIDxjaXJjbGUgY3g9IjI1IiBjeT0iMjUiIHI9IjIwIiBmaWxsPSJub25lIiBzdHJva2U9IiMyMzI2MmEiIHN0cm9rZS13aWR0aD0iN3B4IiBjbGFzcz0idGV4dC1pbnB1dC1sb2FkaW5nLXNwaW5uZXJfX2NpY2xlIi8+Cjwvc3ZnPgo=")

                    Checkmark ->
                        Css.backgroundImage (Css.url "data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMjQiIGhlaWdodD0iMjQiIHZpZXdCb3g9IjAgMCAyNCAyNCIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPHBhdGggZmlsbC1ydWxlPSJldmVub2RkIiBjbGlwLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0xOC45OTgzIDYuOTM5NDNDMTkuMzA3OSA3LjIxNDYyIDE5LjMzNTcgNy42ODg2NyAxOS4wNjA2IDcuOTk4MjZMMTEuMDYwNiAxNi45OTgzQzEwLjkyMzMgMTcuMTUyNiAxMC43Mjg1IDE3LjI0MzYgMTAuNTIyIDE3LjI0OTdDMTAuMzE1NiAxNy4yNTU3IDEwLjExNTcgMTcuMTc2NCA5Ljk2OTY3IDE3LjAzMDNMNC45Njk2NyAxMi4wMzAzQzQuNjc2NzggMTEuNzM3NCA0LjY3Njc4IDExLjI2MjYgNC45Njk2NyAxMC45Njk3QzUuMjYyNTYgMTAuNjc2OCA1LjczNzQ0IDEwLjY3NjggNi4wMzAzMyAxMC45Njk3TDEwLjQ2NzkgMTUuNDA3MkwxNy45Mzk0IDcuMDAxNzJDMTguMjE0NiA2LjY5MjEzIDE4LjY4ODcgNi42NjQyNCAxOC45OTgzIDYuOTM5NDNaIiBmaWxsPSIjMDY4OTNBIi8+Cjwvc3ZnPgo=")

                    Error ->
                        Css.backgroundImage (Css.url "data:image/svg+xml;base64,PHN2ZyB3aWR0aD0iMjQiIGhlaWdodD0iMjQiIHZpZXdCb3g9IjAgMCAyNCAyNCIgZmlsbD0ibm9uZSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIj4KPHBhdGggZmlsbC1ydWxlPSJldmVub2RkIiBjbGlwLXJ1bGU9ImV2ZW5vZGQiIGQ9Ik0xMiAyLjI1QzEyLjI3MzEgMi4yNSAxMi41MjQ1IDIuMzk4NCAxMi42NTY1IDIuNjM3NDNMMjIuMTgyOCAxOS44ODc0QzIyLjMxMTEgMjAuMTE5OCAyMi4zMDcxIDIwLjQwMjYgMjIuMTcyMiAyMC42MzEyQzIyLjAzNzMgMjAuODU5NyAyMS43OTE3IDIxIDIxLjUyNjMgMjFIMi40NzM3MkMyLjIwODMyIDIxIDEuOTYyNjggMjAuODU5NyAxLjgyNzggMjAuNjMxMkMxLjY5MjkyIDIwLjQwMjYgMS42ODg4OCAyMC4xMTk4IDEuODE3MTggMTkuODg3NEwxMS4zNDM1IDIuNjM3NDNDMTEuNDc1NSAyLjM5ODQgMTEuNzI2OSAyLjI1IDEyIDIuMjVaTTIwLjI1NTMgMTkuNUwxMiA0LjU1MTQyTDMuNzQ0NjcgMTkuNUgyMC4yNTUzWk0xMiA4Ljc1QzEyLjQxNDIgOC43NSAxMi43NSA5LjA4NTc5IDEyLjc1IDkuNVYxMy41QzEyLjc1IDEzLjkxNDIgMTIuNDE0MiAxNC4yNSAxMiAxNC4yNUMxMS41ODU4IDE0LjI1IDExLjI1IDEzLjkxNDIgMTEuMjUgMTMuNVY5LjVDMTEuMjUgOS4wODU3OSAxMS41ODU4IDguNzUgMTIgOC43NVpNMTIgMTUuNUMxMS40NDc3IDE1LjUgMTEgMTUuOTQ3NyAxMSAxNi41QzExIDE3LjA1MjMgMTEuNDQ3NyAxNy41IDEyIDE3LjVDMTIuNTUyMyAxNy41IDEzIDE3LjA1MjMgMTMgMTYuNUMxMyAxNS45NDc3IDEyLjU1MjMgMTUuNSAxMiAxNS41WiIgZmlsbD0iI0MzMDAwMCIvPgo8L3N2Zz4K")
                ]

        Nothing ->
            Css.batch []
