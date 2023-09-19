module View.Media exposing (Breakpoint(..), breakpoint)

import Css
import Css.Media


type Breakpoint
    = Tablet


breakpoint : Breakpoint -> List Css.Style -> Css.Style
breakpoint breakpoint_ styles =
    Css.Media.withMedia [ Css.Media.all [ Css.Media.minWidth (Css.px (breakpointToPx breakpoint_)) ] ] styles


breakpointToPx : Breakpoint -> Float
breakpointToPx breakpoint_ =
    case breakpoint_ of
        Tablet ->
            480
