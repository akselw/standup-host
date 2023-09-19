module Layouts.Header exposing (Model, Msg, Props, layout)

import AccessToken exposing (AccessToken)
import Css
import Css.Global
import Css.Media
import Effect exposing (Effect)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model
import View exposing (View)
import View.Button as Button
import View.Link as Link
import View.LinkButton as LinkButton


type alias Props =
    {}


layout : Props -> Shared.Model -> Route () -> Layout () Model Msg contentMsg
layout props shared route =
    Layout.new
        { init = init
        , update = update
        , view =
            shared
                |> Shared.Model.accessToken
                |> view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init _ =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = LoggUtKnappTrykket


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        LoggUtKnappTrykket ->
            ( model
            , Effect.logout
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Maybe AccessToken -> { toContentMsg : Msg -> contentMsg, content : View contentMsg, model : Model } -> View contentMsg
view accessToken { toContentMsg, model, content } =
    { title = content.title
    , body =
        [ Css.Global.global
            [ Css.Global.selector "body"
                [ Css.margin Css.zero
                , Css.property "min-height" "100vh"
                , Css.displayFlex
                , Css.flexDirection Css.column
                , Css.fontFamilies [ "Open Sans", "Helvetica Neue", "sans-serif" ]
                ]
            ]
        , viewHeader accessToken
            |> Html.map toContentMsg
        ]
            ++ [ div [] content.body ]
            ++ [ viewFooter ]
    }


viewHeader : Maybe AccessToken -> Html Msg
viewHeader maybeAccessToken =
    header
        [ Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.justifyContent Css.spaceBetween
            , Css.alignItems Css.center
            , Css.minHeight (Css.px 64)
            , Css.padding2 Css.zero (Css.px 16)
            , Css.marginBottom (Css.px 32)
            ]
        ]
        [ Link.link Route.Path.Home_ [ text "Hvem har standup?" ]
            |> Link.withoutColor
            |> Link.withCss
                [ Css.fontSize (Css.px 18)
                , Css.fontWeight (Css.int 700)
                ]
            |> Link.toHtml
        , case maybeAccessToken of
            Just accessToken ->
                viewLoggedInButtons accessToken

            Nothing ->
                viewNonLoggedInButtons
        ]


viewLoggedInButtons : AccessToken -> Html Msg
viewLoggedInButtons accessToken =
    nav
        [ Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.row
            , Css.alignItems Css.center
            , Css.property "gap" "16px"
            , Css.Media.withMediaQuery [ "screen and (min-width: 756px)" ]
                [ Css.property "gap" "32px"
                ]
            ]
        ]
        [ Link.link Route.Path.MineTeam [ text "Mine team" ]
            |> Link.withoutColor
            |> Link.toHtml
        , Button.button LoggUtKnappTrykket "Logg ut"
            |> Button.withVariant Button.Tertiary
            |> Button.withSize Button.Large
            |> Button.toHtml
        ]


viewNonLoggedInButtons : Html msg
viewNonLoggedInButtons =
    nav []
        [ LinkButton.linkButton Route.Path.Login [ text "Logg inn" ]
            |> LinkButton.withSize Button.Large
            |> LinkButton.toHtml
        ]


viewFooter : Html msg
viewFooter =
    footer
        [ Attributes.css
            [ Css.paddingTop (Css.px 128)
            , Css.marginTop Css.auto
            , Css.marginBottom (Css.px 40)
            , Css.displayFlex
            , Css.flexDirection Css.column
            , Css.alignItems Css.center
            , Css.justifyContent Css.end
            , Css.fontSize (Css.px 13)
            , Css.color (Css.hex "979FAF")
            ]
        ]
        [ span [] [ text "Laget av" ]
        , span [] [ text "Aksel Wester" ]
        ]
