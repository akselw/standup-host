module Layouts.Header exposing (Model, Msg, Props, layout)

import AccessToken exposing (AccessToken)
import Css
import Css.Global
import Css.Media
import Effect exposing (Effect)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (class)
import Html.Styled.Events exposing (onClick)
import Layout exposing (Layout)
import Route exposing (Route)
import Route.Path
import RouteExtras
import Shared
import Shared.Model
import View exposing (View)
import View.Button as Button


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
                , Css.fontFamilies [ "Open Sans", "Helvetica Neue", "sans-serif" ]
                ]
            ]
        , viewHeader accessToken
            |> Html.map toContentMsg
        , div [] content.body
        ]
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
            ]
        ]
        [ a
            [ RouteExtras.href Route.Path.Home_
            , Attributes.css
                [ Css.fontSize (Css.px 18)
                , Css.fontWeight (Css.int 700)
                , Css.color (Css.hex "040F16")
                , Css.textDecoration Css.none
                ]
            ]
            [ text "Hvem har standup?" ]
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
        [ a
            [ RouteExtras.href Route.Path.MineTeam
            , Attributes.css
                [ Css.color (Css.hex "040F16")
                , Css.textDecoration Css.none
                ]
            ]
            [ text "Mine team" ]
        , Button.button LoggUtKnappTrykket "Logg ut"
            |> Button.toHtml
        ]


viewNonLoggedInButtons : Html msg
viewNonLoggedInButtons =
    nav []
        [ a
            [ RouteExtras.href Route.Path.Login
            , Attributes.css
                [ Css.fontFamilies [ "Open Sans", "Helvetica Neue", "sans-serif" ]
                , Css.fontSize (Css.px 14)
                , Css.letterSpacing (Css.px 1.2)
                , Css.color (Css.hex "FFFFFF")
                , Css.backgroundColor (Css.hex "4F46E5")
                , Css.border Css.zero
                , Css.borderRadius (Css.rem 0.375)
                , Css.padding2 (Css.rem 0.625) (Css.rem 0.875)
                , Css.cursor Css.pointer
                , Css.hover [ Css.backgroundColor (Css.hex "6366f1") ]
                , Css.textDecoration Css.none
                ]
            ]
            [ text "Logg inn" ]
        ]
