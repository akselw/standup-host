module Layouts.Header exposing (Model, Msg, Props, layout)

import AccessToken exposing (AccessToken)
import Css
import Css.Global
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
        [ Css.Global.global [ Css.Global.selector "body" [ Css.margin Css.zero ] ]
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
        [ a [ RouteExtras.href Route.Path.Home_ ] [ text "Hvem har standup?" ]
        , case maybeAccessToken of
            Just accessToken ->
                viewLoggedInButtons accessToken

            Nothing ->
                viewNonLoggedInButtons
        ]


viewLoggedInButtons : AccessToken -> Html Msg
viewLoggedInButtons accessToken =
    nav []
        [ a [ RouteExtras.href Route.Path.MinSide ] [ text "Mine team" ]
        , button [ onClick LoggUtKnappTrykket ] [ text "Logg ut" ]
        ]


viewNonLoggedInButtons : Html msg
viewNonLoggedInButtons =
    nav []
        [ a [ RouteExtras.href Route.Path.Login ] [ text "Logg inn" ]
        ]
