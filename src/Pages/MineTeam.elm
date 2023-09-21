module Pages.MineTeam exposing (Model, Msg, page)

import Api
import Auth
import Css
import DatabaseApiToken exposing (DatabaseApiToken)
import Effect exposing (Effect)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Http
import Layouts
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import TeamSummary exposing (TeamSummary)
import View exposing (View)
import View.Button as Button
import View.Link as Link
import View.LinkButton as LinkButton
import View.Page as Page


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared route =
    Page.new
        { init = init user shared.apiKey
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout toLayout


{-| Use the header layout on this page
-}
toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.Header {}



-- INIT


type Model
    = Loading
    | Failure Http.Error
    | Success (List TeamSummary)


init : Auth.User -> DatabaseApiToken -> () -> ( Model, Effect Msg )
init user apiKey () =
    ( Loading
    , Api.getTeamsForUser apiKey GetTeamsResponse user.accessToken
    )



-- UPDATE


type Msg
    = GetTeamsResponse (Result Http.Error (List TeamSummary))
    | NoOp


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        GetTeamsResponse (Ok teams) ->
            ( Success teams
            , Effect.none
            )

        GetTeamsResponse (Err error) ->
            ( Failure error
            , Effect.none
            )

        NoOp ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Hvem har standup? | Mine team"
    , body =
        case model of
            Loading ->
                []

            Failure error ->
                []

            Success teams ->
                Page.viewPageWrapper
                    [ h1 [] [ text "Mine team" ]
                    , div
                        [ Attributes.css
                            [ Css.displayFlex
                            , Css.flexDirection Css.column
                            , Css.border3 (Css.px 1) Css.solid borderColor
                            , Css.borderRadius (Css.px 6)
                            , Css.marginBottom (Css.px 16)
                            ]
                        ]
                        (teams
                            |> List.map viewTeam
                        )
                    ]
    }


borderColor : Css.Color
borderColor =
    Css.hex "979FAF"


viewTeam : TeamSummary -> Html Msg
viewTeam team =
    div
        [ Attributes.css
            [ Css.padding2 (Css.px 16) (Css.px 24)
            , Css.borderTop3 (Css.px 1) Css.solid borderColor
            , Css.firstOfType [ Css.borderWidth Css.zero ]
            ]
        ]
        [ div
            [ Attributes.css
                [ Css.displayFlex
                , Css.flexDirection Css.row
                , Css.flexWrap Css.wrap
                , Css.alignItems Css.center
                , Css.property "gap" "12px"
                , Css.width (Css.pct 100)
                , Css.justifyContent Css.spaceBetween
                ]
            ]
            [ Link.link (Route.Path.Team_ { team = TeamSummary.shortname team }) [ text (TeamSummary.navn team) ]
                |> Link.toHtml
            , LinkButton.linkButton (Route.Path.Team__Settings { team = TeamSummary.shortname team }) [ text "Innstillinger" ]
                |> LinkButton.withVariant Button.Secondary
                |> LinkButton.toHtml
            ]
        ]
