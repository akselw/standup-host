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



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Min side"
    , body =
        case model of
            Loading ->
                []

            Failure error ->
                []

            Success teams ->
                [ h2 [] [ text "Mine team" ]
                , div [ Attributes.css [ Css.displayFlex, Css.flexDirection Css.column ] ]
                    (teams
                        |> List.map viewTeam
                    )
                ]
    }


viewTeam : TeamSummary -> Html msg
viewTeam team =
    a [ Attributes.href (Route.Path.toString (Route.Path.Team_ { team = TeamSummary.shortname team })) ]
        [ text (TeamSummary.navn team) ]
