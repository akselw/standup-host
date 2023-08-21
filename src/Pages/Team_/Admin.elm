module Pages.Team_.Admin exposing (Model, Msg, page)

import AccessToken exposing (AccessToken)
import Api
import Auth
import DatabaseApiToken exposing (DatabaseApiToken)
import Effect exposing (Effect)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Team exposing (Team)
import View exposing (View)


page : Auth.User -> Shared.Model -> Route { team : String } -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared.apiKey route.params.team user.accessToken
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    {}


init : DatabaseApiToken -> String -> AccessToken -> () -> ( Model, Effect Msg )
init apiKey shortName accessToken () =
    ( {}
    , Api.getTeam HentTeamResponse apiKey shortName
    )



-- UPDATE


type Msg
    = HentTeamResponse (Result Team.Error Team)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        HentTeamResponse (Ok team) ->
            let
                _ =
                    Debug.log "team" team
            in
            ( model
            , Effect.none
            )

        HentTeamResponse (Err error) ->
            let
                _ =
                    Debug.log "error" error
            in
            ( model
            , Effect.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    View.fromString "Pages.Team_.Admin"
