module Pages.Team_.Admin exposing (Model, Msg, page)

import AccessToken exposing (AccessToken)
import AdminTeam exposing (AdminTeam)
import Api
import DatabaseApiToken exposing (DatabaseApiToken)
import Effect exposing (Effect)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import View exposing (View)


page : Shared.Model -> Route { team : String } -> Page Model Msg
page shared route =
    Page.new
        { init =
            shared
                |> Shared.Model.accessToken
                |> init shared.apiKey route.params.team
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    {}


init : DatabaseApiToken -> String -> Maybe AccessToken -> () -> ( Model, Effect Msg )
init apiKey shortName maybeAccessToken () =
    ( {}
    , Api.getAdminTeam HentTeamResponse apiKey shortName
    )



-- UPDATE


type Msg
    = HentTeamResponse (Result AdminTeam.Error AdminTeam)


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
