module Pages.Login exposing (Model, Msg, page)

import Authentication
import Dict
import Effect exposing (Effect)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import Shared.Model exposing (AccessTokenStatus(..))
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init shared route
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    {}


init : Shared.Model -> Route () -> () -> ( Model, Effect Msg )
init shared route () =
    ( {}
    , case shared.accessToken of
        Token _ ->
            route.query
                |> Dict.get Authentication.redirectKey
                |> Maybe.andThen Route.Path.fromString
                |> Maybe.withDefault Route.Path.MineTeam
                |> (\redirectPath ->
                        Effect.pushRoute
                            { path = redirectPath
                            , query = Dict.empty
                            , hash = Nothing
                            }
                   )

        _ ->
            route.query
                |> Dict.get Authentication.redirectKey
                |> Maybe.andThen Route.Path.fromString
                |> Maybe.map Authentication.loginWithRedirectUrl
                |> Maybe.withDefault Authentication.login
                |> Effect.sendCmd
    )



-- UPDATE


type Msg
    = ExampleMsgReplaceMe


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        ExampleMsgReplaceMe ->
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
    View.fromString "Pages.Login"
