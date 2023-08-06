module Pages.Login exposing (Model, Msg, page)

import Authentication
import Dict
import Effect exposing (Effect)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init route
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    {}


init : Route () -> () -> ( Model, Effect Msg )
init route () =
    ( {}
    , route.query
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
