module Pages.Home_ exposing (..)

import Authentication
import Effect exposing (Effect)
import Html.Styled as Html exposing (..)
import Html.Styled.Events exposing (onClick)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import View exposing (View)


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- INIT


type alias Model =
    {}


init : () -> ( Model, Effect Msg )
init () =
    ( {}
    , Effect.none
    )



-- UPDATE


type Msg
    = LoginClicked


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        LoginClicked ->
            ( model
            , Authentication.login
                |> Effect.sendCmd
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Pages.Home_"
    , body = [ button [ onClick LoginClicked ] [ text "Log in" ] ]
    }
