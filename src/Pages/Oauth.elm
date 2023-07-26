module Pages.Oauth exposing (Model, Msg, page)

import Auth
import Dict exposing (Dict)
import Effect exposing (Effect)
import Html
import Json.Encode
import LocalStorage
import Page exposing (Page)
import Route exposing (Route)
import Route.Path exposing (Path)
import Shared
import Url
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


a =
    Auth.onPageLoad


init : Route () -> () -> ( Model, Effect Msg )
init route () =
    let
        optionDict =
            route
                |> hashQueryParamFromRoute

        redirectPath =
            queryRedirectToRoute route
                -- TODO: Default burde være "min side"
                |> Maybe.withDefault Route.Path.Home_
    in
    case Dict.get "access_token" optionDict of
        Just accessToken ->
            ( {}
            , Effect.batch
                [ -- TODO: Legg til lagring i shared model
                  Effect.sendCmd (LocalStorage.setItem "hvem-har-standup:access_token" (Json.Encode.string accessToken))
                , Effect.replaceRoute { path = redirectPath, query = Dict.empty, hash = Nothing }
                ]
            )

        Nothing ->
            ( {}, Effect.none )


queryRedirectToRoute : Route () -> Maybe Path
queryRedirectToRoute route =
    route.query
        |> Dict.get "redirect"
        |> Maybe.andThen Route.Path.fromString


hashQueryParamFromRoute : Route a -> Dict String String
hashQueryParamFromRoute { hash } =
    let
        keyValueStringToTuple string =
            case String.split "=" string of
                key :: value :: _ ->
                    Just ( key, value )

                _ ->
                    Nothing
    in
    case hash of
        Just hashString ->
            hashString
                |> String.split "&"
                |> List.filterMap keyValueStringToTuple
                |> Dict.fromList

        Nothing ->
            Dict.empty



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
    View.fromString "Pages.Oauth"
