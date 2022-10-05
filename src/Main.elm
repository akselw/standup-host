module Main exposing (main)

import Browser exposing (Document)
import Browser.Navigation as Navigation
import Dato exposing (Dato)
import Html exposing (..)
import Html.Attributes exposing (class, classList, type_)
import Html.Events exposing (onClick)
import Pages.Team
import Process
import Random
import Random.List
import Route
import Task
import Time exposing (Month(..), Posix)
import Url



--- MODEL ---


type Model
    = HomePage
    | TeamPage Pages.Team.Model
    | TeamNotFound String
    | NotFound



--- UPDATE ---


type Msg
    = HomePageMsg
    | TeamPageMsg Pages.Team.Msg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model, msg ) of
        ( TeamPage pageModel, TeamPageMsg pageMsg ) ->
            let
                ( newPageModel, pageCmd ) =
                    Pages.Team.update pageMsg pageModel
            in
            ( TeamPage newPageModel
            , pageCmd |> Cmd.map TeamPageMsg
            )

        _ ->
            ( model, Cmd.none )


updateWith : (subModel -> Model) -> (subMsg -> Msg) -> Model -> ( subModel, Cmd subMsg ) -> ( Model, Cmd Msg )
updateWith toModel toMsg model ( subModel, subCmd ) =
    ( toModel subModel
    , Cmd.map toMsg subCmd
    )



--- VIEW ---


view : Model -> Document Msg
view model =
    { title = "Hvem har standup?"
    , body =
        [ div [ class "wrapper" ]
            [ div [ class "standup-host" ]
                [ case model of
                    HomePage ->
                        text "home"

                    TeamPage teamPageModel ->
                        Pages.Team.view teamPageModel
                            |> Html.map TeamPageMsg

                    TeamNotFound string ->
                        text ("not found" ++ string)

                    NotFound ->
                        text "not found"
                ]
            ]
        ]
    }



--- MAIN ---


init : () -> Url.Url -> Navigation.Key -> ( Model, Cmd Msg )
init _ url key =
    case Route.fromUrl url of
        Nothing ->
            ( NotFound, Cmd.none )

        Just Route.Home ->
            ( NotFound, Cmd.none )

        Just (Route.Team shortname) ->
            Pages.Team.init ()
                |> updateWith TeamPage TeamPageMsg HomePage


main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        , onUrlChange = always HomePageMsg
        , onUrlRequest = always HomePageMsg
        }
