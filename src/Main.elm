module Main exposing (main)

import Browser
import Dato exposing (Dato)
import Html exposing (..)
import Html.Attributes exposing (class, classList, type_)
import Html.Events exposing (onClick)
import Pages.Team
import Process
import Random
import Random.List
import Task
import Time exposing (Month(..), Posix)



--- MODEL ---


type Model
    = HomePage
    | TeamPage Pages.Team.Model
    | TeamNotFound String



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



--- VIEW ---


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ div [ class "standup-host" ]
            [ case model of
                HomePage ->
                    text "home"

                TeamPage teamPageModel ->
                    Pages.Team.view teamPageModel
                        |> Html.map TeamPageMsg

                TeamNotFound string ->
                    text ("not found" ++ string)
            ]
        ]



--- MAIN ---


init : () -> ( Model, Cmd Msg )
init _ =
    let
        ( teamPageModel, teamPageCmd ) =
            Pages.Team.init ()
    in
    ( TeamPage teamPageModel
    , teamPageCmd
        |> Cmd.map TeamPageMsg
    )


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
