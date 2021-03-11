module Main exposing (main)

import Browser
import Dato exposing (Dato)
import Html exposing (..)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import Random
import Random.List
import Task
import Time exposing (Month(..), Posix)



--- MODEL ---


type Model
    = Init
    | Success
        { dagensDato : Dato
        , dagensRekkefølge : List String
        , morgensdagensRekkefølge : List String
        }


teamBruke : List String
teamBruke =
    [ "Abdifatah"
    , "Aksel"
    , "Aleksander"
    , "Anne Mari"
    , "Arild"
    , "Arnstein"
    , "Ingrid"
    , "Tormod"
    ]



--- UPDATE ---


type Msg
    = TimeReceived Posix
    | VelgNyPersonIDag


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeReceived posix ->
            let
                dato =
                    Dato.fromPosix posix
            in
            ( Success
                { dagensDato = dato
                , dagensRekkefølge =
                    dato
                        |> Dato.toSeed
                        |> Random.step (Random.List.shuffle teamBruke)
                        |> Tuple.first
                , morgensdagensRekkefølge =
                    dato
                        |> Dato.tomorrow
                        |> Dato.toSeed
                        |> Random.step (Random.List.shuffle teamBruke)
                        |> Tuple.first
                }
            , Cmd.none
            )

        VelgNyPersonIDag ->
            case model of
                Init ->
                    ( model, Cmd.none )

                Success record ->
                    case record.dagensRekkefølge of
                        _ :: rest ->
                            ( Success { record | dagensRekkefølge = rest }, Cmd.none )

                        [] ->
                            ( model, Cmd.none )



--- VIEW ---


view : Model -> Html Msg
view model =
    case model of
        Init ->
            text ""

        Success { dagensDato, dagensRekkefølge } ->
            case List.head dagensRekkefølge of
                Just standupVert ->
                    div [ class "wrapper" ]
                        [ div [ class "standup-host" ]
                            [ h1 [] [ text standupVert ]
                            , p [] [ text "skal holde standup" ]
                            , p [] [ text (Dato.toString dagensDato) ]
                            , button
                                [ onClick VelgNyPersonIDag ]
                                [ text (standupVert ++ " kan ikke") ]
                            ]
                        ]

                Nothing ->
                    div [ class "wrapper" ]
                        [ div [ class "standup-host" ]
                            [ text "Da kunne visst ingen da..."
                            ]
                        ]



--- MAIN ---


init : () -> ( Model, Cmd Msg )
init _ =
    ( Init
    , Time.now
        |> Task.perform TimeReceived
    )


main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = always Sub.none
        }
