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
        { dato : Dato
        , rekkefølge : List String
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
    | VelgNyPerson


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeReceived posix ->
            let
                dato =
                    Dato.fromPosix posix
            in
            ( Success
                { dato = dato
                , rekkefølge =
                    dato
                        |> Dato.toSeed
                        |> Random.step (Random.List.shuffle teamBruke)
                        |> Tuple.first
                }
            , Cmd.none
            )

        VelgNyPerson ->
            case model of
                Init ->
                    ( model, Cmd.none )

                Success record ->
                    case record.rekkefølge of
                        _ :: rest ->
                            ( Success { record | rekkefølge = rest }, Cmd.none )

                        [] ->
                            ( model, Cmd.none )



--- VIEW ---


view : Model -> Html Msg
view model =
    case model of
        Init ->
            text ""

        Success { dato, rekkefølge } ->
            case List.head rekkefølge of
                Just standupVert ->
                    div [ class "wrapper" ]
                        [ div [ class "standup-host" ]
                            [ h1 [] [ text standupVert ]
                            , p [] [ text "skal holde standup" ]
                            , p [] [ text (Dato.toString dato) ]
                            , button
                                [ onClick VelgNyPerson ]
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
