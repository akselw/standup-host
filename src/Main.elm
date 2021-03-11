module Main exposing (main)

import Browser
import Dato exposing (Dato)
import Html exposing (..)
import Html.Attributes exposing (class, type_)
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
        , valgtDag : ValgtDag
        }


type ValgtDag
    = Idag
    | NesteArbeidsdag


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
    | EndreFane ValgtDag


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
                        |> Dato.nesteArbeidsdag
                        |> Dato.toSeed
                        |> Random.step (Random.List.shuffle teamBruke)
                        |> Tuple.first
                , valgtDag = Idag
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

        EndreFane valgtDag ->
            case model of
                Init ->
                    ( model, Cmd.none )

                Success modelInfo ->
                    ( Success { modelInfo | valgtDag = valgtDag }, Cmd.none )



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
                            [ div []
                                [ button [ onClick (EndreFane Idag), type_ "button" ] [ text "I dag" ]
                                , button [ onClick (EndreFane NesteArbeidsdag), type_ "button" ]
                                    [ dagensDato
                                        |> Dato.nesteArbeidsdag
                                        |> Dato.toUkedagString
                                        |> text
                                    ]
                                ]
                            , h1 [] [ text standupVert ]
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
