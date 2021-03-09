module Main exposing (main)

import Browser
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
        { time : Posix
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


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


dateToInt : Posix -> Int
dateToInt posix =
    let
        year =
            Time.toYear Time.utc posix

        month =
            posix
                |> Time.toMonth Time.utc
                |> monthToInt

        day =
            Time.toDay Time.utc posix
    in
    Debug.log "int" (year * 10000 + month * 100 + day)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeReceived pos ->
            ( Success
                { time = pos
                , rekkefølge =
                    pos
                        |> dateToInt
                        |> Random.initialSeed
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

        Success { rekkefølge } ->
            case List.head rekkefølge of
                Just standupVert ->
                    div [ class "wrapper" ]
                        [ div [ class "standup-host" ]
                            [ h1 [] [ text standupVert ]
                            , p [] [ text "skal holde standup" ]
                            , button
                                [ onClick VelgNyPerson ]
                                [ text (standupVert ++ " kan ikke") ]
                            ]
                        ]

                Nothing ->
                    text "Da kunne visst ingen da..."



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
