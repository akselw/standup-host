module Main exposing (main)

import Browser
import Dato exposing (Dato)
import Html exposing (..)
import Html.Attributes exposing (class, classList, type_)
import Html.Events exposing (onClick)
import Process
import Random
import Random.List
import Task
import Time exposing (Month(..), Posix)



--- MODEL ---


type AnimationState
    = IkkeVisNoe
    | VisFørsteSetning
    | VisAndreSetning
    | VisStandupVert
    | VisKanIkkeTekst
    | VisAlt


type Model
    = Init
    | Success
        { dagensDato : Dato
        , dagensRekkefølge : List String
        , morgensdagensRekkefølge : List String
        , morgendagensAnimationState : AnimationState
        , valgtDag : ValgtDag
        }


type ValgtDag
    = Idag
    | NesteArbeidsdag


teamBruke : List String
teamBruke =
    [ "Abdifatah"
    , "Aksel"
    , "Sjoffa, for Aleksander er for sensitivt"
    , "Anne Mari"
    , "Arild"
    , "Arnstein"
    , "Ingrid"
    , "Tormod"
    , "Linn Pinn Sukkerspinn"
    , "Mia"
    , "Nga"
    ]



--- UPDATE ---


type Msg
    = TimeReceived Posix
    | VelgNyPersonIDag
    | VelgNyPersonNesteArbeidsdag
    | EndreFane ValgtDag
    | AnimationTick


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
                , morgendagensAnimationState = IkkeVisNoe
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

        VelgNyPersonNesteArbeidsdag ->
            case model of
                Init ->
                    ( model, Cmd.none )

                Success record ->
                    case record.morgensdagensRekkefølge of
                        _ :: rest ->
                            ( Success { record | morgensdagensRekkefølge = rest }, Cmd.none )

                        [] ->
                            ( model, Cmd.none )

        EndreFane valgtDag ->
            case model of
                Init ->
                    ( model, Cmd.none )

                Success modelInfo ->
                    ( Success { modelInfo | valgtDag = valgtDag }
                    , Process.sleep 500
                        |> Task.perform (\_ -> AnimationTick)
                    )

        AnimationTick ->
            case model of
                Init ->
                    ( model, Cmd.none )

                Success modelInfo ->
                    ( Success { modelInfo | morgendagensAnimationState = nesteAnimationState modelInfo.morgendagensAnimationState }
                    , case nesteAnimationState modelInfo.morgendagensAnimationState of
                        VisAlt ->
                            Cmd.none

                        _ ->
                            Process.sleep 1000
                                |> Task.perform (\_ -> AnimationTick)
                    )


nesteAnimationState : AnimationState -> AnimationState
nesteAnimationState animationState =
    case animationState of
        IkkeVisNoe ->
            VisFørsteSetning

        VisFørsteSetning ->
            VisAndreSetning

        VisAndreSetning ->
            VisStandupVert

        VisStandupVert ->
            VisKanIkkeTekst

        VisKanIkkeTekst ->
            VisAlt

        VisAlt ->
            VisAlt



--- VIEW ---


view : Model -> Html Msg
view model =
    div [ class "wrapper" ]
        [ div [ class "standup-host" ]
            (viewContent model)
        ]


viewContent : Model -> List (Html Msg)
viewContent model =
    case model of
        Init ->
            [ text "" ]

        Success { dagensDato, dagensRekkefølge, morgensdagensRekkefølge, valgtDag, morgendagensAnimationState } ->
            [ div []
                [ button [ onClick (EndreFane Idag), type_ "button" ] [ text "I dag" ]
                , button [ onClick (EndreFane NesteArbeidsdag), type_ "button" ]
                    [ dagensDato
                        |> Dato.nesteArbeidsdag
                        |> Dato.toUkedagString
                        |> text
                    ]
                ]
            , case valgtDag of
                Idag ->
                    viewIdag dagensRekkefølge dagensDato

                NesteArbeidsdag ->
                    viewNesteVirkedag morgendagensAnimationState morgensdagensRekkefølge dagensDato
            ]


viewIdag : List String -> Dato -> Html Msg
viewIdag rekkefølge dagensDato =
    case List.head rekkefølge of
        Just standupVert ->
            div [ class "wrapper" ]
                [ div [ class "standup-host" ]
                    [ h1 [] [ text standupVert ]
                    , p [] [ text ("skal holde standup i dag, " ++ String.toLower (Dato.toUkedagString dagensDato) ++ " " ++ Dato.toString dagensDato ++ ".") ]
                    , button
                        [ onClick VelgNyPersonIDag ]
                        [ text (standupVert ++ " kan ikke") ]
                    ]
                ]

        Nothing ->
            text "Da kunne visst ingen da..."


type AnimationElememt
    = FørsteSetning
    | AndreSetning
    | StandupVert
    | KanIkkeTekst
    | NesteKnapp


elementSkalVises : AnimationState -> AnimationElememt -> Bool
elementSkalVises animationState element =
    case ( animationState, element ) of
        ( IkkeVisNoe, _ ) ->
            False

        ( VisFørsteSetning, FørsteSetning ) ->
            True

        ( VisAndreSetning, FørsteSetning ) ->
            True

        ( VisAndreSetning, AndreSetning ) ->
            True

        ( VisStandupVert, FørsteSetning ) ->
            True

        ( VisStandupVert, AndreSetning ) ->
            True

        ( VisStandupVert, StandupVert ) ->
            True

        ( VisKanIkkeTekst, _ ) ->
            True

        ( VisAlt, _ ) ->
            True

        _ ->
            False


animationClasses : AnimationState -> AnimationElememt -> Html.Attribute msg
animationClasses animationState element =
    classList [ ( "fade-in", True ), ( "fade-in-hidden", not (elementSkalVises animationState element) ) ]


viewNesteVirkedag : AnimationState -> List String -> Dato -> Html Msg
viewNesteVirkedag animationState nesteVirkedagsRekkefølge dagensDato =
    case List.head nesteVirkedagsRekkefølge of
        Just standupVert ->
            div [ class "wrapper" ]
                [ div [ class "standup-host" ]
                    [ p [ animationClasses animationState FørsteSetning ]
                        [ text ("Den som skal holde standup på " ++ String.toLower (Dato.toUkedagString (Dato.nesteArbeidsdag dagensDato)))
                        ]
                    , p [ animationClasses animationState AndreSetning ]
                        [ text "er" ]
                    , h1 [ animationClasses animationState StandupVert ]
                        [ text standupVert ]
                    , button
                        [ animationClasses animationState NesteKnapp
                        , onClick VelgNyPersonNesteArbeidsdag
                        ]
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
