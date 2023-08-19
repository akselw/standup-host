module Pages.Team_ exposing (Model, Msg, page)

import Api
import Css
import DatabaseApiToken exposing (DatabaseApiToken)
import Dato exposing (Dato)
import Effect exposing (Effect)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (type_)
import Html.Styled.Events exposing (onClick)
import Page exposing (Page)
import Process
import Random
import Random.List
import Route exposing (Route)
import Shared
import Task
import Team exposing (Team)
import Teammedlem exposing (Teammedlem)
import Time exposing (Month(..), Posix)
import View exposing (View)



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
    | LoadingTeam Dato
    | Failure Team.Error
    | Success
        { dagensDato : Dato
        , dagensRekkefølge : List Teammedlem
        , morgensdagensRekkefølge : List Teammedlem
        , morgendagensAnimationState : AnimationState
        , valgtDag : ValgtDag
        , team : Team
        }


type ValgtDag
    = Idag
    | NesteArbeidsdag



--- UPDATE ---


type Msg
    = TimeReceived Posix
    | VelgNyPersonIDag
    | VelgNyPersonNesteArbeidsdag
    | EndreFane ValgtDag
    | HentTeamResponse (Result Team.Error Team)
    | AnimationTick


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        TimeReceived posix ->
            let
                dato =
                    Dato.fromPosix posix
            in
            ( LoadingTeam dato, Effect.none )

        HentTeamResponse result ->
            case model of
                LoadingTeam dato ->
                    case result of
                        Ok team ->
                            ( Success
                                { dagensDato = dato
                                , dagensRekkefølge =
                                    dato
                                        |> Dato.toSeed
                                        |> Random.step (Random.List.shuffle (Team.medlemmer team))
                                        |> Tuple.first
                                , morgensdagensRekkefølge =
                                    dato
                                        |> Dato.nesteArbeidsdag
                                        |> Dato.toSeed
                                        |> Random.step (Random.List.shuffle (Team.medlemmer team))
                                        |> Tuple.first
                                , valgtDag = Idag
                                , morgendagensAnimationState = IkkeVisNoe
                                , team = team
                                }
                            , Effect.none
                            )

                        Err error ->
                            ( Failure error, Effect.none )

                _ ->
                    ( model, Effect.none )

        VelgNyPersonIDag ->
            case model of
                Success record ->
                    case record.dagensRekkefølge of
                        _ :: rest ->
                            ( Success { record | dagensRekkefølge = rest }, Effect.none )

                        [] ->
                            ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        VelgNyPersonNesteArbeidsdag ->
            case model of
                Success record ->
                    case record.morgensdagensRekkefølge of
                        _ :: rest ->
                            ( Success { record | morgensdagensRekkefølge = rest }, Effect.none )

                        [] ->
                            ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        EndreFane valgtDag ->
            case model of
                Success modelInfo ->
                    ( Success { modelInfo | valgtDag = valgtDag }
                    , Process.sleep 500
                        |> Task.perform (\_ -> AnimationTick)
                        |> Effect.sendCmd
                    )

                _ ->
                    ( model, Effect.none )

        AnimationTick ->
            case model of
                Success modelInfo ->
                    ( Success { modelInfo | morgendagensAnimationState = nesteAnimationState modelInfo.morgendagensAnimationState }
                    , case nesteAnimationState modelInfo.morgendagensAnimationState of
                        VisAlt ->
                            Effect.none

                        _ ->
                            Process.sleep 1000
                                |> Task.perform (\_ -> AnimationTick)
                                |> Effect.sendCmd
                    )

                _ ->
                    ( model, Effect.none )


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


view : Model -> View Msg
view model =
    { title = "Pages.Team_"
    , body =
        [ div
            [ Attributes.css
                [ Css.displayFlex
                , Css.flexDirection Css.column
                , Css.fontFamilies [ "Open Sans", "sans-serif" ]
                , Css.fontSize (Css.px 14)
                , Css.width (Css.pct 100)
                , Css.color (Css.rgb 51 51 51)
                , Css.alignItems Css.center
                ]
            ]
            (viewContent model)
        ]
    }


viewContent : Model -> List (Html Msg)
viewContent model =
    case model of
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

        _ ->
            [ text "" ]


viewIdag : List Teammedlem -> Dato -> Html Msg
viewIdag rekkefølge dagensDato =
    case List.head rekkefølge of
        Just standupVert ->
            div
                [ Attributes.css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.alignItems Css.center
                    ]
                ]
                [ h1 [] [ text (Teammedlem.navn standupVert) ]
                , p [] [ text ("skal holde standup i dag, " ++ String.toLower (Dato.toUkedagString dagensDato) ++ " " ++ Dato.toString dagensDato ++ ".") ]
                , button
                    [ onClick VelgNyPersonIDag ]
                    [ text (Teammedlem.navn standupVert ++ " kan ikke") ]
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


animationClasses : AnimationState -> AnimationElememt -> Css.Style
animationClasses animationState element =
    if elementSkalVises animationState element then
        Css.batch []

    else
        Css.opacity Css.zero


viewNesteVirkedag : AnimationState -> List Teammedlem -> Dato -> Html Msg
viewNesteVirkedag animationState nesteVirkedagsRekkefølge dagensDato =
    case List.head nesteVirkedagsRekkefølge of
        Just standupVert ->
            div
                [ Attributes.css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.alignItems Css.center
                    ]
                ]
                [ p [ Attributes.css [ animationClasses animationState FørsteSetning ] ]
                    [ text ("Den som skal holde standup på " ++ String.toLower (Dato.toUkedagString (Dato.nesteArbeidsdag dagensDato)))
                    ]
                , p [ Attributes.css [ animationClasses animationState AndreSetning ] ]
                    [ text "er" ]
                , h1 [ Attributes.css [ animationClasses animationState StandupVert ] ]
                    [ text (Teammedlem.navn standupVert) ]
                , button
                    [ Attributes.css [ animationClasses animationState NesteKnapp ]
                    , onClick VelgNyPersonNesteArbeidsdag
                    ]
                    [ text (Teammedlem.navn standupVert ++ " kan ikke") ]
                ]

        Nothing ->
            text "Da kunne visst ingen da..."



--- MAIN ---


init : DatabaseApiToken -> String -> () -> ( Model, Effect Msg )
init apiKey teamShortName =
    \_ ->
        ( Init
        , Effect.batch
            [ Time.now
                |> Task.perform TimeReceived
                |> Effect.sendCmd
            , Api.getTeam HentTeamResponse apiKey teamShortName
            ]
        )


page : Shared.Model -> Route { team : String } -> Page Model Msg
page shared route =
    Page.new
        { init = init shared.apiKey route.params.team
        , update = update
        , subscriptions = always Sub.none
        , view = view
        }
