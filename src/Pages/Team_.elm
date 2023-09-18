module Pages.Team_ exposing (Model, Msg, page)

import Api
import Css
import Css.Transitions
import DatabaseApiToken exposing (DatabaseApiToken)
import Dato exposing (Dato)
import Effect exposing (Effect)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes
import Layouts
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
import View.Button as Button



--- MODEL ---


type Model
    = Init
    | LoadingTeam Dato
    | Failure Team.Error
    | Success
        { dagensDato : Dato
        , dagensRekkefølge : List Teammedlem
        , morgensdagensRekkefølge : List Teammedlem
        , viewState : ViewState
        , valgtDag : ValgtDag
        , team : Team
        }


type ViewState
    = IngenAnimasjon Teammedlem (List Teammedlem)
    | BytterTeammedlem { fra : Teammedlem, til : Teammedlem } (List Teammedlem)
    | IngenKunne


type ValgtDag
    = Idag
    | NesteArbeidsdag



--- UPDATE ---


type Msg
    = TimeReceived Posix
    | VelgNyPersonIDag
    | VelgNyPersonNesteArbeidsdag
    | AnimasjonFerdig
    | EndreFane ValgtDag
    | HentTeamResponse (Result Team.Error Team)


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
                            let
                                dagensRekkefølge =
                                    dato
                                        |> Dato.toSeed
                                        |> Random.step (Random.List.shuffle (Team.medlemmer team))
                                        |> Tuple.first
                            in
                            ( Success
                                { dagensDato = dato
                                , dagensRekkefølge = dagensRekkefølge
                                , morgensdagensRekkefølge =
                                    dato
                                        |> Dato.nesteArbeidsdag
                                        |> Dato.toSeed
                                        |> Random.step (Random.List.shuffle (Team.medlemmer team))
                                        |> Tuple.first
                                , viewState = initViewState team dato
                                , valgtDag = Idag
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
                            ( Success
                                { record
                                    | dagensRekkefølge = rest
                                    , viewState = nesteState record.viewState
                                }
                            , Process.sleep teammedlemBytteAnimasjonstid
                                |> Task.perform (always AnimasjonFerdig)
                                |> Effect.sendCmd
                            )

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

        AnimasjonFerdig ->
            case model of
                Success record ->
                    ( Success { record | viewState = nesteState record.viewState }, Effect.none )

                _ ->
                    ( model, Effect.none )

        EndreFane valgtDag ->
            case model of
                Success modelInfo ->
                    ( Success
                        { modelInfo
                            | valgtDag = valgtDag
                            , viewState =
                                case valgtDag of
                                    Idag ->
                                        initViewState modelInfo.team modelInfo.dagensDato

                                    NesteArbeidsdag ->
                                        modelInfo.dagensDato
                                            |> Dato.nesteArbeidsdag
                                            |> initViewState modelInfo.team
                        }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )


initViewState : Team -> Dato -> ViewState
initViewState team dato =
    let
        rekkefølge =
            dato
                |> Dato.toSeed
                |> Random.step (Random.List.shuffle (Team.medlemmer team))
                |> Tuple.first
    in
    case rekkefølge of
        first :: rest ->
            IngenAnimasjon first rest

        [] ->
            IngenKunne


nesteState : ViewState -> ViewState
nesteState viewState =
    case viewState of
        IngenAnimasjon teammedlem teammedlemmer ->
            case teammedlemmer of
                first :: rest ->
                    BytterTeammedlem { fra = teammedlem, til = first } rest

                [] ->
                    IngenKunne

        BytterTeammedlem { til } teammedlemmer ->
            IngenAnimasjon til teammedlemmer

        IngenKunne ->
            IngenKunne



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


viewDatoRadButton : String -> ( Msg, String ) -> Html Msg
viewDatoRadButton gridArea ( msg, label ) =
    span [ Attributes.css [ Css.property "grid-area" gridArea ] ]
        [ Button.button msg label
            |> Button.withVariant Button.Secondary
            |> Button.toHtml
        ]


viewDatoRad : { leftButton : Maybe ( Msg, String ), rowText : String, rightButton : Maybe ( Msg, String ) } -> Html Msg
viewDatoRad { leftButton, rowText, rightButton } =
    div
        [ Attributes.css
            [ Css.property "display" "grid"
            , Css.property "grid-template-columns" "1fr auto 1fr"
            , Css.property "grid-template-areas" "\"left-button middle right-button\""
            , Css.property "gap" "24px"
            ]
        ]
        [ leftButton
            |> Maybe.map (viewDatoRadButton "left-button")
            |> Maybe.withDefault (text "")
        , span
            [ Attributes.css
                [ Css.property "grid-area" "middle"
                , Css.alignSelf Css.center
                ]
            ]
            [ text rowText ]
        , rightButton
            |> Maybe.map (viewDatoRadButton "right-button")
            |> Maybe.withDefault (text "")
        ]


viewContent : Model -> List (Html Msg)
viewContent model =
    case model of
        Success { dagensDato, dagensRekkefølge, morgensdagensRekkefølge, valgtDag, viewState } ->
            case valgtDag of
                Idag ->
                    [ viewDatoRad
                        { leftButton = Nothing
                        , rowText = Dato.toUkedagString dagensDato ++ " " ++ Dato.toString dagensDato
                        , rightButton =
                            Just
                                ( EndreFane NesteArbeidsdag
                                , dagensDato
                                    |> Dato.nesteArbeidsdag
                                    |> Dato.toUkedagString
                                )
                        }
                    , viewIdag viewState
                    ]

                NesteArbeidsdag ->
                    let
                        nesteVirkedag =
                            Dato.nesteArbeidsdag dagensDato
                    in
                    [ viewDatoRad
                        { leftButton = Just ( EndreFane Idag, "I dag" )
                        , rowText = Dato.toUkedagString nesteVirkedag ++ " " ++ Dato.toString nesteVirkedag
                        , rightButton = Nothing
                        }
                    , viewIdag viewState
                    ]

        _ ->
            []


teammedlemBytteAnimasjonstid : Float
teammedlemBytteAnimasjonstid =
    200


viewIdag : ViewState -> Html Msg
viewIdag viewState =
    case viewState of
        IngenAnimasjon standupVert _ ->
            div
                [ Attributes.css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.alignItems Css.center
                    ]
                ]
                [ p [] [ text "Den som skal holde standup er" ]
                , div [ Attributes.css [ Css.height (Css.px 75), Css.overflow Css.hidden, Css.textAlign Css.center ] ]
                    [ h1 [] [ text (Teammedlem.navn standupVert) ]
                    , h1 [] [ text (Teammedlem.navn standupVert) ]
                    ]
                , Button.button VelgNyPersonIDag (Teammedlem.navn standupVert ++ " kan ikke")
                    |> Button.toHtml
                ]

        BytterTeammedlem { fra, til } _ ->
            div
                [ Attributes.css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.alignItems Css.center
                    ]
                ]
                [ p [] [ text "Den som skal holde standup er" ]
                , div [ Attributes.css [ Css.height (Css.px 75), Css.overflow Css.hidden, Css.textAlign Css.center ] ]
                    [ h1 [ Attributes.css [ Css.transform (Css.translateY (Css.px -56.7)), Css.Transitions.transition [ Css.Transitions.transform teammedlemBytteAnimasjonstid ] ] ] [ text (Teammedlem.navn fra) ]
                    , h1 [ Attributes.css [ Css.transform (Css.translateY (Css.px -56.7)), Css.Transitions.transition [ Css.Transitions.transform teammedlemBytteAnimasjonstid ] ] ] [ text (Teammedlem.navn til) ]
                    ]
                , Button.button VelgNyPersonIDag (Teammedlem.navn fra ++ " kan ikke")
                    |> Button.toHtml
                ]

        IngenKunne ->
            text "Da kunne visst ingen da..."


viewNesteVirkedag : List Teammedlem -> Html Msg
viewNesteVirkedag nesteVirkedagsRekkefølge =
    case List.head nesteVirkedagsRekkefølge of
        Just standupVert ->
            div
                [ Attributes.css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.alignItems Css.center
                    ]
                ]
                [ p []
                    [ text "Den som skal holde standup er" ]
                , h1 []
                    [ text (Teammedlem.navn standupVert) ]
                , Button.button VelgNyPersonNesteArbeidsdag (Teammedlem.navn standupVert ++ " kan ikke")
                    |> Button.toHtml
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
        |> Page.withLayout toLayout


{-| Use the header layout on this page
-}
toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.Header {}
