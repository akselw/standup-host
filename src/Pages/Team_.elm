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
        , valgtDag : ValgtDag
        , viewState : ViewState
        , team : Team
        }


type ValgtDag
    = Idag
    | NesteArbeidsdag


type ViewState
    = StandupHost Teammedlem AnimasjonState
    | IngenStandupHost


type AnimasjonState
    = IngenAnimasjon (List Teammedlem)
    | BytterTeammedlem { til : Teammedlem } (List Teammedlem)



--- UPDATE ---


type Msg
    = TimeReceived Posix
    | VelgNyPerson
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
                            ( Success
                                { dagensDato = dato
                                , valgtDag = Idag
                                , viewState = initViewState team dato
                                , team = team
                                }
                            , Effect.none
                            )

                        Err error ->
                            ( Failure error, Effect.none )

                _ ->
                    ( model, Effect.none )

        VelgNyPerson ->
            case model of
                Success modelInfo ->
                    ( Success { modelInfo | viewState = nesteState modelInfo.viewState }
                    , Process.sleep teammedlemBytteAnimasjonstid
                        |> Task.perform (always AnimasjonFerdig)
                        |> Effect.sendCmd
                    )

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
            StandupHost first (IngenAnimasjon rest)

        [] ->
            IngenStandupHost


nesteState : ViewState -> ViewState
nesteState viewState =
    case viewState of
        StandupHost teammedlem (IngenAnimasjon teammedlemmer) ->
            case teammedlemmer of
                first :: rest ->
                    StandupHost teammedlem (BytterTeammedlem { til = first } rest)

                [] ->
                    IngenStandupHost

        StandupHost _ (BytterTeammedlem { til } teammedlemmer) ->
            StandupHost til (IngenAnimasjon teammedlemmer)

        IngenStandupHost ->
            IngenStandupHost



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
        Success { dagensDato, valgtDag, viewState } ->
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
                    , viewDag viewState
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
                    , viewDag viewState
                    ]

        _ ->
            []


teammedlemBytteAnimasjonstid : Float
teammedlemBytteAnimasjonstid =
    200


viewDag : ViewState -> Html Msg
viewDag viewState =
    case viewState of
        StandupHost standupVert animasjonState ->
            div
                [ Attributes.css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.alignItems Css.center
                    ]
                ]
                [ p [] [ text "Den som skal holde standup er" ]
                , div [ Attributes.css [ Css.height (Css.px 75), Css.overflow Css.hidden, Css.textAlign Css.center ] ]
                    (case animasjonState of
                        IngenAnimasjon _ ->
                            [ h1 [] [ text (Teammedlem.navn standupVert) ]
                            , h1 [] [ text (Teammedlem.navn standupVert) ]
                            ]

                        BytterTeammedlem { til } _ ->
                            [ h1
                                [ Attributes.css
                                    [ Css.transform (Css.translateY (Css.px -56.7))
                                    , Css.Transitions.transition [ Css.Transitions.transform teammedlemBytteAnimasjonstid ]
                                    ]
                                ]
                                [ text (Teammedlem.navn standupVert) ]
                            , h1
                                [ Attributes.css
                                    [ Css.transform (Css.translateY (Css.px -56.7))
                                    , Css.Transitions.transition [ Css.Transitions.transform teammedlemBytteAnimasjonstid ]
                                    ]
                                ]
                                [ text (Teammedlem.navn til) ]
                            ]
                    )
                , Button.button VelgNyPerson (Teammedlem.navn standupVert ++ " kan ikke")
                    |> Button.toHtml
                ]

        IngenStandupHost ->
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
