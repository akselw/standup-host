module Pages.Team_ exposing (Model, Msg, page)

import Api
import Css
import DatabaseApiToken exposing (DatabaseApiToken)
import Dato exposing (Dato)
import Effect exposing (Effect)
import Html.Styled exposing (..)
import Html.Styled.Attributes as Attributes exposing (type_)
import Html.Styled.Events exposing (onClick)
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
                    ( Success { modelInfo | valgtDag = valgtDag }, Effect.none )

                _ ->
                    ( model, Effect.none )



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
        Success { dagensDato, dagensRekkefølge, morgensdagensRekkefølge, valgtDag } ->
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
                    , viewIdag dagensRekkefølge
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
                    , viewNesteVirkedag morgensdagensRekkefølge
                    ]

        _ ->
            []


viewIdag : List Teammedlem -> Html Msg
viewIdag rekkefølge =
    case List.head rekkefølge of
        Just standupVert ->
            div
                [ Attributes.css
                    [ Css.displayFlex
                    , Css.flexDirection Css.column
                    , Css.alignItems Css.center
                    ]
                ]
                [ p [] [ text "Den som skal holde standup er" ]
                , h1 [] [ text (Teammedlem.navn standupVert) ]
                , Button.button VelgNyPersonIDag (Teammedlem.navn standupVert ++ " kan ikke")
                    |> Button.toHtml
                ]

        Nothing ->
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
                , button [ onClick VelgNyPersonNesteArbeidsdag ]
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
        |> Page.withLayout toLayout


{-| Use the header layout on this page
-}
toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.Header {}
