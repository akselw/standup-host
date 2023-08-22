module Pages.Team_.Settings exposing (Model, Msg, page)

import AccessToken exposing (AccessToken)
import Api
import Auth
import Css
import DatabaseApiToken exposing (DatabaseApiToken)
import Effect exposing (Effect)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (value)
import Html.Styled.Events exposing (onClick, onInput)
import Http
import Layouts
import List.Extra
import Page exposing (Page)
import Route exposing (Route)
import Shared
import Shared.Model
import Team exposing (Team)
import Teammedlem exposing (Teammedlem)
import View exposing (View)


page : Auth.User -> Shared.Model -> Route { team : String } -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared.apiKey route.params.team user.accessToken
        , update = update shared.apiKey user.accessToken
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout toLayout


{-| Use the sidebar layout on this page
-}
toLayout : Model -> Layouts.Layout Msg
toLayout model =
    Layouts.Header {}



-- INIT


type Model
    = Loading
    | Failure Team.Error
    | NotTeamOwner Team
    | TeamOwner TeamOwnerModel


type alias TeamOwnerModel =
    { team : Team
    , medlemmer : List ( Teammedlem, TeammedlemState )
    , leggTilMedlemState : LeggTilMedlemState
    }


type TeammedlemState
    = InitialMedlemState
    | RedigererNavn String
    | LagrerNavneendring String
    | LagrerSletting


type LeggTilMedlemState
    = InitialLeggTilMedlemState
    | RedigererLeggTilMedlem String
    | LagrerLeggTilMedlem String


init : DatabaseApiToken -> String -> AccessToken -> () -> ( Model, Effect Msg )
init apiKey shortName accessToken () =
    ( Loading
    , Api.getTeam HentTeamResponse apiKey shortName
    )



-- UPDATE


type Msg
    = HentTeamResponse (Result Team.Error Team)
    | SuccessMsg SuccessMsg


type SuccessMsg
    = RedigerKnappTrykket Teammedlem
    | AvbrytRedigeringKnappTrykket Teammedlem
    | LagreRedigeringKnappTrykket Teammedlem
    | MedlemNavnOppdatert Teammedlem String
    | NavneendringResponse (Result Http.Error Teammedlem)
    | SlettKnappTrykket Teammedlem
    | SlettMedlemResponse Teammedlem (Result Http.Error ())
    | StartÅLeggeTilMedlemTrykket
    | LeggTilMedlemNavnOppdatert String
    | AvbrytLeggTilMedlemKnappTrykket
    | LagreLeggTilMedlemKnappTrykket
    | LeggTilMedlemResponse (Result Http.Error Teammedlem)


update : DatabaseApiToken -> AccessToken -> Msg -> Model -> ( Model, Effect Msg )
update apiKey accessToken msg model =
    case msg of
        HentTeamResponse (Ok team) ->
            ( if Team.hasOwner team (AccessToken.userId accessToken) then
                TeamOwner
                    { team = team
                    , medlemmer = initTeammedlemmer team
                    , leggTilMedlemState = InitialLeggTilMedlemState
                    }

              else
                NotTeamOwner team
            , Effect.none
            )

        HentTeamResponse (Err error) ->
            ( Failure error
            , Effect.none
            )

        SuccessMsg successMsg ->
            case model of
                TeamOwner teamOwnerModel ->
                    let
                        ( newModel, effect ) =
                            successUpdate apiKey accessToken successMsg teamOwnerModel
                    in
                    ( TeamOwner newModel, Effect.map SuccessMsg effect )

                _ ->
                    ( model
                    , Effect.none
                    )


initTeammedlemmer : Team -> List ( Teammedlem, TeammedlemState )
initTeammedlemmer team =
    team
        |> Team.medlemmer
        |> List.sortBy (Teammedlem.navn >> String.toLower)
        |> List.map (\medlem -> ( medlem, InitialMedlemState ))


successUpdate : DatabaseApiToken -> AccessToken -> SuccessMsg -> TeamOwnerModel -> ( TeamOwnerModel, Effect SuccessMsg )
successUpdate apiKey accessToken msg model =
    case msg of
        RedigerKnappTrykket teammedlem ->
            ( { model | medlemmer = updateMedlemState model.medlemmer teammedlem initRedigering }
            , Effect.none
            )

        MedlemNavnOppdatert teammedlem string ->
            ( { model | medlemmer = replaceMedlemState model.medlemmer teammedlem (RedigererNavn string) }
            , Effect.none
            )

        AvbrytRedigeringKnappTrykket teammedlem ->
            ( { model | medlemmer = replaceMedlemState model.medlemmer teammedlem InitialMedlemState }
            , Effect.none
            )

        LagreRedigeringKnappTrykket teammedlem ->
            case endretMedlemnavn model.medlemmer teammedlem of
                Just endretNavn ->
                    ( { model | medlemmer = replaceMedlemState model.medlemmer teammedlem (LagrerNavneendring endretNavn) }
                    , Api.updateTeammedlemNavn apiKey NavneendringResponse accessToken teammedlem endretNavn
                    )

                Nothing ->
                    ( model
                    , Effect.none
                    )

        NavneendringResponse (Ok teammedlem) ->
            ( { model | medlemmer = updateMedlem model.medlemmer teammedlem }
            , Effect.none
            )

        NavneendringResponse (Err err) ->
            ( model
            , Effect.none
            )

        SlettKnappTrykket teammedlem ->
            ( { model | medlemmer = replaceMedlemState model.medlemmer teammedlem LagrerSletting }
            , Api.slettTeammedlem apiKey (SlettMedlemResponse teammedlem) accessToken teammedlem
            )

        SlettMedlemResponse teammedlem (Ok _) ->
            ( { model | medlemmer = List.filter (\( medlem, _ ) -> Teammedlem.id teammedlem /= Teammedlem.id medlem) model.medlemmer }
            , Effect.none
            )

        SlettMedlemResponse teammedlem (Err _) ->
            ( model
            , Effect.none
            )

        StartÅLeggeTilMedlemTrykket ->
            ( { model | leggTilMedlemState = RedigererLeggTilMedlem "" }
            , Effect.none
              -- TODO: Fokus på inputfelt
            )

        LeggTilMedlemNavnOppdatert string ->
            ( { model | leggTilMedlemState = RedigererLeggTilMedlem string }
            , Effect.none
            )

        AvbrytLeggTilMedlemKnappTrykket ->
            ( { model | leggTilMedlemState = InitialLeggTilMedlemState }
            , Effect.none
            )

        LagreLeggTilMedlemKnappTrykket ->
            case model.leggTilMedlemState of
                RedigererLeggTilMedlem navn ->
                    ( { model | leggTilMedlemState = LagrerLeggTilMedlem navn }
                    , Api.leggTilTeammedlem apiKey LeggTilMedlemResponse accessToken model.team navn
                    )

                _ ->
                    ( model
                    , Effect.none
                    )

        LeggTilMedlemResponse (Ok teammedlem) ->
            ( { model
                | medlemmer = model.medlemmer ++ [ ( teammedlem, InitialMedlemState ) ]
                , leggTilMedlemState = RedigererLeggTilMedlem ""
              }
            , Effect.none
              -- TODO: Fokus på inputfelt
            )

        LeggTilMedlemResponse (Err err) ->
            ( { model
                | leggTilMedlemState =
                    case model.leggTilMedlemState of
                        LagrerLeggTilMedlem string ->
                            RedigererLeggTilMedlem string

                        _ ->
                            model.leggTilMedlemState
              }
            , Effect.none
            )


initRedigering : ( Teammedlem, TeammedlemState ) -> TeammedlemState
initRedigering ( medlem, _ ) =
    RedigererNavn (Teammedlem.navn medlem)


replaceMedlemState : List ( Teammedlem, TeammedlemState ) -> Teammedlem -> TeammedlemState -> List ( Teammedlem, TeammedlemState )
replaceMedlemState medlemmer medlemToUpdate teammedlemState =
    updateMedlemState medlemmer medlemToUpdate (always teammedlemState)


updateMedlemState : List ( Teammedlem, TeammedlemState ) -> Teammedlem -> (( Teammedlem, TeammedlemState ) -> TeammedlemState) -> List ( Teammedlem, TeammedlemState )
updateMedlemState medlemmer medlemToUpdate updateFunction =
    List.map
        (\( medlem, state ) ->
            if Teammedlem.id medlem == Teammedlem.id medlemToUpdate then
                ( medlem, updateFunction ( medlem, state ) )

            else
                ( medlem, state )
        )
        medlemmer


endretMedlemnavn : List ( Teammedlem, TeammedlemState ) -> Teammedlem -> Maybe String
endretMedlemnavn medlemmer teammedlem =
    let
        medlemOgState =
            medlemmer
                |> List.Extra.find (\( medlem, _ ) -> Teammedlem.id medlem == Teammedlem.id teammedlem)
                |> Maybe.map Tuple.second
    in
    case medlemOgState of
        Just (RedigererNavn endretNavn) ->
            Just endretNavn

        _ ->
            Nothing


updateMedlem : List ( Teammedlem, TeammedlemState ) -> Teammedlem -> List ( Teammedlem, TeammedlemState )
updateMedlem medlemmer medlemToUpdate =
    List.map
        (\( medlem, state ) ->
            if Teammedlem.id medlem == Teammedlem.id medlemToUpdate then
                ( medlemToUpdate, InitialMedlemState )

            else
                ( medlem, state )
        )
        medlemmer



-- VIEW


view : Model -> View Msg
view model =
    { title = ""
    , body =
        case model of
            Loading ->
                []

            Failure _ ->
                [ text "Det skjedde en feil ved lastingen av siden" ]

            NotTeamOwner _ ->
                [ text "Du er ikke eier av dette teamet." ]

            TeamOwner teamOwnerModel ->
                viewTeamOwner teamOwnerModel
                    |> List.map (Html.map SuccessMsg)
    }


viewTeamOwner : TeamOwnerModel -> List (Html SuccessMsg)
viewTeamOwner model =
    [ viewInnstillinger model
    , viewTeammedlemmer model.leggTilMedlemState model.medlemmer
    ]


viewInnstillinger : TeamOwnerModel -> Html SuccessMsg
viewInnstillinger model =
    text ""


viewTeammedlemmer : LeggTilMedlemState -> List ( Teammedlem, TeammedlemState ) -> Html SuccessMsg
viewTeammedlemmer leggTilMedlemState medlemmer =
    div
        [ Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "8px"
            ]
        ]
        (List.concat
            [ List.map viewTeammedlem medlemmer
            , [ viewLeggTilMedlem leggTilMedlemState ]
            ]
        )


viewTeammedlem : ( Teammedlem, TeammedlemState ) -> Html SuccessMsg
viewTeammedlem ( medlem, medlemState ) =
    div [ Attributes.css [ Css.padding (Css.px 16) ] ]
        [ case medlemState of
            InitialMedlemState ->
                div []
                    [ text (Teammedlem.navn medlem)
                    , button [ onClick (RedigerKnappTrykket medlem) ] [ text "Rediger" ]
                    , button [ onClick (SlettKnappTrykket medlem) ] [ text "Slett" ]
                    ]

            RedigererNavn string ->
                div []
                    [ label []
                        [ text ("Endre navn på \"" ++ Teammedlem.navn medlem ++ "\"")
                        , input [ value string, onInput (MedlemNavnOppdatert medlem) ] []
                        ]
                    , button [ onClick (AvbrytRedigeringKnappTrykket medlem) ] [ text "Avbryt" ]
                    , button [ onClick (LagreRedigeringKnappTrykket medlem) ] [ text "Lagre" ]
                    ]

            LagrerNavneendring string ->
                text "lagrer"

            LagrerSletting ->
                text "sletter"
        ]


viewLeggTilMedlem : LeggTilMedlemState -> Html SuccessMsg
viewLeggTilMedlem leggTilMedlemState =
    case leggTilMedlemState of
        InitialLeggTilMedlemState ->
            button [ onClick StartÅLeggeTilMedlemTrykket ] [ text "Legg til" ]

        RedigererLeggTilMedlem string ->
            div []
                [ label []
                    [ text "Navn"
                    , input [ value string, onInput LeggTilMedlemNavnOppdatert ] []
                    ]
                , button [ onClick AvbrytLeggTilMedlemKnappTrykket ] [ text "Avbryt" ]
                , button [ onClick LagreLeggTilMedlemKnappTrykket ] [ text "Legg til" ]
                ]

        LagrerLeggTilMedlem string ->
            text "lagrer"



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
