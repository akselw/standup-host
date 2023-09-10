module Pages.Team_.Settings exposing (Model, Msg, page)

import AccessToken exposing (AccessToken)
import Api
import Auth
import Browser.Dom
import Css
import DatabaseApiToken exposing (DatabaseApiToken)
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes exposing (value)
import Html.Styled.Events exposing (onSubmit)
import Http
import Layouts
import List.Extra
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import RouteExtras
import Shared
import Shared.Model
import ShortnameUniqueness exposing (ShortnameUniqueness, ShortnameUniquenessCheck)
import Task
import Team exposing (Team)
import TeamSettingsForm exposing (TeamSettingsForm, ValidatedTeamSettingsForm)
import TeamSummary exposing (TeamSummary)
import Teammedlem exposing (Teammedlem)
import View exposing (View)
import View.Button as Button
import View.TextInput as TextInput


page : Auth.User -> Shared.Model -> Route { team : String } -> Page Model Msg
page user shared route =
    Page.new
        { init = init shared.apiKey route.params.team user.accessToken
        , update = update shared.apiKey user.accessToken
        , subscriptions = subscriptions
        , view = view
        }
        |> Page.withLayout toLayout


{-| Use the header layout on this page
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
    , formState : FormState
    , medlemmer : List ( Teammedlem, TeammedlemState )
    , leggTilMedlemState : LeggTilMedlemState
    }


type FormState
    = NoForm
    | Editing ShortnameUniqueness TeamSettingsForm
    | SavingForm ValidatedTeamSettingsForm
    | SavingFormFailure ValidatedTeamSettingsForm Http.Error


type TeammedlemState
    = InitialMedlemState
    | LagrerSletting
    | RedigererNavn String
    | LagrerNavneendring String


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
    = EndreInnstilingerKnappTrykket
    | NavnOppdatert String
    | NavnMistetFokus
    | ShortnameOppdatert String
    | ShortnameMistetFokus
    | ShortnameUniquenessResponse String (Result Http.Error ShortnameUniquenessCheck)
    | LagreSkjemaEndringerTrykket
    | AvbrytSkjemaendringTrykket
    | UpdateTeamSummaryResponse (Result Http.Error TeamSummary)
    | RedigerKnappTrykket Teammedlem
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
    | FocusedOnElement (Result Browser.Dom.Error ())


update : DatabaseApiToken -> AccessToken -> Msg -> Model -> ( Model, Effect Msg )
update apiKey accessToken msg model =
    case msg of
        HentTeamResponse (Ok team) ->
            ( if Team.hasOwner team (AccessToken.userId accessToken) then
                TeamOwner
                    { team = team
                    , formState = NoForm
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
        EndreInnstilingerKnappTrykket ->
            ( { model
                | formState =
                    model.team
                        |> TeamSettingsForm.init
                        |> Editing ShortnameUniqueness.init
              }
            , focusOnInput TeamnavnInput
            )

        NavnOppdatert string ->
            case model.formState of
                Editing shortnameUniqueness form ->
                    ( { model
                        | formState =
                            form
                                |> TeamSettingsForm.oppdaterNavn string
                                |> Editing shortnameUniqueness
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        NavnMistetFokus ->
            case model.formState of
                Editing shortnameUniqueness form ->
                    ( { model
                        | formState =
                            form
                                |> TeamSettingsForm.visFeilmeldingNavn
                                |> Editing shortnameUniqueness
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        ShortnameOppdatert string ->
            case model.formState of
                Editing shortnameUniqueness form ->
                    ( { model
                        | formState =
                            form
                                |> TeamSettingsForm.oppdaterShortname string
                                |> Editing shortnameUniqueness
                      }
                    , Api.checkShortnameUniqueness apiKey (ShortnameUniquenessResponse string) string
                    )

                _ ->
                    ( model, Effect.none )

        ShortnameMistetFokus ->
            case model.formState of
                Editing shortnameUniqueness form ->
                    ( { model
                        | formState =
                            form
                                |> TeamSettingsForm.visFeilmeldingShortname
                                |> Editing shortnameUniqueness
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        ShortnameUniquenessResponse shortname result ->
            case model.formState of
                Editing shortnameUniqueness form ->
                    ( { model | formState = Editing (ShortnameUniqueness.insert shortname result shortnameUniqueness) form }, Effect.none )

                _ ->
                    ( model, Effect.none )

        LagreSkjemaEndringerTrykket ->
            case model.formState of
                Editing shortnameUniqueness form ->
                    case TeamSettingsForm.validate form of
                        Just validated ->
                            ( { model | formState = SavingForm validated }
                            , Api.updateTeam apiKey UpdateTeamSummaryResponse accessToken validated
                            )

                        Nothing ->
                            ( { model
                                | formState =
                                    form
                                        |> TeamSettingsForm.visAlleFeilmeldinger
                                        |> Editing shortnameUniqueness
                              }
                            , Effect.none
                            )

                _ ->
                    ( model, Effect.none )

        AvbrytSkjemaendringTrykket ->
            ( { model | formState = NoForm }, Effect.none )

        UpdateTeamSummaryResponse res ->
            case model.formState of
                SavingForm form ->
                    case res of
                        Ok teamSummary ->
                            ( { model
                                | team = Team.updateTeamSummary model.team teamSummary
                                , formState = NoForm
                              }
                            , Effect.replaceRoute
                                { path = Route.Path.Team__Settings { team = TeamSummary.shortname teamSummary }
                                , query = Dict.empty
                                , hash = Nothing
                                }
                            )

                        Err error ->
                            ( { model | formState = SavingFormFailure form error }, Effect.none )

                _ ->
                    ( model, Effect.none )

        RedigerKnappTrykket teammedlem ->
            ( { model
                | medlemmer =
                    model.medlemmer
                        |> lukkAlleMedlemRedigeringer
                        |> updateMedlemState teammedlem initRedigering
                , leggTilMedlemState = InitialLeggTilMedlemState
              }
            , focusOnInput MedlemnavnInput
            )

        MedlemNavnOppdatert teammedlem string ->
            ( { model | medlemmer = replaceMedlemState teammedlem (RedigererNavn string) model.medlemmer }
            , Effect.none
            )

        AvbrytRedigeringKnappTrykket teammedlem ->
            ( { model | medlemmer = replaceMedlemState teammedlem InitialMedlemState model.medlemmer }
            , Effect.none
            )

        LagreRedigeringKnappTrykket teammedlem ->
            case endretMedlemnavn model.medlemmer teammedlem of
                Just endretNavn ->
                    ( { model | medlemmer = replaceMedlemState teammedlem (LagrerNavneendring endretNavn) model.medlemmer }
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
            ( { model | medlemmer = replaceMedlemState teammedlem LagrerSletting model.medlemmer }
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
            ( { model
                | leggTilMedlemState = RedigererLeggTilMedlem ""
                , medlemmer = lukkAlleMedlemRedigeringer model.medlemmer
              }
            , focusOnInput LeggTilMedlemInput
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
            , focusOnInput LeggTilMedlemInput
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

        FocusedOnElement _ ->
            ( model, Effect.none )


initRedigering : ( Teammedlem, TeammedlemState ) -> TeammedlemState
initRedigering ( medlem, _ ) =
    RedigererNavn (Teammedlem.navn medlem)


replaceMedlemState : Teammedlem -> TeammedlemState -> List ( Teammedlem, TeammedlemState ) -> List ( Teammedlem, TeammedlemState )
replaceMedlemState medlemToUpdate teammedlemState medlemmer =
    updateMedlemState medlemToUpdate (always teammedlemState) medlemmer


updateMedlemState : Teammedlem -> (( Teammedlem, TeammedlemState ) -> TeammedlemState) -> List ( Teammedlem, TeammedlemState ) -> List ( Teammedlem, TeammedlemState )
updateMedlemState medlemToUpdate updateFunction medlemmer =
    List.map
        (\( medlem, state ) ->
            if Teammedlem.id medlem == Teammedlem.id medlemToUpdate then
                ( medlem, updateFunction ( medlem, state ) )

            else
                ( medlem, state )
        )
        medlemmer


lukkAlleMedlemRedigeringer : List ( Teammedlem, TeammedlemState ) -> List ( Teammedlem, TeammedlemState )
lukkAlleMedlemRedigeringer medlemmer =
    medlemmer
        |> List.map
            (\( medlem, _ ) ->
                ( medlem, InitialMedlemState )
            )


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



--- InputId ---


type InputId
    = TeamnavnInput
    | MedlemnavnInput
    | LeggTilMedlemInput


inputIdToString : InputId -> String
inputIdToString inputId_ =
    case inputId_ of
        TeamnavnInput ->
            "teamnavn_input"

        MedlemnavnInput ->
            "medlemnavn_input"

        LeggTilMedlemInput ->
            "legg_til_medlem_input"


focusOnInput : InputId -> Effect SuccessMsg
focusOnInput inputId =
    inputId
        |> inputIdToString
        |> Browser.Dom.focus
        |> Task.attempt FocusedOnElement
        |> Effect.sendCmd



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
    [ div [ Attributes.css [ Css.maxWidth (Css.px 632), Css.margin Css.auto, Css.padding2 Css.zero (Css.px 16) ] ]
        [ h1 [] [ text (Team.navn model.team) ]
        , viewInnstillingerSection model
        , viewTeammedlemmer model.leggTilMedlemState model.medlemmer
        ]
    ]


viewInnstillingerSection : TeamOwnerModel -> Html SuccessMsg
viewInnstillingerSection model =
    div []
        [ h2 [] [ text "Innstillinger" ]
        , case model.formState of
            NoForm ->
                viewInnstillinger model.team

            Editing shortnameUniqueness form ->
                viewForm { isLoading = False, shortnameIcon = shortnameStatusIcon model.team form shortnameUniqueness } form

            SavingForm validatedForm ->
                validatedForm
                    |> TeamSettingsForm.fromValidated
                    |> viewForm { isLoading = True, shortnameIcon = Nothing }

            SavingFormFailure validatedForm error ->
                viewFormFailure validatedForm error
        ]


shortnameStatusIcon : Team -> TeamSettingsForm -> ShortnameUniqueness -> Maybe TextInput.StatusIcon
shortnameStatusIcon team form shortnameUniqueness =
    if Team.shortname team == TeamSettingsForm.shortname form then
        Just TextInput.Checkmark

    else if ShortnameUniqueness.isLoading shortnameUniqueness (TeamSettingsForm.shortname form) then
        Just TextInput.LoadingSpinner

    else if ShortnameUniqueness.isUnique shortnameUniqueness (TeamSettingsForm.shortname form) then
        Just TextInput.Checkmark

    else
        Just TextInput.Error


viewInnstillinger : Team -> Html SuccessMsg
viewInnstillinger team =
    let
        teamPath =
            Route.Path.Team_ { team = Team.shortname team }

        url =
            "https://hvemharstandup.no" ++ Route.Path.toString teamPath
    in
    div
        [ Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "16px"
            , Css.padding2 (Css.px 16) (Css.px 24)
            , Css.border3 (Css.px 1) Css.solid borderColor
            , Css.borderRadius (Css.px 6)
            , Css.marginBottom (Css.px 40)
            ]
        ]
        [ viewIndividualSetting { label = "Navn", value = text (Team.navn team) }
        , viewIndividualSetting { label = "Shortname", value = text (Team.shortname team) }
        , viewIndividualSetting { label = "URL", value = a [ RouteExtras.href teamPath ] [ text url ] }
        , Button.button EndreInnstilingerKnappTrykket "Endre"
            |> Button.withVariant Button.Secondary
            |> Button.withCss [ Css.alignSelf Css.end ]
            |> Button.toHtml
        ]


viewIndividualSetting : { label : String, value : Html msg } -> Html msg
viewIndividualSetting { label, value } =
    div
        [ Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "2px"
            ]
        ]
        [ span [ Attributes.css [ Css.fontWeight (Css.int 700) ] ] [ text label ]
        , span [] [ value ]
        ]


viewForm : { isLoading : Bool, shortnameIcon : Maybe TextInput.StatusIcon } -> TeamSettingsForm -> Html SuccessMsg
viewForm { isLoading, shortnameIcon } form =
    Html.form
        [ onSubmit LagreSkjemaEndringerTrykket
        , Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "16px"
            , Css.padding2 (Css.px 16) (Css.px 24)
            , Css.border3 (Css.px 1) Css.solid borderColor
            , Css.borderRadius (Css.px 6)
            , Css.marginBottom (Css.px 40)
            ]
        ]
        [ form
            |> TeamSettingsForm.navn
            |> TextInput.input { label = "Navn", msg = NavnOppdatert }
            |> TextInput.withId (inputIdToString TeamnavnInput)
            |> TextInput.withDisabled isLoading
            |> TextInput.withErrorMessage (TeamSettingsForm.feilmeldingNavn form)
            |> TextInput.onBlur NavnMistetFokus
            |> TextInput.toHtml
        , form
            |> TeamSettingsForm.shortname
            |> TextInput.input { label = "Shortname", msg = ShortnameOppdatert }
            |> TextInput.withStatusIcon shortnameIcon
            |> TextInput.withDisabled isLoading
            |> TextInput.withErrorMessage (TeamSettingsForm.feilmeldingShortname form)
            |> TextInput.onBlur ShortnameMistetFokus
            |> TextInput.toHtml
        , viewIndividualSetting { label = "URL", value = text ("https://hvemharstandup.no/" ++ TeamSettingsForm.shortname form) }
        , div [ Attributes.css [ Css.alignSelf Css.end, Css.displayFlex, Css.flexDirection Css.row, Css.property "gap" "12px" ] ]
            [ Button.button AvbrytSkjemaendringTrykket "Avbryt"
                |> Button.withVariant Button.Secondary
                |> Button.withDisabled isLoading
                |> Button.toHtml
            , Button.submit "Lagre"
                |> Button.withLoadingSpinner isLoading
                |> Button.toHtml
            ]
        ]


viewSavingForm : ValidatedTeamSettingsForm -> Html msg
viewSavingForm validatedForm =
    text "viewSavingForm"


viewFormFailure : ValidatedTeamSettingsForm -> Http.Error -> Html SuccessMsg
viewFormFailure validatedForm error =
    text "viewFormFailure"


borderColor : Css.Color
borderColor =
    Css.hex "979FAF"


viewTeammedlemmer : LeggTilMedlemState -> List ( Teammedlem, TeammedlemState ) -> Html SuccessMsg
viewTeammedlemmer leggTilMedlemState medlemmer =
    div []
        [ h2 [ Attributes.css [ Css.marginTop Css.zero, Css.marginBottom (Css.px 16) ] ] [ text "Teammedlemmer" ]
        , div
            [ Attributes.css
                [ Css.displayFlex
                , Css.flexDirection Css.column
                , Css.border3 (Css.px 1) Css.solid borderColor
                , Css.borderRadius (Css.px 6)
                , Css.marginBottom (Css.px 16)
                ]
            ]
            (List.concat
                [ List.map viewTeammedlem medlemmer
                , [ viewLeggTilMedlemInput leggTilMedlemState ]
                ]
            )
        , viewLeggTilMedlemKnapp leggTilMedlemState
        ]


viewTeammedlem : ( Teammedlem, TeammedlemState ) -> Html SuccessMsg
viewTeammedlem ( medlem, medlemState ) =
    viewTeammedlemListeElement
        (case medlemState of
            InitialMedlemState ->
                [ viewTeammedlemUtenForm { isLoading = False } medlem ]

            LagrerSletting ->
                [ viewTeammedlemUtenForm { isLoading = True } medlem ]

            RedigererNavn string ->
                [ viewRedigerTeammedlemNavn { isLoading = False } medlem string ]

            LagrerNavneendring string ->
                [ viewRedigerTeammedlemNavn { isLoading = True } medlem string ]
        )


viewTeammedlemUtenForm : { isLoading : Bool } -> Teammedlem -> Html SuccessMsg
viewTeammedlemUtenForm { isLoading } medlem =
    div [ Attributes.css teammedlemListeElementLayout ]
        [ span [ Attributes.css [ Css.flex (Css.num 1) ] ] [ text (Teammedlem.navn medlem) ]
        , Button.button (RedigerKnappTrykket medlem) "Rediger"
            |> Button.withVariant Button.Secondary
            |> Button.withDisabled isLoading
            |> Button.toHtml
        , Button.button (SlettKnappTrykket medlem) "Slett"
            |> Button.withVariant Button.Secondary
            |> Button.withLoadingSpinner isLoading
            |> Button.toHtml
        ]


viewRedigerTeammedlemNavn : { isLoading : Bool } -> Teammedlem -> String -> Html SuccessMsg
viewRedigerTeammedlemNavn { isLoading } medlem string =
    form
        [ onSubmit (LagreRedigeringKnappTrykket medlem)
        , Attributes.css teammedlemListeElementLayout
        ]
        [ TextInput.input { msg = MedlemNavnOppdatert medlem, label = "Endre navn på \"" ++ Teammedlem.navn medlem ++ "\"" } string
            |> TextInput.withId (inputIdToString MedlemnavnInput)
            |> TextInput.withDisabled isLoading
            |> TextInput.withCss [ Css.flex (Css.num 1) ]
            |> TextInput.toHtml
        , div [ Attributes.css [ Css.alignSelf Css.end, Css.displayFlex, Css.justifyContent Css.spaceBetween, Css.property "gap" "12px" ] ]
            [ Button.button (AvbrytRedigeringKnappTrykket medlem) "Avbryt"
                |> Button.withVariant Button.Secondary
                |> Button.withDisabled isLoading
                |> Button.toHtml
            , Button.submit "Lagre"
                |> Button.withLoadingSpinner isLoading
                |> Button.toHtml
            ]
        ]


viewTeammedlemListeElement : List (Html msg) -> Html msg
viewTeammedlemListeElement children =
    div
        [ Attributes.css
            [ Css.padding2 (Css.px 16) (Css.px 24)
            , Css.borderTop3 (Css.px 1) Css.solid borderColor
            , Css.firstOfType [ Css.borderWidth Css.zero ]
            ]
        ]
        children


teammedlemListeElementLayout : List Css.Style
teammedlemListeElementLayout =
    [ Css.displayFlex
    , Css.flexDirection Css.row
    , Css.flexWrap Css.wrap
    , Css.alignItems Css.center
    , Css.property "gap" "12px"
    , Css.width (Css.pct 100)
    , Css.justifyContent Css.end
    ]


viewLeggTilMedlemInput : LeggTilMedlemState -> Html SuccessMsg
viewLeggTilMedlemInput leggTilMedlemState =
    case leggTilMedlemState of
        InitialLeggTilMedlemState ->
            text ""

        RedigererLeggTilMedlem string ->
            viewTeammedlemListeElement
                [ viewLeggTilTeammedlem { isLoading = False } string ]

        LagrerLeggTilMedlem string ->
            viewTeammedlemListeElement
                [ viewLeggTilTeammedlem { isLoading = True } string ]


viewLeggTilTeammedlem : { isLoading : Bool } -> String -> Html SuccessMsg
viewLeggTilTeammedlem { isLoading } string =
    form
        [ onSubmit LagreLeggTilMedlemKnappTrykket
        , Attributes.css teammedlemListeElementLayout
        ]
        [ TextInput.input { msg = LeggTilMedlemNavnOppdatert, label = "Navn" } string
            |> TextInput.withId (inputIdToString LeggTilMedlemInput)
            |> TextInput.withDisabled isLoading
            |> TextInput.withCss [ Css.flex (Css.num 1) ]
            |> TextInput.toHtml
        , div [ Attributes.css [ Css.alignSelf Css.end, Css.displayFlex, Css.justifyContent Css.spaceBetween, Css.property "gap" "12px" ] ]
            [ Button.button AvbrytLeggTilMedlemKnappTrykket "Avbryt"
                |> Button.withVariant Button.Secondary
                |> Button.withDisabled isLoading
                |> Button.toHtml
            , Button.submit "Legg til"
                |> Button.withVariant Button.Tertiary
                |> Button.withLoadingSpinner isLoading
                |> Button.toHtml
            ]
        ]


viewLeggTilMedlemKnapp : LeggTilMedlemState -> Html SuccessMsg
viewLeggTilMedlemKnapp leggTilMedlemState =
    case leggTilMedlemState of
        InitialLeggTilMedlemState ->
            Button.button StartÅLeggeTilMedlemTrykket "Legg til"
                |> Button.withSize Button.Large
                |> Button.toHtml

        _ ->
            text ""



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
