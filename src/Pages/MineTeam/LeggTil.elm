module Pages.MineTeam.LeggTil exposing (Model, Msg, page)

import AccessToken exposing (AccessToken)
import Api
import Auth
import Css
import DatabaseApiToken exposing (DatabaseApiToken)
import Dict
import Effect exposing (Effect)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onSubmit)
import Http
import Layouts
import LeggTilTeamForm exposing (LeggTilTeamForm, ValidatedLeggTilTeamForm)
import Page exposing (Page)
import Route exposing (Route)
import Route.Path
import Shared
import SlugUniqueness exposing (SlugUniqueness, SlugUniquenessCheck)
import TeamSummary exposing (TeamSummary)
import View exposing (View)
import View.Button as Button
import View.Page as Page
import View.TextInput as TextInput


page : Auth.User -> Shared.Model -> Route () -> Page Model Msg
page user shared _ =
    Page.new
        { init = init
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


type alias Model =
    { formState : FormState
    , slugUniqueness : SlugUniqueness
    }


type FormState
    = Editing LeggTilTeamForm
    | Saving ValidatedLeggTilTeamForm
    | Failure Http.Error ValidatedLeggTilTeamForm


init : () -> ( Model, Effect Msg )
init () =
    ( { formState = Editing LeggTilTeamForm.init
      , slugUniqueness = SlugUniqueness.init
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = NavnOppdatert String
    | NavnMistetFokus
    | SlugOppdatert String
    | SlugMistetFokus
    | SlugUniquenessResponse String (Result Http.Error SlugUniquenessCheck)
    | LagreTrykket
    | AvbrytTrykket
    | CreateTeamResponse (Result Http.Error TeamSummary)


update : DatabaseApiToken -> AccessToken -> Msg -> Model -> ( Model, Effect Msg )
update apiKey accessToken msg model =
    case msg of
        NavnOppdatert string ->
            case model.formState of
                Editing form ->
                    ( { model
                        | formState =
                            form
                                |> LeggTilTeamForm.oppdaterNavn string
                                |> Editing
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        NavnMistetFokus ->
            case model.formState of
                Editing form ->
                    ( { model
                        | formState =
                            form
                                |> LeggTilTeamForm.visFeilmeldingNavn
                                |> Editing
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        SlugOppdatert string ->
            case model.formState of
                Editing form ->
                    ( { model
                        | formState =
                            form
                                |> LeggTilTeamForm.oppdaterSlug string
                                |> Editing
                      }
                    , Api.checkSlugUniqueness apiKey (SlugUniquenessResponse string) string
                    )

                _ ->
                    ( model, Effect.none )

        SlugMistetFokus ->
            case model.formState of
                Editing form ->
                    ( { model
                        | formState =
                            form
                                |> LeggTilTeamForm.visFeilmeldingSlug
                                |> Editing
                      }
                    , Effect.none
                    )

                _ ->
                    ( model, Effect.none )

        SlugUniquenessResponse slug result ->
            ( { model | slugUniqueness = SlugUniqueness.insert slug result model.slugUniqueness }
            , Effect.none
            )

        LagreTrykket ->
            case model.formState of
                Editing form ->
                    case LeggTilTeamForm.validate model.slugUniqueness form of
                        Just validatedForm ->
                            ( { model | formState = Saving validatedForm }
                            , Api.createTeam apiKey CreateTeamResponse accessToken validatedForm
                            )

                        Nothing ->
                            ( { model
                                | formState =
                                    form
                                        |> LeggTilTeamForm.visAlleFeilmeldinger
                                        |> Editing
                              }
                            , Effect.none
                            )

                _ ->
                    ( model, Effect.none )

        AvbrytTrykket ->
            ( model
            , Effect.pushRoute
                { path = Route.Path.MineTeam
                , query = Dict.empty
                , hash = Nothing
                }
            )

        CreateTeamResponse (Ok teamSummary) ->
            ( model
            , Effect.pushRoute
                { path = Route.Path.Team__Settings { team = TeamSummary.slugString teamSummary }
                , query = Dict.empty
                , hash = Nothing
                }
            )

        CreateTeamResponse (Err error) ->
            case model.formState of
                Saving form ->
                    ( { model | formState = Failure error form }, Effect.none )

                _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> View Msg
view model =
    { title = "Hvem har standup? | Legg til team"
    , body =
        Page.viewPageWrapper
            [ h1 [] [ text "Legg til team" ]
            , case model.formState of
                Editing form ->
                    viewForm
                        { isLoading = False
                        , slugIcon = slugStatusIcon form model.slugUniqueness
                        }
                        form

                Saving form ->
                    form
                        |> LeggTilTeamForm.fromValidated
                        |> viewForm
                            { isLoading = True
                            , slugIcon = Just TextInput.Checkmark
                            }

                Failure error validatedLeggTilTeamForm ->
                    text "failure"
            ]
    }


viewForm : { isLoading : Bool, slugIcon : Maybe TextInput.StatusIcon } -> LeggTilTeamForm -> Html Msg
viewForm { isLoading, slugIcon } form =
    Html.form
        [ onSubmit LagreTrykket
        , Attributes.css
            [ Css.displayFlex
            , Css.flexDirection Css.column
            , Css.property "gap" "16px"
            ]
        ]
        [ form
            |> LeggTilTeamForm.navn
            |> TextInput.input { label = "Teamnavn", msg = NavnOppdatert }
            |> TextInput.withErrorMessage (LeggTilTeamForm.feilmeldingNavn form)
            |> TextInput.onBlur NavnMistetFokus
            |> TextInput.toHtml
        , form
            |> LeggTilTeamForm.slug
            |> TextInput.input { label = "Slug", msg = SlugOppdatert }
            |> TextInput.withStatusIcon slugIcon
            |> TextInput.withDisabled isLoading
            |> TextInput.withErrorMessage (LeggTilTeamForm.feilmeldingSlug form)
            |> TextInput.onBlur SlugMistetFokus
            |> TextInput.toHtml
        , viewIndividualSetting { label = "URL", value = text ("https://hvemharstandup.no/" ++ LeggTilTeamForm.slug form) }
        , div [ Attributes.css [ Css.alignSelf Css.end, Css.displayFlex, Css.flexDirection Css.row, Css.property "gap" "12px" ] ]
            [ Button.button AvbrytTrykket "Avbryt"
                |> Button.withVariant Button.Secondary
                |> Button.withDisabled isLoading
                |> Button.toHtml
            , Button.submit "Lagre"
                |> Button.withLoadingSpinner isLoading
                |> Button.toHtml
            ]
        ]


slugStatusIcon : LeggTilTeamForm -> SlugUniqueness -> Maybe TextInput.StatusIcon
slugStatusIcon form slugUniqueness =
    if SlugUniqueness.isLoading slugUniqueness (LeggTilTeamForm.slug form) then
        Just TextInput.LoadingSpinner

    else if SlugUniqueness.isUnique slugUniqueness (LeggTilTeamForm.slug form) then
        Just TextInput.Checkmark

    else
        Just TextInput.Error


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
