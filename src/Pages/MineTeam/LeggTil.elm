module Pages.MineTeam.LeggTil exposing (Model, Msg, page)

import Css
import Effect exposing (Effect)
import Html.Styled as Html exposing (..)
import Html.Styled.Attributes as Attributes
import Html.Styled.Events exposing (onSubmit)
import Http
import Layouts
import LeggTilTeamForm exposing (LeggTilTeamForm, ValidatedLeggTilTeamForm)
import Page exposing (Page)
import Route exposing (Route)
import Shared
import ShortnameUniqueness exposing (ShortnameUniqueness, ShortnameUniquenessCheck)
import TeamSummary exposing (TeamSummary)
import View exposing (View)
import View.Button as Button
import View.Page as Page
import View.TextInput as TextInput


page : Shared.Model -> Route () -> Page Model Msg
page shared route =
    Page.new
        { init = init
        , update = update
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
    , shortnameUniqueness : ShortnameUniqueness
    }


type FormState
    = Editing LeggTilTeamForm
    | Saving ValidatedLeggTilTeamForm
    | Failure Http.Error ValidatedLeggTilTeamForm


init : () -> ( Model, Effect Msg )
init () =
    ( { formState = Editing LeggTilTeamForm.init
      , shortnameUniqueness = ShortnameUniqueness.init
      }
    , Effect.none
    )



-- UPDATE


type Msg
    = NavnOppdatert String
    | NavnMistetFokus
    | ShortnameOppdatert String
    | ShortnameMistetFokus
    | ShortnameUniquenessResponse String (Result Http.Error ShortnameUniquenessCheck)
    | LagreTrykket
    | AvbrytTrykket
    | CreateTeamResponse (Result Http.Error TeamSummary)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        NavnOppdatert string ->
            case model.formState of
                Editing form ->
                    ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        NavnMistetFokus ->
            case model.formState of
                Editing form ->
                    ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        ShortnameOppdatert string ->
            case model.formState of
                Editing form ->
                    ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        ShortnameMistetFokus ->
            case model.formState of
                Editing form ->
                    ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        ShortnameUniquenessResponse string result ->
            case model.formState of
                Editing form ->
                    ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        LagreTrykket ->
            case model.formState of
                Editing form ->
                    ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        AvbrytTrykket ->
            case model.formState of
                Editing form ->
                    ( model, Effect.none )

                _ ->
                    ( model, Effect.none )

        CreateTeamResponse result ->
            case model.formState of
                Editing form ->
                    ( model, Effect.none )

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
                        , shortnameIcon = shortnameStatusIcon form model.shortnameUniqueness
                        }
                        form

                Saving form ->
                    form
                        |> LeggTilTeamForm.fromValidated
                        |> viewForm
                            { isLoading = True
                            , shortnameIcon = Just TextInput.Checkmark
                            }

                Failure error validatedLeggTilTeamForm ->
                    text "failure"
            ]
    }


viewForm : { isLoading : Bool, shortnameIcon : Maybe TextInput.StatusIcon } -> LeggTilTeamForm -> Html Msg
viewForm { isLoading, shortnameIcon } form =
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
            |> LeggTilTeamForm.shortname
            |> TextInput.input { label = "Shortname", msg = ShortnameOppdatert }
            |> TextInput.withStatusIcon shortnameIcon
            |> TextInput.withDisabled isLoading
            |> TextInput.withErrorMessage (LeggTilTeamForm.feilmeldingShortname form)
            |> TextInput.onBlur ShortnameMistetFokus
            |> TextInput.toHtml
        , viewIndividualSetting { label = "URL", value = text ("https://hvemharstandup.no/" ++ LeggTilTeamForm.shortname form) }
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


shortnameStatusIcon : LeggTilTeamForm -> ShortnameUniqueness -> Maybe TextInput.StatusIcon
shortnameStatusIcon form shortnameUniqueness =
    if ShortnameUniqueness.isLoading shortnameUniqueness (LeggTilTeamForm.shortname form) then
        Just TextInput.LoadingSpinner

    else if ShortnameUniqueness.isUnique shortnameUniqueness (LeggTilTeamForm.shortname form) then
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
