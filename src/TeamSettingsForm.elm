module TeamSettingsForm exposing
    ( TeamSettingsForm
    , ValidatedTeamSettingsForm
    , encode
    , feilmeldingNavn
    , feilmeldingSlug
    , fromValidated
    , init
    , navn
    , oppdaterNavn
    , oppdaterSlug
    , slug
    , teamId
    , validate
    , visAlleFeilmeldinger
    , visFeilmeldingNavn
    , visFeilmeldingSlug
    )

import Json.Encode
import Slug exposing (Slug)
import SlugUniqueness exposing (SlugUniqueness)
import Team exposing (Team)
import TeamId exposing (TeamId)
import UserId exposing (UserId)


type TeamSettingsForm
    = Form
        { navn : String
        , slug : String
        , visFeilmeldingNavn : Bool
        , visFeilmeldingSlug : Bool
        , team : Team
        }


init : Team -> TeamSettingsForm
init team =
    Form
        { navn = Team.navn team
        , slug = Team.slugString team
        , visFeilmeldingNavn = False
        , visFeilmeldingSlug = False
        , team = team
        }



--- Innhold ---


navn : TeamSettingsForm -> String
navn (Form form) =
    form.navn


slug : TeamSettingsForm -> String
slug (Form form) =
    form.slug



--- Oppdatering ---


oppdaterNavn : String -> TeamSettingsForm -> TeamSettingsForm
oppdaterNavn navn_ (Form form) =
    Form { form | navn = navn_ }


oppdaterSlug : String -> TeamSettingsForm -> TeamSettingsForm
oppdaterSlug slug_ (Form form) =
    Form { form | slug = slug_ }



--- Feilmeldinger ---


ikkeTomStreng : String -> Bool
ikkeTomStreng string =
    string
        |> String.trim
        |> String.isEmpty
        |> not


navnErGyldig : String -> Bool
navnErGyldig string =
    ikkeTomStreng string


feilmeldingNavn : TeamSettingsForm -> Maybe String
feilmeldingNavn (Form form) =
    if form.visFeilmeldingNavn && not (navnErGyldig form.navn) then
        Just "Teamnavn kan ikke være tomt"

    else
        Nothing


feilmeldingSlug : TeamSettingsForm -> Maybe String
feilmeldingSlug (Form form) =
    if form.visFeilmeldingSlug then
        case Slug.fromString form.slug of
            Err Slug.WrongFormat ->
                Just "Slug må kun inneholde bokstaver, tall og bindestreker, og kan ikke være tomt"

            Err Slug.SlugInBlacklist ->
                Just "Ikke gyldig slug"

            Ok _ ->
                Nothing

    else
        Nothing


visFeilmeldingNavn : TeamSettingsForm -> TeamSettingsForm
visFeilmeldingNavn (Form form) =
    Form { form | visFeilmeldingNavn = True }


visFeilmeldingSlug : TeamSettingsForm -> TeamSettingsForm
visFeilmeldingSlug (Form form) =
    Form { form | visFeilmeldingSlug = True }


visAlleFeilmeldinger : TeamSettingsForm -> TeamSettingsForm
visAlleFeilmeldinger form =
    form
        |> visFeilmeldingNavn
        |> visFeilmeldingSlug



--- Validering ---


type ValidatedTeamSettingsForm
    = ValidatedForm
        { navn : String
        , slug : Slug
        , team : Team
        }


validate : SlugUniqueness -> TeamSettingsForm -> Maybe ValidatedTeamSettingsForm
validate slugUniqueness (Form form) =
    if navnErGyldig form.navn && slugErUnique slugUniqueness form then
        form.slug
            |> Slug.fromString
            |> Result.toMaybe
            |> Maybe.map
                (\slug_ ->
                    ValidatedForm
                        { navn = form.navn
                        , slug = slug_
                        , team = form.team
                        }
                )

    else
        Nothing


slugErUnique : SlugUniqueness -> { r | slug : String, team : Team } -> Bool
slugErUnique slugUniqueness form =
    Team.slugString form.team == form.slug || SlugUniqueness.isUnique slugUniqueness form.slug


teamId : ValidatedTeamSettingsForm -> String
teamId (ValidatedForm form) =
    form.team
        |> Team.id
        |> TeamId.toString


fromValidated : ValidatedTeamSettingsForm -> TeamSettingsForm
fromValidated (ValidatedForm form) =
    Form
        { navn = form.navn
        , slug = Slug.toString form.slug
        , visFeilmeldingNavn = False
        , visFeilmeldingSlug = False
        , team = form.team
        }



--- Encoding ---


encode : ValidatedTeamSettingsForm -> Json.Encode.Value
encode (ValidatedForm form) =
    Json.Encode.object
        [ ( "name", Json.Encode.string form.navn )
        , ( "slug", Slug.encode form.slug )
        ]
