module TeamSettingsForm exposing
    ( TeamSettingsForm
    , ValidatedTeamSettingsForm
    , encode
    , feilmeldingNavn
    , feilmeldingShortname
    , fromValidated
    , init
    , navn
    , oppdaterNavn
    , oppdaterShortname
    , shortname
    , teamId
    , validate
    , visAlleFeilmeldinger
    , visFeilmeldingNavn
    , visFeilmeldingShortname
    )

import Json.Encode
import Team exposing (Team)
import TeamId exposing (TeamId)


type TeamSettingsForm
    = Form
        { navn : String
        , shortname : String
        , visFeilmeldingNavn : Bool
        , visFeilmeldingShortname : Bool
        , teamId : TeamId
        }


init : Team -> TeamSettingsForm
init team =
    Form
        { navn = Team.navn team
        , shortname = Team.shortname team
        , visFeilmeldingNavn = False
        , visFeilmeldingShortname = False
        , teamId = Team.id team
        }



--- Innhold ---


navn : TeamSettingsForm -> String
navn (Form form) =
    form.navn


shortname : TeamSettingsForm -> String
shortname (Form form) =
    form.shortname



--- Oppdatering ---


oppdaterNavn : String -> TeamSettingsForm -> TeamSettingsForm
oppdaterNavn navn_ (Form form) =
    Form { form | navn = navn_ }


oppdaterShortname : String -> TeamSettingsForm -> TeamSettingsForm
oppdaterShortname shortname_ (Form form) =
    Form { form | shortname = shortname_ }



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


shortnameErGyldig : String -> Bool
shortnameErGyldig string =
    ikkeTomStreng string
        && String.all (\char -> Char.isAlphaNum char || char == '-') string
        && (String.length string >= 3)


feilmeldingNavn : TeamSettingsForm -> Maybe String
feilmeldingNavn (Form form) =
    if form.visFeilmeldingNavn && not (navnErGyldig form.navn) then
        Just "Teamnavn kan ikke være tomt"

    else
        Nothing


feilmeldingShortname : TeamSettingsForm -> Maybe String
feilmeldingShortname (Form form) =
    if form.visFeilmeldingShortname && not (shortnameErGyldig form.shortname) then
        Just "Shortname må kun inneholde bokstaver, tall og bindestreker, og kan ikke være tomt"
        -- TODO: Endre feilmelding basert på type feil

    else
        Nothing


visFeilmeldingNavn : TeamSettingsForm -> TeamSettingsForm
visFeilmeldingNavn (Form form) =
    Form { form | visFeilmeldingNavn = True }


visFeilmeldingShortname : TeamSettingsForm -> TeamSettingsForm
visFeilmeldingShortname (Form form) =
    Form { form | visFeilmeldingShortname = True }


visAlleFeilmeldinger : TeamSettingsForm -> TeamSettingsForm
visAlleFeilmeldinger form =
    form
        |> visFeilmeldingNavn
        |> visFeilmeldingShortname



--- TODO: Lag en ShortnameUniquenessCheck type, som decodes fra en list med {shortname=...}, der tom liste gir unique
--- Validering ---


type ValidatedTeamSettingsForm
    = ValidatedForm
        { navn : String
        , shortname : String
        , teamId : TeamId
        }


validate : TeamSettingsForm -> Maybe ValidatedTeamSettingsForm
validate (Form form) =
    if navnErGyldig form.navn && shortnameErGyldig form.shortname then
        Just
            (ValidatedForm
                { navn = form.navn
                , shortname = form.shortname
                , teamId = form.teamId
                }
            )

    else
        Nothing


teamId : ValidatedTeamSettingsForm -> String
teamId (ValidatedForm form) =
    TeamId.toString form.teamId


fromValidated : ValidatedTeamSettingsForm -> TeamSettingsForm
fromValidated (ValidatedForm form) =
    Form
        { navn = form.navn
        , shortname = form.shortname
        , visFeilmeldingNavn = False
        , visFeilmeldingShortname = False
        , teamId = form.teamId
        }



--- Encoding ---


encode : ValidatedTeamSettingsForm -> Json.Encode.Value
encode (ValidatedForm form) =
    Json.Encode.object
        [ ( "name", Json.Encode.string form.navn )
        , ( "shortname", Json.Encode.string form.shortname )
        ]
