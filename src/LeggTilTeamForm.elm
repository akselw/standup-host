module LeggTilTeamForm exposing (LeggTilTeamForm, ValidatedLeggTilTeamForm, encode, feilmeldingNavn, feilmeldingShortname, fromValidated, init, navn, oppdaterNavn, oppdaterShortname, shortname, validate, visAlleFeilmeldinger, visFeilmeldingNavn, visFeilmeldingShortname)

import Json.Encode
import ShortnameUniqueness exposing (ShortnameUniqueness)
import UserId exposing (UserId)


type LeggTilTeamForm
    = Form
        { navn : String
        , shortname : String
        , visFeilmeldingNavn : Bool
        , visFeilmeldingShortname : Bool
        }


init : LeggTilTeamForm
init =
    Form
        { navn = ""
        , shortname = ""
        , visFeilmeldingNavn = False
        , visFeilmeldingShortname = False
        }



--- Innhold ---


navn : LeggTilTeamForm -> String
navn (Form form) =
    form.navn


shortname : LeggTilTeamForm -> String
shortname (Form form) =
    form.shortname



--- Oppdatering ---


oppdaterNavn : String -> LeggTilTeamForm -> LeggTilTeamForm
oppdaterNavn navn_ (Form form) =
    Form { form | navn = navn_ }


oppdaterShortname : String -> LeggTilTeamForm -> LeggTilTeamForm
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


feilmeldingNavn : LeggTilTeamForm -> Maybe String
feilmeldingNavn (Form form) =
    if form.visFeilmeldingNavn && not (navnErGyldig form.navn) then
        Just "Teamnavn kan ikke være tomt"

    else
        Nothing


feilmeldingShortname : LeggTilTeamForm -> Maybe String
feilmeldingShortname (Form form) =
    if form.visFeilmeldingShortname && not (shortnameErGyldig form.shortname) then
        Just "Shortname må kun inneholde bokstaver, tall og bindestreker, og kan ikke være tomt"
        -- TODO: Endre feilmelding basert på type feil

    else
        Nothing


visFeilmeldingNavn : LeggTilTeamForm -> LeggTilTeamForm
visFeilmeldingNavn (Form form) =
    Form { form | visFeilmeldingNavn = True }


visFeilmeldingShortname : LeggTilTeamForm -> LeggTilTeamForm
visFeilmeldingShortname (Form form) =
    Form { form | visFeilmeldingShortname = True }


visAlleFeilmeldinger : LeggTilTeamForm -> LeggTilTeamForm
visAlleFeilmeldinger form =
    form
        |> visFeilmeldingNavn
        |> visFeilmeldingShortname



--- Validering ---


type ValidatedLeggTilTeamForm
    = ValidatedForm
        { navn : String
        , shortname : String
        }


validate : ShortnameUniqueness -> LeggTilTeamForm -> Maybe ValidatedLeggTilTeamForm
validate shortnameUniqueness (Form form) =
    if navnErGyldig form.navn && shortnameErGyldig form.shortname && ShortnameUniqueness.isUnique shortnameUniqueness form.shortname then
        Just
            (ValidatedForm
                { navn = form.navn
                , shortname = form.shortname
                }
            )

    else
        Nothing


fromValidated : ValidatedLeggTilTeamForm -> LeggTilTeamForm
fromValidated (ValidatedForm form) =
    Form
        { navn = form.navn
        , shortname = form.shortname
        , visFeilmeldingNavn = False
        , visFeilmeldingShortname = False
        }



--- Encoding ---


encode : ValidatedLeggTilTeamForm -> UserId -> Json.Encode.Value
encode (ValidatedForm form) userId =
    Json.Encode.object
        [ ( "name", Json.Encode.string form.navn )
        , ( "shortname", Json.Encode.string form.shortname )
        , ( "owner_id", UserId.encode userId )
        ]
