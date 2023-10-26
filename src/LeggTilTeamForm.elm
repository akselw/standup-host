module LeggTilTeamForm exposing
    ( LeggTilTeamForm
    , ValidatedLeggTilTeamForm
    , encode
    , feilmeldingNavn
    , feilmeldingSlug
    , fromValidated
    , init
    , navn
    , oppdaterNavn
    , oppdaterSlug
    , slug
    , validate
    , visAlleFeilmeldinger
    , visFeilmeldingNavn
    , visFeilmeldingSlug
    )

import Json.Encode
import Slug exposing (Slug)
import SlugUniqueness exposing (SlugUniqueness)
import UserId exposing (UserId)


type LeggTilTeamForm
    = Form
        { navn : String
        , slug : String
        , visFeilmeldingNavn : Bool
        , visFeilmeldingSlug : Bool
        }


init : LeggTilTeamForm
init =
    Form
        { navn = ""
        , slug = ""
        , visFeilmeldingNavn = False
        , visFeilmeldingSlug = False
        }



--- Innhold ---


navn : LeggTilTeamForm -> String
navn (Form form) =
    form.navn


slug : LeggTilTeamForm -> String
slug (Form form) =
    form.slug



--- Oppdatering ---


oppdaterNavn : String -> LeggTilTeamForm -> LeggTilTeamForm
oppdaterNavn navn_ (Form form) =
    Form { form | navn = navn_ }


oppdaterSlug : String -> LeggTilTeamForm -> LeggTilTeamForm
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


feilmeldingNavn : LeggTilTeamForm -> Maybe String
feilmeldingNavn (Form form) =
    if form.visFeilmeldingNavn && not (navnErGyldig form.navn) then
        Just "Teamnavn kan ikke være tomt"

    else
        Nothing


feilmeldingSlug : LeggTilTeamForm -> Maybe String
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


visFeilmeldingNavn : LeggTilTeamForm -> LeggTilTeamForm
visFeilmeldingNavn (Form form) =
    Form { form | visFeilmeldingNavn = True }


visFeilmeldingSlug : LeggTilTeamForm -> LeggTilTeamForm
visFeilmeldingSlug (Form form) =
    Form { form | visFeilmeldingSlug = True }


visAlleFeilmeldinger : LeggTilTeamForm -> LeggTilTeamForm
visAlleFeilmeldinger form =
    form
        |> visFeilmeldingNavn
        |> visFeilmeldingSlug



--- Validering ---


type ValidatedLeggTilTeamForm
    = ValidatedForm
        { navn : String
        , slug : Slug
        }


validate : SlugUniqueness -> LeggTilTeamForm -> Maybe ValidatedLeggTilTeamForm
validate slugUniqueness (Form form) =
    if navnErGyldig form.navn && SlugUniqueness.isUnique slugUniqueness form.slug then
        form.slug
            |> Slug.fromString
            |> Result.toMaybe
            |> Maybe.map
                (\slug_ ->
                    ValidatedForm
                        { navn = form.navn
                        , slug = slug_
                        }
                )

    else
        Nothing


fromValidated : ValidatedLeggTilTeamForm -> LeggTilTeamForm
fromValidated (ValidatedForm form) =
    Form
        { navn = form.navn
        , slug = Slug.toString form.slug
        , visFeilmeldingNavn = False
        , visFeilmeldingSlug = False
        }



--- Encoding ---


encode : ValidatedLeggTilTeamForm -> UserId -> Json.Encode.Value
encode (ValidatedForm form) userId =
    Json.Encode.object
        [ ( "name", Json.Encode.string form.navn )
        , ( "slug", Slug.encode form.slug )
        , ( "owner_id", UserId.encode userId )
        ]
