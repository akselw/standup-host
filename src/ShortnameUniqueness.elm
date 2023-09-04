module ShortnameUniqueness exposing (ShortnameUniqueness, ShortnameUniquenessCheck, decoder, init, insert, isLoading, isUnique)

import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder)


type ShortnameUniqueness
    = ShortnameUniqueness (Dict String (Result Http.Error ShortnameUniquenessCheck))


type ShortnameUniquenessCheck
    = Unique
    | AlreadyTaken


init : ShortnameUniqueness
init =
    ShortnameUniqueness Dict.empty


insert : String -> Result Http.Error ShortnameUniquenessCheck -> ShortnameUniqueness -> ShortnameUniqueness
insert shortname result (ShortnameUniqueness dict) =
    dict
        |> Dict.insert shortname result
        |> ShortnameUniqueness


isUnique : String -> ShortnameUniqueness -> Bool
isUnique shortname (ShortnameUniqueness dict) =
    case Dict.get shortname dict of
        Just (Ok Unique) ->
            True

        _ ->
            False


isLoading : String -> ShortnameUniqueness -> Bool
isLoading shortname (ShortnameUniqueness dict) =
    case Dict.get shortname dict of
        Nothing ->
            True

        _ ->
            False


decoder : Decoder ShortnameUniquenessCheck
decoder =
    Json.Decode.list (Json.Decode.field "shortname" Json.Decode.string)
        |> Json.Decode.map
            (\list ->
                if List.length list == 0 then
                    Unique

                else
                    AlreadyTaken
            )
