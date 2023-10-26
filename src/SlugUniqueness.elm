module SlugUniqueness exposing
    ( SlugUniqueness
    , SlugUniquenessCheck
    , decoder
    , init
    , insert
    , isLoading
    , isUnique
    )

import Dict exposing (Dict)
import Http
import Json.Decode exposing (Decoder)


type SlugUniqueness
    = SlugUniqueness (Dict String (Result Http.Error SlugUniquenessCheck))


type SlugUniquenessCheck
    = Unique
    | AlreadyTaken


init : SlugUniqueness
init =
    SlugUniqueness Dict.empty


insert : String -> Result Http.Error SlugUniquenessCheck -> SlugUniqueness -> SlugUniqueness
insert slug result (SlugUniqueness dict) =
    dict
        |> Dict.insert slug result
        |> SlugUniqueness


isUnique : SlugUniqueness -> String -> Bool
isUnique (SlugUniqueness dict) slug =
    case Dict.get slug dict of
        Just (Ok Unique) ->
            True

        _ ->
            False


isLoading : SlugUniqueness -> String -> Bool
isLoading (SlugUniqueness dict) slug =
    case Dict.get slug dict of
        Nothing ->
            True

        _ ->
            False


decoder : Decoder SlugUniquenessCheck
decoder =
    Json.Decode.list (Json.Decode.field "slug" Json.Decode.string)
        |> Json.Decode.map
            (\list ->
                if List.length list == 0 then
                    Unique

                else
                    AlreadyTaken
            )
