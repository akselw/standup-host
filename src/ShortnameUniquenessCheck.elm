module ShortnameUniquenessCheck exposing (ShortnameUniquenessCheck, decoder, isUnique)

import Json.Decode exposing (Decoder)


type ShortnameUniquenessCheck
    = Unique
    | AlreadyTaken


isUnique : ShortnameUniquenessCheck -> Bool
isUnique check =
    case check of
        Unique ->
            True

        AlreadyTaken ->
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
