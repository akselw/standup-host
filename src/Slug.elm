module Slug exposing
    ( Error(..)
    , Slug
    , decoder
    , encode
    , fromString
    , toString
    )

import Json.Decode exposing (Decoder)
import Json.Encode


type Slug
    = Slug String


type Error
    = SlugInBlacklist
    | WrongFormat


fromString : String -> Result Error Slug
fromString string =
    if hasWrongFormat string then
        Err WrongFormat

    else if isInBlacklist string then
        Err SlugInBlacklist

    else
        Ok (Slug string)


hasWrongFormat : String -> Bool
hasWrongFormat string =
    let
        isCorrectFormat =
            String.all isLowerAlphaNumOrDash string
                && firstAndLastIsAlphaNum string
                && hasNoDoubleDashes string
    in
    not isCorrectFormat


firstAndLastIsAlphaNum : String -> Bool
firstAndLastIsAlphaNum string =
    let
        firstIsAlphaNum =
            string
                |> String.toList
                |> List.head
                |> Maybe.map Char.isAlphaNum
                |> Maybe.withDefault False

        lastIsAlphaNum =
            string
                |> String.toList
                |> List.reverse
                |> List.head
                |> Maybe.map Char.isAlphaNum
                |> Maybe.withDefault False
    in
    firstIsAlphaNum && lastIsAlphaNum


isLowerAlphaNumOrDash : Char -> Bool
isLowerAlphaNumOrDash char =
    char
        == '-'
        || Char.isDigit char
        || (Char.isLower char && Char.isAlpha char)


hasNoDoubleDashes : String -> Bool
hasNoDoubleDashes string =
    string
        |> String.contains "--"
        |> not


isInBlacklist : String -> Bool
isInBlacklist string =
    List.member string
        [ "admin"
        , "settings"
        , "mine-team"
        , "login"
        , "oauth"
        , "team"
        , "legg-til"
        ]



--- TO STRING ---


toString : Slug -> String
toString (Slug slug) =
    slug



--- ENCODE ---


encode : Slug -> Json.Encode.Value
encode (Slug slug) =
    Json.Encode.string slug



--- DECODER ---


decoder : Decoder Slug
decoder =
    Json.Decode.string
        |> Json.Decode.map Slug
