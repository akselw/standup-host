module Slug exposing
    ( Error(..)
    , Slug
    , fromString
    )


type Slug
    = Slug String


type Error
    = SlugInBlacklist
    | WrongFormat


fromString : String -> Result Error Slug
fromString string =
    if wrongFormat string then
        Err WrongFormat

    else if slugInBlacklist string then
        Err SlugInBlacklist

    else
        Ok (Slug string)


wrongFormat : String -> Bool
wrongFormat string =
    let
        isCorrectFormat =
            String.all isLowerAlphaNumOrDash string
                && firstAndLastIsAlphaNum string
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


slugInBlacklist : String -> Bool
slugInBlacklist string =
    False
