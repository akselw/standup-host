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
    True


slugInBlacklist : String -> Bool
slugInBlacklist string =
    True
